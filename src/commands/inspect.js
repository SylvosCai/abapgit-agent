/**
 * Inspect command - Syntax check for ABAP files
 */

const pathModule = require('path');
const fs = require('fs');
const { printHttpError } = require('../utils/format-error');

/**
 * Escape a string for safe embedding in XML text/attribute content
 */
function escapeXml(str) {
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&apos;');
}

/**
 * Build JUnit XML from inspect results array.
 *
 * Maps to JUnit schema:
 *   <testsuites>
 *     <testsuite name="CLAS ZCL_MY_CLASS" tests="N" failures="F" errors="0">
 *       <testcase name="Inspect / Syntax check" classname="ZCL_MY_CLASS"/>   ← clean
 *       <testcase name="Inspect / CL_CI_TEST_EXCEPTION/UNHANDLED_01"         ← finding
 *                classname="ZCL_MY_CLASS">
 *         <failure type="SyntaxError" message="...">method/line/detail</failure>
 *       </testcase>
 *     </testsuite>
 *   </testsuites>
 *
 * One testsuite per object. Each error/warning becomes its own testcase named
 * after the SCI check rule (check_class/check_code) so Jenkins shows exactly
 * which rule fired. Clean objects get a single passing testcase.
 */
function buildInspectJUnit(results) {
  const suites = results.map(res => {
    const objectType = res.OBJECT_TYPE !== undefined ? res.OBJECT_TYPE : (res.object_type || 'UNKNOWN');
    const objectName = res.OBJECT_NAME !== undefined ? res.OBJECT_NAME : (res.object_name || 'UNKNOWN');
    const errors     = res.ERRORS   !== undefined ? res.ERRORS   : (res.errors   || []);
    const warnings   = res.WARNINGS !== undefined ? res.WARNINGS : (res.warnings || []);
    const errorCount = errors.length;
    const warnCount  = warnings.length;
    const testCount  = Math.max(1, errorCount + warnCount);

    const testcases = [];

    const inspectClass = `${escapeXml(objectName)}.Inspect`;

    if (errorCount === 0 && warnCount === 0) {
      testcases.push(`    <testcase name="Syntax check" classname="${inspectClass}"/>`);
    }

    for (const err of errors) {
      const line       = err.LINE        || err.line        || '?';
      const column     = err.COLUMN      || err.column      || '?';
      const text       = err.TEXT        || err.text        || 'Unknown error';
      const methodName = err.METHOD_NAME || err.method_name;
      const sobjname   = err.SOBJNAME    || err.sobjname    || '';
      const checkClass = err.CHECK_CLASS || err.check_class || '';
      const checkCode  = err.CHECK_CODE  || err.check_code  || '';
      const detail     = [
        methodName ? `Method: ${methodName}` : null,
        `Line ${line}, Column ${column}`,
        sobjname ? `Include: ${sobjname}` : null,
        text
      ].filter(Boolean).join('\n');
      const checkId  = checkClass && checkCode ? `${checkClass}/${checkCode}` : null;
      const caseName = checkId ? checkId : (methodName ? `${methodName} line ${line}` : `Line ${line}`);
      testcases.push(
        `    <testcase name="${escapeXml(caseName)}" classname="${inspectClass}">\n` +
        `      <failure type="SyntaxError" message="${escapeXml(text)}">${escapeXml(detail)}</failure>\n` +
        `    </testcase>`
      );
    }

    for (const warn of warnings) {
      const line       = warn.LINE        || warn.line        || '?';
      const text       = warn.MESSAGE     || warn.message     || warn.TEXT || warn.text || 'Warning';
      const methodName = warn.METHOD_NAME || warn.method_name;
      const sobjname   = warn.SOBJNAME    || warn.sobjname    || '';
      const checkClass = warn.CHECK_CLASS || warn.check_class || '';
      const checkCode  = warn.CHECK_CODE  || warn.check_code  || '';
      const detail     = [
        methodName ? `Method: ${methodName}` : null,
        `Line ${line}`,
        sobjname ? `Include: ${sobjname}` : null,
        text
      ].filter(Boolean).join('\n');
      const checkId  = checkClass && checkCode ? `${checkClass}/${checkCode}` : null;
      const caseName = checkId ? checkId : (methodName ? `${methodName} line ${line}` : `Line ${line}`);
      testcases.push(
        `    <testcase name="${escapeXml(caseName)}" classname="${inspectClass}">\n` +
        `      <failure type="Warning" message="${escapeXml(text)}">${escapeXml(detail)}</failure>\n` +
        `    </testcase>`
      );
    }

    return (
      `  <testsuite name="${escapeXml(objectType + ' ' + objectName)}" ` +
      `tests="${testCount}" failures="${errorCount + warnCount}" errors="0">\n` +
      testcases.join('\n') + '\n' +
      `  </testsuite>`
    );
  });

  return (
    '<?xml version="1.0" encoding="UTF-8"?>\n' +
    '<testsuites>\n' +
    suites.join('\n') + '\n' +
    '</testsuites>\n'
  );
}

/**
 * Inspect a single batch of files in one request (max CHUNK_SIZE files)
 */
const CHUNK_SIZE = 10;

async function inspectAllFiles(files, csrfToken, config, variant, http, verbose = false) {
  // Convert files to uppercase names
  const fileNames = files.map(f => {
    const baseName = pathModule.basename(f).toUpperCase();
    return baseName;
  });

  try {
    // Send all files in one request
    const data = {
      files: fileNames
    };

    // Add variant if specified
    if (variant) {
      data.variant = variant;
    }

    const result = await http.post('/sap/bc/z_abapgit_agent/inspect', data, { csrfToken });

    // Handle both table result and old single result
    let results = [];
    if (Array.isArray(result)) {
      results = result;
    } else {
      // Convert single result to array format
      results = [{
        OBJECT_TYPE: 'UNKNOWN',
        OBJECT_NAME: files.join(', '),
        SUCCESS: result.SUCCESS !== undefined ? result.SUCCESS === 'X' || result.SUCCESS === true : result.success,
        ERROR_COUNT: result.ERROR_COUNT || result.error_count || 0,
        ERRORS: result.ERRORS || result.errors || [],
        WARNINGS: result.warnings || []
      }];
    }

    return results;
  } catch (error) {
    printHttpError(error, { verbose });
    process.exit(1);
  }
}

/**
 * Apply inspect.suppress rules to a single result — downgrade matching errors/warnings to infos.
 */
function applySuppressRules(result, suppressRules) {
  if (!suppressRules || suppressRules.length === 0) return;
  const objName = (result.OBJECT_NAME || result.object_name || '').toUpperCase();
  for (const rule of suppressRules) {
    const objPattern = new RegExp('^' + (rule.object || '*').toUpperCase().replace(/\*/g, '.*') + '$');
    if (!objPattern.test(objName)) continue;
    const msgPattern = new RegExp((rule.message || '*').replace(/\*/g, '.*'), 'i');

    const errors = result.ERRORS || result.errors || [];
    const kept = [];
    for (const err of errors) {
      const text = err.TEXT || err.text || '';
      if (msgPattern.test(text)) {
        const infos = result.INFOS || result.infos || [];
        infos.push({ ...err, MESSAGE: `[suppressed] ${text}`, message: `[suppressed] ${text}` });
        if (result.INFOS !== undefined) result.INFOS = infos; else result.infos = infos;
        const ec = result.ERROR_COUNT !== undefined ? 'ERROR_COUNT' : 'error_count';
        result[ec] = Math.max(0, (result[ec] || 0) - 1);
      } else {
        kept.push(err);
      }
    }
    if (result.ERRORS !== undefined) result.ERRORS = kept; else result.errors = kept;

    const warnings = result.WARNINGS || result.warnings || [];
    const keptW = [];
    for (const warn of warnings) {
      const text = warn.MESSAGE || warn.message || '';
      if (msgPattern.test(text)) {
        const infos = result.INFOS || result.infos || [];
        infos.push({ ...warn, MESSAGE: `[suppressed] ${text}`, message: `[suppressed] ${text}` });
        if (result.INFOS !== undefined) result.INFOS = infos; else result.infos = infos;
      } else {
        keptW.push(warn);
      }
    }
    if (result.WARNINGS !== undefined) result.WARNINGS = keptW; else result.warnings = keptW;
  }
}

/**
 * Split files into chunks and inspect each sequentially, combining results.
 * Calls onChunkResult(result) for each object result as chunks complete.
 */
async function inspectInChunks(files, csrfToken, config, variant, http, verbose = false, onChunkResult = null) {
  const results = [];
  for (let i = 0; i < files.length; i += CHUNK_SIZE) {
    const chunk = files.slice(i, i + CHUNK_SIZE);
    const chunkResults = await inspectAllFiles(chunk, csrfToken, config, variant, http, verbose);
    for (const result of chunkResults) {
      results.push(result);
      if (onChunkResult) onChunkResult(result);
    }
  }
  return results;
}

/**
 * Process a single inspect result
 */
async function processInspectResult(res) {
  // Handle both uppercase and lowercase keys
  const success = res.SUCCESS !== undefined ? res.SUCCESS : res.success;
  const objectType = res.OBJECT_TYPE !== undefined ? res.OBJECT_TYPE : res.object_type;
  const objectName = res.OBJECT_NAME !== undefined ? res.OBJECT_NAME : res.object_name;
  const errorCount = res.ERROR_COUNT !== undefined ? res.ERROR_COUNT : (res.error_count || 0);
  const errors = res.ERRORS !== undefined ? res.ERRORS : (res.errors || []);
  const warnings = res.WARNINGS !== undefined ? res.WARNINGS : (res.warnings || []);
  const infos = res.INFOS !== undefined ? res.INFOS : (res.infos || []);

  if (errorCount > 0 || warnings.length > 0 || infos.length > 0) {
    if (errorCount > 0) {
      console.log(`❌ ${objectType} ${objectName} - Syntax check failed (${errorCount} error(s)):`);
    } else {
      const total = warnings.length + infos.length;
      console.log(`⚠️  ${objectType} ${objectName} - Syntax check passed with warnings (${total}):`);
    }
    console.log('\nErrors:');
    console.log('─'.repeat(60));

    for (const err of errors) {
      const line = err.LINE || err.line || '?';
      const column = err.COLUMN || err.column || '?';
      const text = err.TEXT || err.text || 'Unknown error';
      const methodName = err.METHOD_NAME || err.method_name;
      const sobjname = err.SOBJNAME || err.sobjname;
      const checkClass = err.CHECK_CLASS || err.check_class || '';
      const checkCode  = err.CHECK_CODE  || err.check_code  || '';

      if (methodName) {
        console.log(`  Method: ${methodName}`);
      }
      console.log(`  Line ${line}, Column ${column}:`);
      if (sobjname && sobjname.includes('====')) {
        console.log(`    Include: ${sobjname}`);
      }
      console.log(`    ${text}`);
      if (checkCode) {
        console.log(`    [${checkClass}/${checkCode}]`);
        console.log(`    → abapgit-agent inspect --doc ${checkClass}/${checkCode}`);
      }
      console.log('');
    }

    // Show warnings if any
    if (warnings.length > 0) {
      console.log('Warnings:');
      console.log('─'.repeat(60));
      for (const warn of warnings) {
        const line = warn.LINE || warn.line || '?';
        const text = warn.MESSAGE || warn.message || 'Unknown warning';
        const methodName = warn.METHOD_NAME || warn.method_name;
        const sobjname = warn.SOBJNAME || warn.sobjname;
        const checkClass = warn.CHECK_CLASS || warn.check_class || '';
        const checkCode  = warn.CHECK_CODE  || warn.check_code  || '';

        if (methodName) {
          console.log(`  Method: ${methodName}`);
        }
        console.log(`  Line ${line}:`);
        if (sobjname && sobjname.includes('====')) {
          console.log(`    Include: ${sobjname}`);
        }
        console.log(`    ${text}`);
        if (checkCode) {
          console.log(`    [${checkClass}/${checkCode}]`);
          console.log(`    → abapgit-agent inspect --doc ${checkClass}/${checkCode}`);
        }
      }
    }

    // Show infos if any
    if (infos.length > 0) {
      console.log('Info:');
      console.log('─'.repeat(60));
      for (const info of infos) {
        const line = info.LINE || info.line || '?';
        const text = info.MESSAGE || info.message || 'Unknown info';
        const methodName = info.METHOD_NAME || info.method_name;
        const sobjname = info.SOBJNAME || info.sobjname;
        const checkClass = info.CHECK_CLASS || info.check_class || '';
        const checkCode  = info.CHECK_CODE  || info.check_code  || '';

        if (methodName) {
          console.log(`  Method: ${methodName}`);
        }
        console.log(`  Line ${line}:`);
        if (sobjname && sobjname.includes('====')) {
          console.log(`    Include: ${sobjname}`);
        }
        console.log(`    ${text}`);
        if (checkCode) {
          console.log(`    [${checkClass}/${checkCode}]`);
          console.log(`    → abapgit-agent inspect --doc ${checkClass}/${checkCode}`);
        }
      }
    }
  } else if (success === true || success === 'X') {
    console.log(`✅ ${objectType} ${objectName} - Syntax check passed`);
  } else {
    console.log(`⚠️  ${objectType} ${objectName} - Syntax check returned unexpected status`);
  }
}

module.exports = {
  name: 'inspect',
  description: 'Inspect ABAP source files for syntax issues',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, AbapHttp, getInspectConfig } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent inspect --files <file1>,<file2>,... [--variant <check-variant>] [--junit-output <file>] [--json]
  abapgit-agent inspect --doc <check_class>/<check_code>

Description:
  Run SAP Code Inspector checks on activated ABAP objects. Requires the objects
  to be already active in the ABAP system (run pull first).

  Use --doc to fetch the SAP documentation for a specific check finding.
  The check class and code are shown in brackets after each finding, e.g.:
    [CL_CI_TEST_OMIT_BRACKETS/OMIT_01]

Parameters:
  --files <file1,...>       Comma-separated ABAP source files (required for inspection).
  --variant <variant>       Code Inspector variant (default: system default).
  --junit-output <file>     Write results as JUnit XML to this file.
  --json                    Output as JSON.
  --doc <class>/<code>      Fetch documentation for a check finding.

Examples:
  abapgit-agent inspect --files src/zcl_my_class.clas.abap
  abapgit-agent inspect --files src/zcl_my_class.clas.abap --variant ALL_CHECKS
  abapgit-agent inspect --files src/zcl_my_class.clas.abap --junit-output reports/inspect.xml
  abapgit-agent inspect --doc CL_CI_TEST_OMIT_BRACKETS/OMIT_01
`);
      return;
    }

    // --doc mode: fetch documentation for a check finding
    const docArgIndex = args.indexOf('--doc');
    if (docArgIndex !== -1) {
      const docArg = args[docArgIndex + 1];
      if (!docArg || !docArg.includes('/')) {
        console.error('Error: --doc requires <check_class>/<check_code>');
        console.error('Example: abapgit-agent inspect --doc CL_CI_TEST_OMIT_BRACKETS/OMIT_01');
        process.exit(1);
      }
      const slashIdx = docArg.indexOf('/');
      const checkClass = docArg.slice(0, slashIdx);
      const checkCode  = docArg.slice(slashIdx + 1);

      const config = loadConfig();
      const http = new AbapHttp(config);
      const csrfToken = await http.fetchCsrfToken();

      try {
        const result = await http.post('/sap/bc/z_abapgit_agent/insp_doc', {
          check_class: checkClass,
          check_code:  checkCode
        }, { csrfToken });

        const title = result.TITLE || result.title || '';
        let   text  = result.TEXT  || result.text  || '';

        // Strip ITF inline tags (<AB>, <EX>, </>, <DS:...>, etc.), HTML tags, and ITF placeholders (&...&)
        text = text
          .replace(/<style[^>]*>[\s\S]*?<\/style>/gi, '')
          .replace(/<[^>]+>/g, '')
          .replace(/&[A-Z_]+&/g, '')
          .replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&')
          .replace(/&nbsp;/g, ' ').replace(/&#160;/g, ' ')
          .replace(/\n{3,}/g, '\n\n')
          .trim();

        if (title) console.log(`\n${title}\n${'─'.repeat(Math.min(title.length, 60))}`);
        console.log(`\n${text || 'No documentation available.'}\n`);
      } catch (error) {
        const { printHttpError } = require('../utils/format-error');
        printHttpError(error, { verbose: args.includes('--verbose') });
        process.exit(1);
      }
      return;
    }

    const jsonOutput = args.includes('--json');
    const verbose = args.includes('--verbose');
    const filesArgIndex = args.indexOf('--files');
    if (filesArgIndex === -1 || filesArgIndex + 1 >= args.length) {
      console.error('Error: --files parameter required');
      console.error('Usage: abapgit-agent inspect --files <file1>,<file2>,... [--variant <check-variant>] [--junit-output <file>] [--json]');
      console.error('Example: abapgit-agent inspect --files src/zcl_my_class.clas.abap');
      console.error('Example: abapgit-agent inspect --files src/zcl_my_class.clas.abap --variant ALL_CHECKS');
      console.error('Example: abapgit-agent inspect --files src/zcl_my_class.clas.abap --junit-output reports/inspect.xml');
      process.exit(1);
    }

    let filesSyntaxCheck = [...new Set(args[filesArgIndex + 1].split(',').map(f => f.trim()))];

    // Skip XML-only object types that have no ABAP source to inspect
    const XML_ONLY_EXTENSIONS = [
      '.pinf.xml', '.suso.xml', '.msag.xml', '.doma.xml',
      '.dtel.xml', '.ttyp.xml', '.tabl.xml',
    ];
    const beforeXmlOnly = filesSyntaxCheck.length;
    filesSyntaxCheck = filesSyntaxCheck.filter(f => {
      const lower = pathModule.basename(f).toLowerCase();
      return !XML_ONLY_EXTENSIONS.some(ext => lower.endsWith(ext));
    });
    if (beforeXmlOnly > filesSyntaxCheck.length && !args.includes('--json')) {
      console.log(`  Skipped ${beforeXmlOnly - filesSyntaxCheck.length} XML-only file(s) (no source to inspect)`);
    }

    // Parse optional --variant parameter; fall back to project config
    const variantArgIndex = args.indexOf('--variant');
    const variantArg = variantArgIndex !== -1 ? args[variantArgIndex + 1] : null;
    const inspectConfig = getInspectConfig();
    const variant = variantArg || inspectConfig.variant || null;

    // Filter out files matching inspect.exclude patterns from project config
    const excludePatterns = inspectConfig.exclude || [];
    if (excludePatterns.length > 0) {
      const before = filesSyntaxCheck.length;
      filesSyntaxCheck = filesSyntaxCheck.filter(f => {
        const objName = pathModule.basename(f).split('.')[0].toUpperCase();
        return !excludePatterns.some(pattern => {
          const re = new RegExp('^' + pattern.toUpperCase().replace(/\*/g, '.*') + '$');
          return re.test(objName);
        });
      });
      const skipped = before - filesSyntaxCheck.length;
      if (skipped > 0 && !args.includes('--json')) {
        console.log(`  Skipped ${skipped} file(s) excluded by inspect config`);
      }
      if (filesSyntaxCheck.length === 0) {
        if (args.includes('--json')) {
          console.log(JSON.stringify([]));
        } else {
          console.log('\n  All files excluded by inspect config — nothing to check.\n');
        }
        return;
      }
    }

    // Parse optional --junit-output parameter
    const junitArgIndex = args.indexOf('--junit-output');
    const junitOutput = junitArgIndex !== -1 ? args[junitArgIndex + 1] : null;

    if (!jsonOutput) {
      console.log(`\n  Inspect for ${filesSyntaxCheck.length} file(s)`);
      if (variant) {
        const source = variantArg ? '' : ' (from project config)';
        console.log(`  Using variant: ${variant}${source}`);
      }
      if (junitOutput) {
        console.log(`  JUnit output: ${junitOutput}`);
      }
      console.log('');
    }

    const config = loadConfig();
    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    const suppressRules = inspectConfig.suppress || [];
    let hasErrors = false;

    // Stream results: apply suppress and print each object as its chunk completes
    const onChunkResult = jsonOutput ? null : async (result) => {
      applySuppressRules(result, suppressRules);
      await processInspectResult(result);
      const errorCount = result.ERROR_COUNT !== undefined ? result.ERROR_COUNT : (result.error_count || 0);
      if (errorCount > 0) hasErrors = true;
    };

    const results = await inspectInChunks(filesSyntaxCheck, csrfToken, config, variant, http, verbose, onChunkResult);

    // For JSON mode, apply suppress rules now (they were skipped during streaming)
    if (jsonOutput) {
      for (const result of results) {
        applySuppressRules(result, suppressRules);
      }
    }

    // JUnit output mode — write XML file, then continue to normal output
    if (junitOutput) {
      const xml = buildInspectJUnit(results);
      const outputPath = pathModule.isAbsolute(junitOutput)
        ? junitOutput
        : pathModule.join(process.cwd(), junitOutput);
      const dir = pathModule.dirname(outputPath);
      if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir, { recursive: true });
      }
      fs.writeFileSync(outputPath, xml, 'utf8');
      if (!jsonOutput) {
        console.log(`  JUnit report written to: ${outputPath}`);
      }
    }

    // JSON output mode
    if (jsonOutput) {
      console.log(JSON.stringify(results, null, 2));
      return;
    }

    if (hasErrors) {
      process.exit(1);
    }
  }
};
