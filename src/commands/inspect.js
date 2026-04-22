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
 *       <testcase name="Syntax check" classname="ZCL_MY_CLASS">
 *         <failure type="SyntaxError" message="...">line/col/method detail</failure>
 *       </testcase>
 *     </testsuite>
 *   </testsuites>
 *
 * One testsuite per object. Each error becomes a <failure>. Warnings become
 * a single <failure type="Warning"> so they are visible but don't fail the build
 * unless there are also hard errors (Jenkins distinguishes failure vs unstable).
 */
function buildInspectJUnit(results) {
  const suites = results.map(res => {
    const objectType = res.OBJECT_TYPE !== undefined ? res.OBJECT_TYPE : (res.object_type || 'UNKNOWN');
    const objectName = res.OBJECT_NAME !== undefined ? res.OBJECT_NAME : (res.object_name || 'UNKNOWN');
    const errors     = res.ERRORS   !== undefined ? res.ERRORS   : (res.errors   || []);
    const warnings   = res.WARNINGS !== undefined ? res.WARNINGS : (res.warnings || []);
    const errorCount = errors.length;
    const warnCount  = warnings.length;
    // One testcase per error/warning; at least one testcase for a clean object
    const testCount  = Math.max(1, errorCount + warnCount);

    const testcases = [];

    if (errorCount === 0 && warnCount === 0) {
      testcases.push(`    <testcase name="Syntax check" classname="${escapeXml(objectName)}"/>`);
    }

    for (const err of errors) {
      const line       = err.LINE       || err.line       || '?';
      const column     = err.COLUMN     || err.column     || '?';
      const text       = err.TEXT       || err.text       || 'Unknown error';
      const methodName = err.METHOD_NAME || err.method_name;
      const sobjname   = err.SOBJNAME   || err.sobjname   || '';
      const detail     = [
        methodName ? `Method: ${methodName}` : null,
        `Line ${line}, Column ${column}`,
        sobjname ? `Include: ${sobjname}` : null,
        text
      ].filter(Boolean).join('\n');
      const caseName = methodName ? `${methodName} line ${line}` : `Line ${line}`;
      testcases.push(
        `    <testcase name="${escapeXml(caseName)}" classname="${escapeXml(objectName)}">\n` +
        `      <failure type="SyntaxError" message="${escapeXml(text)}">${escapeXml(detail)}</failure>\n` +
        `    </testcase>`
      );
    }

    for (const warn of warnings) {
      const line       = warn.LINE       || warn.line       || '?';
      const text       = warn.MESSAGE    || warn.message    || warn.TEXT || warn.text || 'Warning';
      const methodName = warn.METHOD_NAME || warn.method_name;
      const sobjname   = warn.SOBJNAME   || warn.sobjname   || '';
      const detail     = [
        methodName ? `Method: ${methodName}` : null,
        `Line ${line}`,
        sobjname ? `Include: ${sobjname}` : null,
        text
      ].filter(Boolean).join('\n');
      const caseName = methodName ? `${methodName} line ${line} (warning)` : `Line ${line} (warning)`;
      testcases.push(
        `    <testcase name="${escapeXml(caseName)}" classname="${escapeXml(objectName)}">\n` +
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
 * Inspect all files in one request
 */
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

      if (methodName) {
        console.log(`  Method: ${methodName}`);
      }
      console.log(`  Line ${line}, Column ${column}:`);
      if (sobjname && sobjname.includes('====')) {
        console.log(`    Include: ${sobjname}`);
      }
      console.log(`    ${text}`);
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

        if (methodName) {
          console.log(`  Method: ${methodName}`);
        }
        console.log(`  Line ${line}:`);
        if (sobjname && sobjname.includes('====')) {
          console.log(`    Include: ${sobjname}`);
        }
        console.log(`    ${text}`);
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

        if (methodName) {
          console.log(`  Method: ${methodName}`);
        }
        console.log(`  Line ${line}:`);
        if (sobjname && sobjname.includes('====')) {
          console.log(`    Include: ${sobjname}`);
        }
        console.log(`    ${text}`);
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

Description:
  Run SAP Code Inspector checks on activated ABAP objects. Requires the objects
  to be already active in the ABAP system (run pull first).

Parameters:
  --files <file1,...>       Comma-separated ABAP source files (required).
  --variant <variant>       Code Inspector variant (default: system default).
  --junit-output <file>     Write results as JUnit XML to this file.
  --json                    Output as JSON.

Examples:
  abapgit-agent inspect --files src/zcl_my_class.clas.abap
  abapgit-agent inspect --files src/zcl_my_class.clas.abap --variant ALL_CHECKS
  abapgit-agent inspect --files src/zcl_my_class.clas.abap --junit-output reports/inspect.xml
`);
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

    const filesSyntaxCheck = args[filesArgIndex + 1].split(',').map(f => f.trim());

    // Parse optional --variant parameter; fall back to project config
    const variantArgIndex = args.indexOf('--variant');
    const variantArg = variantArgIndex !== -1 ? args[variantArgIndex + 1] : null;
    const inspectConfig = getInspectConfig();
    const variant = variantArg || inspectConfig.variant || null;

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

    // Send all files in one request
    const results = await inspectAllFiles(filesSyntaxCheck, csrfToken, config, variant, http, verbose);

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

    // Process results
    let hasErrors = false;
    for (const result of results) {
      await processInspectResult(result);
      const errorCount = result.ERROR_COUNT !== undefined ? result.ERROR_COUNT : (result.error_count || 0);
      if (errorCount > 0) hasErrors = true;
    }

    if (hasErrors) {
      process.exit(1);
    }
  }
};
