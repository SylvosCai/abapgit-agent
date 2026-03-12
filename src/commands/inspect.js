/**
 * Inspect command - Syntax check for ABAP files
 */

const pathModule = require('path');
const { printHttpError } = require('../utils/format-error');

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
    const { loadConfig, AbapHttp } = context;

    const jsonOutput = args.includes('--json');
    const verbose = args.includes('--verbose');
    const filesArgIndex = args.indexOf('--files');
    if (filesArgIndex === -1 || filesArgIndex + 1 >= args.length) {
      console.error('Error: --files parameter required');
      console.error('Usage: abapgit-agent inspect --files <file1>,<file2>,... [--variant <check-variant>] [--json]');
      console.error('Example: abapgit-agent inspect --files src/zcl_my_class.clas.abap');
      console.error('Example: abapgit-agent inspect --files src/zcl_my_class.clas.abap --variant ALL_CHECKS');
      console.error('Example: abapgit-agent inspect --files src/zcl_my_class.clas.abap --json');
      process.exit(1);
    }

    const filesSyntaxCheck = args[filesArgIndex + 1].split(',').map(f => f.trim());

    // Parse optional --variant parameter
    const variantArgIndex = args.indexOf('--variant');
    const variant = variantArgIndex !== -1 ? args[variantArgIndex + 1] : null;

    if (!jsonOutput) {
      console.log(`\n  Inspect for ${filesSyntaxCheck.length} file(s)`);
      if (variant) {
        console.log(`  Using variant: ${variant}`);
      }
      console.log('');
    }

    const config = loadConfig();
    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    // Send all files in one request
    const results = await inspectAllFiles(filesSyntaxCheck, csrfToken, config, variant, http, verbose);

    // JSON output mode
    if (jsonOutput) {
      console.log(JSON.stringify(results, null, 2));
      return;
    }

    // Process results
    for (const result of results) {
      await processInspectResult(result);
    }
  }
};
