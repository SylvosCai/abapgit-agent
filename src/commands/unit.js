/**
 * Unit command - Run AUnit tests for ABAP test class files
 */

const pathModule = require('path');
const fs = require('fs');

/**
 * Run unit test for a single file
 */
async function runUnitTestForFile(sourceFile, csrfToken, config, coverage, http) {
  console.log(`  Running unit test for: ${sourceFile}`);

  try {
    // Read file content
    const absolutePath = pathModule.isAbsolute(sourceFile)
      ? sourceFile
      : pathModule.join(process.cwd(), sourceFile);

    if (!fs.existsSync(absolutePath)) {
      console.error(`  ❌ File not found: ${absolutePath}`);
      return;
    }

    // Extract object type and name from file path
    // e.g., "zcl_my_test.clas.abap" -> CLAS, ZCL_MY_TEST
    const fileName = pathModule.basename(sourceFile).toUpperCase();
    const parts = fileName.split('.');
    if (parts.length < 3) {
      console.error(`  ❌ Invalid file format: ${sourceFile}`);
      return;
    }

    // obj_name is first part (may contain path), obj_type is second part
    const objType = parts[1] === 'CLASS' ? 'CLAS' : parts[1];
    let objName = parts[0];

    // Handle subdirectory paths
    const lastSlash = objName.lastIndexOf('/');
    if (lastSlash >= 0) {
      objName = objName.substring(lastSlash + 1);
    }

    // Send files array to unit endpoint (ABAP expects string_table of file names)
    const data = {
      files: [sourceFile],
      coverage: coverage
    };

    const result = await http.post('/sap/bc/z_abapgit_agent/unit', data, { csrfToken });

    // Handle uppercase keys from ABAP
    const success = result.SUCCESS || result.success;
    const testCount = result.TEST_COUNT || result.test_count || 0;
    const passedCount = result.PASSED_COUNT || result.passed_count || 0;
    const failedCount = result.FAILED_COUNT || result.failed_count || 0;
    const message = result.MESSAGE || result.message || '';
    const errors = result.ERRORS || result.errors || [];

    // Handle coverage data
    const coverageStats = result.COVERAGE_STATS || result.coverage_stats;

    if (testCount === 0) {
      console.log(`  ➖ ${objName} - No unit tests`);
    } else if (success === 'X' || success === true) {
      console.log(`  ✅ ${objName} - All tests passed`);
    } else {
      console.log(`  ❌ ${objName} - Tests failed`);
    }

    console.log(`     Tests: ${testCount} | Passed: ${passedCount} | Failed: ${failedCount}`);

    // Display coverage if available
    if (coverage && coverageStats) {
      const totalLines = coverageStats.TOTAL_LINES || coverageStats.total_lines || 0;
      const coveredLines = coverageStats.COVERED_LINES || coverageStats.covered_lines || 0;
      const coverageRate = coverageStats.COVERAGE_RATE || coverageStats.coverage_rate || 0;

      console.log(`     📊 Coverage: ${coverageRate}%`);
      console.log(`        Total Lines: ${totalLines}`);
      console.log(`        Covered Lines: ${coveredLines}`);
    }

    if (failedCount > 0 && errors.length > 0) {
      for (const err of errors) {
        const className = err.CLASS_NAME || err.class_name || '?';
        const methodName = err.METHOD_NAME || err.method_name || '?';
        const errorText = err.ERROR_TEXT || err.error_text || 'Unknown error';
        console.log(`     ✗ ${className}=>${methodName}: ${errorText}`);
      }
    }

    return result;
  } catch (error) {
    console.error(`\n  Error: ${error.message}`);
  }
}

module.exports = {
  name: 'unit',
  description: 'Run AUnit tests for ABAP test class files',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, AbapHttp } = context;

    const filesArgIndex = args.indexOf('--files');
    if (filesArgIndex === -1 || filesArgIndex + 1 >= args.length) {
      console.error('Error: --files parameter required');
      console.error('Usage: abapgit-agent unit --files <file1>,<file2>,... [--coverage]');
      console.error('Example: abapgit-agent unit --files src/zcl_my_test.clas.abap');
      console.error('Example: abapgit-agent unit --files src/zcl_my_test.clas.abap --coverage');
      process.exit(1);
    }

    const files = args[filesArgIndex + 1].split(',').map(f => f.trim());

    // Check for coverage option
    const coverage = args.includes('--coverage');

    console.log(`\n  Running unit tests for ${files.length} file(s)${coverage ? ' (with coverage)' : ''}`);
    console.log('');

    const config = loadConfig();
    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    for (const sourceFile of files) {
      await runUnitTestForFile(sourceFile, csrfToken, config, coverage, http);
    }
  }
};
