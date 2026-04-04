/**
 * Unit command - Run AUnit tests for ABAP test class files
 */

const pathModule = require('path');
const fs = require('fs');
const { formatHttpError } = require('../utils/format-error');

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
 * Build JUnit XML from unit test results array.
 *
 * Maps to JUnit schema:
 *   <testsuites>
 *     <testsuite name="ZCL_MY_TEST" tests="10" failures="2" errors="0">
 *       <testcase name="TEST_METHOD_1" classname="ZCL_MY_TEST"/>
 *       <testcase name="TEST_METHOD_2" classname="ZCL_MY_TEST">
 *         <failure type="FAILURE" message="...">detail</failure>
 *       </testcase>
 *     </testsuite>
 *   </testsuites>
 *
 * One testsuite per test class file. Each failed test method becomes a <failure>.
 * Passing methods are listed as empty <testcase> elements (Jenkins counts them).
 */
function buildUnitJUnit(results) {
  const suites = results.map(res => {
    const success      = res.SUCCESS      || res.success;
    const testCount    = res.TEST_COUNT    || res.test_count    || 0;
    const passedCount  = res.PASSED_COUNT  || res.passed_count  || 0;
    const failedCount  = res.FAILED_COUNT  || res.failed_count  || 0;
    const errors       = res.ERRORS        || res.errors        || [];
    const className    = res._className    || 'UNKNOWN';  // injected by caller

    // Build a set of failed method names for quick lookup
    const failedMethods = new Set(
      errors.map(e => (e.CLASS_NAME || e.class_name || '') + '=>' + (e.METHOD_NAME || e.method_name || ''))
    );

    const testcases = [];

    // Emit one <testcase> per failed test
    for (const err of errors) {
      const errClassName  = err.CLASS_NAME  || err.class_name  || className;
      const methodName    = err.METHOD_NAME || err.method_name || '?';
      const errorKind     = err.ERROR_KIND  || err.error_kind  || 'FAILURE';
      const errorText     = err.ERROR_TEXT  || err.error_text  || 'Test failed';
      testcases.push(
        `    <testcase name="${escapeXml(methodName)}" classname="${escapeXml(errClassName)}">\n` +
        `      <failure type="${escapeXml(errorKind)}" message="${escapeXml(errorText)}">${escapeXml(errorText)}</failure>\n` +
        `    </testcase>`
      );
    }

    // Emit empty <testcase> elements for passing tests (Jenkins shows total count)
    // We can't enumerate them individually (ABAP doesn't return passing method names),
    // so emit one aggregate passing testcase when passedCount > 0
    if (passedCount > 0) {
      testcases.push(
        `    <testcase name="(${passedCount} passing test(s))" classname="${escapeXml(className)}"/>`
      );
    }

    if (testCount === 0) {
      testcases.push(`    <testcase name="(no tests)" classname="${escapeXml(className)}"/>`);
    }

    return (
      `  <testsuite name="${escapeXml(className)}" ` +
      `tests="${Math.max(testCount, 1)}" failures="${failedCount}" errors="0">\n` +
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
 * Run unit test for a single file
 */
async function runUnitTestForFile(sourceFile, csrfToken, config, coverage, http, jsonOutput = false, verbose = false) {
  if (!jsonOutput) {
    console.log(`  Running unit test for: ${sourceFile}`);
  }

  try {
    // Read file content
    const absolutePath = pathModule.isAbsolute(sourceFile)
      ? sourceFile
      : pathModule.join(process.cwd(), sourceFile);

    if (!fs.existsSync(absolutePath)) {
      const error = {
        file: sourceFile,
        error: 'File not found',
        statusCode: 404
      };
      if (!jsonOutput) {
        console.error(`  ❌ File not found: ${absolutePath}`);
      }
      return error;
    }

    // Extract object type and name from file path
    // e.g., "zcl_my_test.clas.abap" -> CLAS, ZCL_MY_TEST
    const fileName = pathModule.basename(sourceFile).toUpperCase();
    const parts = fileName.split('.');
    if (parts.length < 3) {
      const error = {
        file: sourceFile,
        error: 'Invalid file format',
        statusCode: 400
      };
      if (!jsonOutput) {
        console.error(`  ❌ Invalid file format: ${sourceFile}`);
      }
      return error;
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

    if (!jsonOutput) {
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
    }

    return result;
  } catch (error) {
    // Build error response object
    const errorResponse = {
      file: sourceFile,
      error: error.message || 'Unknown error',
      statusCode: error.statusCode || 500
    };

    // Add additional error details if available
    if (error.body) {
      errorResponse.body = error.body;
    }

    if (!jsonOutput) {
      console.error(`\n  ❌ Error: ${formatHttpError(error)}`);
      if (verbose && error.body) {
        console.error('\n--- Raw response body ---');
        const raw = typeof error.body === 'object' ? JSON.stringify(error.body, null, 2) : String(error.body);
        console.error(raw);
        console.error('--- End of response body ---');
      }
    }

    return errorResponse;
  }
}

module.exports = {
  name: 'unit',
  description: 'Run AUnit tests for ABAP test class files',
  requiresAbapConfig: true,
  requiresVersionCheck: true,

  async execute(args, context) {
    const { loadConfig, AbapHttp } = context;

    if (args.includes('--help') || args.includes('-h')) {
      console.log(`
Usage:
  abapgit-agent unit --files <file1>,<file2>,... [--coverage] [--junit-output <file>] [--json]

Description:
  Run AUnit tests for ABAP test class files (.testclasses.abap).
  Objects must be already active in the ABAP system (run pull first).

Parameters:
  --files <file1,...>     Comma-separated .testclasses.abap files (required).
  --coverage              Include code coverage data in output.
  --junit-output <file>   Write results as JUnit XML to this file.
  --json                  Output as JSON.

Examples:
  abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap
  abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap --coverage
  abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap --junit-output reports/unit.xml
`);
      return;
    }

    const jsonOutput = args.includes('--json');
    const verbose = args.includes('--verbose');
    const filesArgIndex = args.indexOf('--files');
    if (filesArgIndex === -1 || filesArgIndex + 1 >= args.length) {
      console.error('Error: --files parameter required');
      console.error('Usage: abapgit-agent unit --files <file1>,<file2>,... [--coverage] [--junit-output <file>] [--json]');
      console.error('Example: abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap');
      console.error('Example: abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap --coverage');
      console.error('Example: abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap --junit-output reports/unit.xml');
      process.exit(1);
    }

    const files = args[filesArgIndex + 1].split(',').map(f => f.trim());

    // Check for coverage option
    const coverage = args.includes('--coverage');

    // Parse optional --junit-output parameter
    const junitArgIndex = args.indexOf('--junit-output');
    const junitOutput = junitArgIndex !== -1 ? args[junitArgIndex + 1] : null;

    if (!jsonOutput) {
      console.log(`\n  Running unit tests for ${files.length} file(s)${coverage ? ' (with coverage)' : ''}`);
      if (junitOutput) {
        console.log(`  JUnit output: ${junitOutput}`);
      }
      console.log('');
    }

    const config = loadConfig();
    const http = new AbapHttp(config);
    const csrfToken = await http.fetchCsrfToken();

    // Collect results for JSON / JUnit output
    const results = [];
    let hasErrors = false;

    for (const sourceFile of files) {
      const result = await runUnitTestForFile(sourceFile, csrfToken, config, coverage, http, jsonOutput, verbose);
      if (result) {
        // Inject class name derived from file path for JUnit builder
        const fileName = pathModule.basename(sourceFile).toUpperCase();
        result._className = fileName.split('.')[0];
        results.push(result);

        if (result.error || result.statusCode >= 400) {
          hasErrors = true;
        }
        // Also treat failed tests as an error for exit code
        const failedCount = result.FAILED_COUNT || result.failed_count || 0;
        if (failedCount > 0) {
          hasErrors = true;
        }
      }
    }

    // JUnit output mode — write XML, then continue to normal output
    if (junitOutput) {
      const xml = buildUnitJUnit(results);
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
    }

    // Exit with error code if any tests failed or had errors
    if (hasErrors) {
      if (!jsonOutput) {
        console.error('\n❌ Unit tests completed with errors');
      }
      process.exit(1);
    }
  }
};
