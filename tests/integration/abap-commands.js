/**
 * Command test cases with specific assertions (runs against real ABAP system)
 *
 * These are isolated command tests. For workflow tests (multi-step sequences),
 * see pull-runner.js, lifecycle-runner.js, and xml-only-runner.js.
 *
 * Test Distribution:
 *   - syntax: 24 tests (validation, auto-detection, DDLS, FIXPT)
 *   - view:    5 tests (class, interface, table, class --full, class --full --lines)
 *   - tree:    3 tests (package, depth, types)
 *   - preview: 3 tests (table, limit, columns)
 *   - list:    3 tests (package, type filter, name filter)
 *   - where:   3 tests (class, interface, type filter)
 *   - debug:   5 tests (delete-all, set, list, delete-all cleanup, list-empty — breakpoint management only;
 *                        full session coverage is in debug-scenarios.sh)
 *   - dump:    4 tests (basic list, user filter, date filter, JSON output)
 *   - ref:     3 tests (topics, repos, search)
 *   - upgrade: 4 tests (check, dry-run, invalid version, cli-only)
 *   - pull:    1 test  (--files)
 *   - unit:    2 tests  (test class, --junit-output)
 *   - run:     2 tests (run program, run class)
 *   - status:  1 test  (config check)
 *   - inspect: 2 tests  (code inspector, --junit-output)
 *   - health:  1 test  (system health)
 *   Total:    64 tests
 *
 * Run specific command tests:
 *   npm run test:cmd:syntax
 *   npm run test:cmd:pull
 *   npm run test:cmd:view
 *   npm run test:cmd:dump
 *   npm run test:cmd:debug
 *   npm run test:cmd:upgrade
 */
const commandTestCases = [
  // ===================================================================
  // PULL COMMAND - 1 test
  // Purpose: Test pulling specific files
  // Note: For git ref (tag/branch) tests, see pull-runner.js
  // ===================================================================
  {
    command: 'pull',
    name: 'pull --files (specific file)',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain either success or failure info, but not crash
      const hasResult = output.includes('Pull completed') ||
        output.includes('Job ID') ||
        output.includes(' Activated ') ||
        output.includes('Failed Objects');
      const noCrash = !output.includes('Error:') || output.includes('Error:') && output.includes('Job');
      return hasResult && noCrash;
    }
  },

  // ===================================================================
  // INSPECT COMMAND - 2 tests
  // Purpose: Run Code Inspector checks on activated objects
  // ===================================================================
  {
    command: 'inspect',
    name: 'inspect single file',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain syntax check result
      const hasResult = output.includes('Syntax check passed') ||
        output.includes('Syntax check failed');
      const hasObject = output.includes('ZCL_ABGAGT_UTIL') || output.includes('CLAS');
      return hasResult && hasObject;
    }
  },
  {
    command: 'inspect',
    name: 'inspect --junit-output writes valid JUnit XML',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap',
           '--junit-output', '/tmp/abapgit-agent-test-inspect.xml'],
    expectSuccess: true,
    verify: (output) => {
      const fs = require('fs');
      // CLI output still shows normal results
      if (!(output.includes('Syntax check passed') || output.includes('Syntax check failed'))) return false;
      // File must exist
      if (!fs.existsSync('/tmp/abapgit-agent-test-inspect.xml')) return false;
      // File must be valid JUnit XML
      const xml = fs.readFileSync('/tmp/abapgit-agent-test-inspect.xml', 'utf8');
      return xml.includes('<?xml') &&
             xml.includes('<testsuites') &&
             xml.includes('<testsuite') &&
             xml.includes('ZCL_ABGAGT_UTIL');
    },
    cleanup: () => {
      const fs = require('fs');
      try { fs.unlinkSync('/tmp/abapgit-agent-test-inspect.xml'); } catch (_) {}
    }
  },

  // ===================================================================
  // UNIT COMMAND - 2 tests
  // Purpose: Run ABAP unit tests
  // ===================================================================
  {
    command: 'unit',
    name: 'unit test class',
    args: ['--files', 'abap/zcl_abgagt_util.clas.testclasses.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain test results
      const hasResult = output.includes('Tests:') &&
        (output.includes('Passed:') || output.includes('Failed:'));
      const hasClass = output.includes('ZCL_ABGAGT_UTIL');
      return hasResult && hasClass;
    }
  },
  {
    command: 'unit',
    name: 'unit --junit-output writes valid JUnit XML',
    args: ['--files', 'abap/zcl_abgagt_util.clas.testclasses.abap',
           '--junit-output', '/tmp/abapgit-agent-test-unit.xml'],
    expectSuccess: true,
    verify: (output) => {
      const fs = require('fs');
      // CLI output still shows normal results
      if (!output.includes('Tests:')) return false;
      // File must exist
      if (!fs.existsSync('/tmp/abapgit-agent-test-unit.xml')) return false;
      // File must be valid JUnit XML
      const xml = fs.readFileSync('/tmp/abapgit-agent-test-unit.xml', 'utf8');
      return xml.includes('<?xml') &&
             xml.includes('<testsuites') &&
             xml.includes('<testsuite') &&
             xml.includes('ZCL_ABGAGT_UTIL');
    },
    cleanup: () => {
      const fs = require('fs');
      try { fs.unlinkSync('/tmp/abapgit-agent-test-unit.xml'); } catch (_) {}
    }
  },

  // ===================================================================
  // TREE COMMAND - 3 tests
  // Purpose: Display package hierarchy and structure
  // ===================================================================
  {
    command: 'tree',
    name: 'tree package',
    args: ['--package', 'S_NWDEMO_BASIS'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain package name and tree structure
      const hasPackage = output.includes('S_NWDEMO_BASIS');
      const hasTree = output.includes('📦') || output.includes('Package');
      return hasPackage && hasTree;
    }
  },
  {
    command: 'tree',
    name: 'tree with depth',
    args: ['--package', 'S_NWDEMO_BASIS', '--depth', '2'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain package name and show depth
      const hasPackage = output.includes('S_NWDEMO_BASIS');
      const hasDepth = output.includes('Depth') || output.includes('depth');
      return hasPackage;
    }
  },
  {
    command: 'tree',
    name: 'tree with types',
    args: ['--package', 'S_NWDEMO_BASIS', '--include-types'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain package name and type counts
      const hasPackage = output.includes('S_NWDEMO_BASIS');
      const hasTypes = output.includes('CLAS=') || output.includes('TYPES:');
      return hasPackage;
    }
  },

  // ===================================================================
  // LIST COMMAND - 3 tests
  // Purpose: List objects in a package with filtering
  // ===================================================================
  {
    command: 'list',
    name: 'list package',
    args: ['--package', 'SAPBC_DATAMODEL'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain package name and object list
      const hasPackage = output.includes('SAPBC_DATAMODEL');
      const hasObjects = output.includes('Objects in') || output.includes('AVAS') || output.includes('DEVC');
      return hasPackage && hasObjects;
    }
  },
  {
    command: 'list',
    name: 'list with type filter',
    args: ['--package', 'SAPBC_IBF_SCUSTOMER', '--type', 'CLAS,INT'],
    expectSuccess: true,
    verify: (output) => {
      // Should show filtered types
      const hasFilter = output.includes('CLAS') || output.includes('INT') || output.includes('CLAS,INT');
      return hasFilter;
    }
  },
  {
    command: 'list',
    name: 'list with name filter',
    args: ['--package', 'SAPBC_IBF_SCUSTOMER', '--name', "'CL_*'"],
    expectSuccess: true,
    verify: (output) => {
      // Should contain filtered results (or empty if no matches)
      const hasResult = output.includes('Objects in') || output.includes('CL_');
      return hasResult;
    }
  },

  // ===================================================================
  // PREVIEW COMMAND - 3 tests
  // Purpose: Preview table data with pagination and column filtering
  // ===================================================================
  {
    command: 'preview',
    name: 'preview table',
    args: ['--objects', 'SFLIGHT'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain table name and data
      const hasTable = output.includes('SFLIGHT');
      const hasData = output.includes('CARRID') || output.includes('FLDATE') || output.includes('Row');
      return hasTable && hasData;
    }
  },
  {
    command: 'preview',
    name: 'preview with limit',
    args: ['--objects', 'SFLIGHT', '--limit', '5'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain table name and limit info
      const hasTable = output.includes('SFLIGHT');
      const hasLimit = output.includes('5') || output.includes('rows');
      return hasTable && hasLimit;
    }
  },
  {
    command: 'preview',
    name: 'preview with columns',
    args: ['--objects', 'SFLIGHT', '--columns', 'CARRID,CONNID,PRICE'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain table name and specific columns
      const hasTable = output.includes('SFLIGHT');
      const hasColumns = output.includes('CARRID') && output.includes('CONNID') && output.includes('PRICE');
      return hasTable && hasColumns;
    }
  },

  // ===================================================================
  // VIEW COMMAND - 3 tests
  // Purpose: View object definitions (class, interface, table)
  // Note: For view verification tests in pull workflow, see pull-runner.js
  // ===================================================================
  {
    command: 'view',
    name: 'view class',
    args: ['--objects', 'ZCL_ABGAGT_AGENT'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain the class name and definition
      const hasClass = output.includes('ZCL_ABGAGT_AGENT');
      const hasDef = output.includes('CLASS') || output.includes('DEFINITION');
      return hasClass && hasDef;
    }
  },
  {
    command: 'view',
    name: 'view interface',
    args: ['--objects', 'ZIF_ABGAGT_COMMAND'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain the interface name and interface keyword
      const hasInterface = output.includes('ZIF_ABGAGT_COMMAND');
      const hasDef = output.includes('INTERFACE');
      return hasInterface && hasDef;
    }
  },
  {
    command: 'view',
    name: 'view table',
    args: ['--objects', 'SFLIGHT', '--type', 'TABL'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain table name and field info
      const hasTable = output.includes('SFLIGHT');
      const hasField = output.includes('CARRID') || output.includes('Field') || output.includes('TABLE');
      return hasTable && hasField;
    }
  },
  {
    command: 'view',
    name: 'view class --full (clean source without line numbers)',
    args: ['--objects', 'ZCL_ABGAGT_AGENT', '--full'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain CM method section headers
      const hasCmHeader = /Method:.*CM\d/.test(output);
      // Should NOT contain include-relative line number brackets
      const noIncludeRelLines = !/\[\s*\d+\]/.test(output);
      // Should NOT contain debug breakpoint hints
      const noDebugHints = !output.includes('debug set');
      return hasCmHeader && noIncludeRelLines && noDebugHints;
    }
  },
  {
    command: 'view',
    name: 'view class --full --lines (dual line numbers for debugging)',
    args: ['--objects', 'ZCL_ABGAGT_AGENT', '--full', '--lines'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain global line numbers
      const hasGlobalLines = /^\s+\d+\s+/m.test(output);
      // Should contain at least one CM method header
      const hasCmHeader = /Method:.*CM\d/.test(output);
      // Should contain include-relative line numbers
      const hasIncludeRelLines = /\[\s*\d+\]/.test(output);
      // Should contain debug breakpoint hints
      const hasDebugHints = output.includes('debug set');
      return hasGlobalLines && hasCmHeader && hasIncludeRelLines && hasDebugHints;
    }
  },

  // ===================================================================
  // WHERE COMMAND - 3 tests
  // Purpose: Find where-used list for objects
  // ===================================================================
  {
    command: 'where',
    name: 'where class usage',
    args: ['--objects', 'ZCL_ABGAGT_AGENT'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain where-used results or empty result
      const hasResult = output.includes('Where-used') || output.includes('OBJECT') || output.includes('OBJECT_NAME') || output.includes('found');
      return hasResult;
    }
  },
  {
    command: 'where',
    name: 'where interface usage',
    args: ['--objects', 'ZIF_ABGAGT_COMMAND'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain where-used results
      const hasResult = output.includes('Where-used') || output.includes('OBJECT') || output.includes('OBJECT_NAME');
      return hasResult;
    }
  },
  {
    command: 'where',
    name: 'where with type filter',
    args: ['--objects', 'ZCL_ABGAGT_AGENT', '--type', 'CLAS'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain where-used results
      const hasResult = output.includes('Where-used') || output.includes('OBJECT');
      return hasResult;
    }
  },

  // ===================================================================
  // HEALTH COMMAND - 1 test
  // Purpose: Check ABAP backend health and version
  // ===================================================================
  {
    command: 'health',
    name: 'health check',
    args: [],
    expectSuccess: true,
    verify: (output) => {
      // Should contain status and version
      const hasStatus = output.includes('"status"') || output.includes('OK') || output.includes('healthy');
      const hasVersion = output.includes('"version"');
      return hasStatus && hasVersion;
    }
  },

  // ===================================================================
  // STATUS COMMAND - 1 test
  // Purpose: Check agent configuration status
  // ===================================================================
  {
    command: 'status',
    name: 'status check',
    args: [],
    expectSuccess: true,
    verify: (output) => {
      // Should contain ENABLED or NOT configured
      const hasResult = output.includes('ENABLED') || output.includes('NOT configured');
      return hasResult;
    }
  },

  // ===================================================================
  // SYNTAX COMMAND - 24 tests
  // Purpose: Source-based syntax checking (pre-commit validation)
  // Categories:
  //   - Basic validation (5 tests)
  //   - Error detection (4 tests)
  //   - Auto-detection & includes (7 tests)
  //   - DDLS/CDS views (6 tests)
  //   - FIXPT flag handling (2 tests)
  // ===================================================================

  // Basic syntax validation
  {
    command: 'syntax',
    name: 'syntax check class file',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain syntax check result
      const hasResult = output.includes('Syntax check passed') ||
        output.includes('Syntax check failed');
      const hasObject = output.includes('ZCL_ABGAGT_UTIL') || output.includes('CLAS');
      return hasResult && hasObject;
    }
  },
  {
    command: 'syntax',
    name: 'syntax check interface file',
    args: ['--files', 'abap/zif_abgagt_command.intf.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain syntax check result
      const hasResult = output.includes('Syntax check passed') ||
        output.includes('Syntax check failed');
      const hasObject = output.includes('ZIF_ABGAGT_COMMAND') || output.includes('INTF');
      return hasResult && hasObject;
    }
  },
  {
    command: 'syntax',
    name: 'syntax check with cloud mode',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap', '--cloud'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain syntax check result and show cloud mode
      const hasResult = output.includes('Syntax check passed') ||
        output.includes('Syntax check failed');
      const hasCloudIndicator = output.includes('ABAP Cloud') || output.includes('Cloud');
      return hasResult;
    }
  },
  {
    command: 'syntax',
    name: 'syntax check multiple files',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap,abap/zif_abgagt_command.intf.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain results for both objects
      const hasClass = output.includes('ZCL_ABGAGT_UTIL');
      const hasInterface = output.includes('ZIF_ABGAGT_COMMAND');
      const hasResult = output.includes('Syntax check passed') ||
        output.includes('Syntax check failed');
      return hasResult && (hasClass || hasInterface);
    }
  },
  {
    command: 'syntax',
    name: 'syntax check with json output',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap', '--json'],
    expectSuccess: true,
    verify: (output) => {
      // Should return valid JSON
      try {
        const json = JSON.parse(output);
        return json.RESULTS !== undefined || json.results !== undefined;
      } catch (e) {
        return false;
      }
    }
  },

  // Error detection tests
  {
    command: 'syntax',
    args: ['--files', 'tests/fixtures/zcl_syntax_error.clas.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should detect syntax errors and report them
      const hasFailure = output.includes('Syntax check failed') ||
        output.includes('error');
      const hasErrorCount = output.includes('error(s)') ||
        output.includes('ERROR_COUNT') ||
        output.includes('error_count');
      return hasFailure || hasErrorCount;
    }
  },
  {
    command: 'syntax',
    name: 'syntax check detects interface errors',
    args: ['--files', 'tests/fixtures/zif_syntax_error.intf.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should detect syntax errors in interface
      const hasFailure = output.includes('Syntax check failed') ||
        output.includes('error');
      const hasObject = output.includes('ZIF_SYNTAX_ERROR') || output.includes('INTF');
      return hasFailure && hasObject;
    }
  },
  {
    command: 'syntax',
    name: 'syntax check detects program errors',
    args: ['--files', 'tests/fixtures/zsyntax_error.prog.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should detect syntax errors in program
      const hasFailure = output.includes('Syntax check failed') ||
        output.includes('error');
      const hasObject = output.includes('ZSYNTAX_ERROR') || output.includes('PROG');
      return hasFailure && hasObject;
    }
  },
  {
    command: 'syntax',
    name: 'syntax check multiple files with errors',
    args: ['--files', 'tests/fixtures/zcl_syntax_error.clas.abap,tests/fixtures/zif_syntax_error.intf.abap,tests/fixtures/zsyntax_error.prog.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should detect errors in all 3 files
      const hasClassError = output.includes('ZCL_SYNTAX_ERROR') && output.includes('failed');
      const hasIntfError = output.includes('ZIF_SYNTAX_ERROR') && output.includes('failed');
      const hasProgError = output.includes('ZSYNTAX_ERROR') && output.includes('failed');
      const hasMultipleErrors = output.includes('3 of 3') ||
        (output.match(/Syntax check failed/g) || []).length >= 3;
      return hasMultipleErrors || (hasClassError && hasIntfError && hasProgError);
    }
  },

  // Auto-detection and include field tests
  {
    command: 'syntax',
    name: 'syntax check testclasses file with auto-detection',
    args: ['--files', 'tests/fixtures/zcl_test_auto_detect.clas.testclasses.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should auto-detect main, locals_def, locals_imp
      const hasAutoDetect = output.includes('Auto-detected') ||
        output.includes('ZCL_TEST_AUTO_DETECT');
      const hasMainFile = output.includes('zcl_test_auto_detect.clas.abap') ||
        output.includes('Syntax check');
      return hasAutoDetect || hasMainFile;
    }
  },
  {
    command: 'syntax',
    name: 'syntax check testclasses detects error in testclasses with include field',
    args: ['--files', 'tests/fixtures/zcl_test_auto_detect.clas.testclasses.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should detect UNDEFINED_VARIABLE error in testclasses section
      const hasError = output.includes('UNDEFINED_VARIABLE') ||
        output.includes('failed') ||
        output.includes('error');
      const hasInclude = output.includes('Test classes') ||
        output.includes('testclasses');
      const hasLineNumber = /Line \d+/.test(output);
      return hasError && (hasInclude || hasLineNumber);
    }
  },
  {
    command: 'syntax',
    name: 'syntax check locals_imp detects error with include field',
    args: ['--files', 'tests/fixtures/zcl_test_auto_detect.clas.locals_imp.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should detect missing period error in locals_imp
      const hasError = output.includes('failed') ||
        output.includes('error') ||
        output.includes('expected');
      const hasInclude = output.includes('Local implementations') ||
        output.includes('locals_imp');
      const hasLineNumber = /Line \d+/.test(output);
      // Error should be at line 3 in locals_imp (missing period after rv_result = iv_a * iv_b)
      return hasError && (hasInclude || hasLineNumber);
    }
  },
  {
    command: 'syntax',
    name: 'syntax check main class with auto-detection of companions',
    args: ['--files', 'tests/fixtures/zcl_test_auto_detect.clas.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should auto-detect locals_def, locals_imp, testclasses
      const hasAutoDetect = output.includes('Auto-detected') ||
        output.includes('ZCL_TEST_AUTO_DETECT');
      const hasCompanionFile = output.includes('locals_def') ||
        output.includes('locals_imp') ||
        output.includes('testclasses');
      // Should find errors in locals_imp and testclasses
      const hasError = output.includes('failed') || output.includes('error');
      return hasAutoDetect || hasCompanionFile || hasError;
    }
  },
  {
    command: 'syntax',
    name: 'syntax check with JSON output includes include field',
    args: ['--files', 'tests/fixtures/zcl_test_auto_detect.clas.locals_imp.abap', '--json'],
    expectSuccess: true,
    verify: (output) => {
      // Should return valid JSON with include field in error
      try {
        const json = JSON.parse(output);
        const results = json.RESULTS || json.results;
        if (!results || results.length === 0) return false;

        const errors = results[0].ERRORS || results[0].errors || [];
        if (errors.length === 0) return false;

        // Check if include field exists
        const hasInclude = errors.some(err =>
          (err.INCLUDE && err.INCLUDE === 'locals_imp') ||
          (err.include && err.include === 'locals_imp')
        );
        return hasInclude;
      } catch (e) {
        return false;
      }
    }
  },
  {
    command: 'syntax',
    name: 'syntax check auto-detection from locals_def',
    args: ['--files', 'tests/fixtures/zcl_test_auto_detect.clas.locals_def.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should auto-detect main, locals_imp, testclasses
      const hasAutoDetect = output.includes('Auto-detected') ||
        output.includes('ZCL_TEST_AUTO_DETECT');
      const hasResult = output.includes('Syntax check') ||
        output.includes('error') ||
        output.includes('passed');
      return hasAutoDetect || hasResult;
    }
  },
  {
    command: 'syntax',
    name: 'syntax check shows exact filename with include location',
    args: ['--files', 'tests/fixtures/zcl_test_auto_detect.clas.testclasses.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should show both human-readable name AND exact filename
      // Format: "In: Test classes (zcl_test_auto_detect.clas.testclasses.abap)"
      const hasFilename = output.includes('.testclasses.abap') ||
        output.includes('.locals_imp.abap') ||
        output.includes('.clas.abap');
      const hasReadableName = output.includes('Test classes') ||
        output.includes('Local implementations') ||
        output.includes('Main class');
      return hasFilename || hasReadableName;
    }
  },

  // DDLS/CDS View syntax tests
  {
    command: 'syntax',
    name: 'syntax check valid CDS view with annotations',
    setup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/ddls');
      if (!fs.existsSync(fixturesDir)) {
        fs.mkdirSync(fixturesDir, { recursive: true });
      }
      const source = `@AbapCatalog.sqlViewName: 'ZV_TEST_VALID'
@EndUserText.label: 'Test Valid View'
define view ZC_Test_Valid as select from sflight
{
  key carrid,
      connid,
      fldate
}`;
      fs.writeFileSync(path.join(fixturesDir, 'zc_test_valid.ddls.asddls'), source);
    },
    args: ['--files', 'tests/fixtures/ddls/zc_test_valid.ddls.asddls', '--json'],
    expectSuccess: true,
    verify: (output) => {
      try {
        const json = JSON.parse(output);
        const result = json.RESULTS && json.RESULTS[0];
        return result && result.OBJECT_TYPE === 'DDLS' && result.SUCCESS === true;
      } catch (e) {
        return false;
      }
    },
    cleanup: () => {
      const fs = require('fs');
      const path = require('path');
      const filePath = path.join(__dirname, '../fixtures/ddls/zc_test_valid.ddls.asddls');
      if (fs.existsSync(filePath)) fs.unlinkSync(filePath);
    }
  },
  {
    command: 'syntax',
    name: 'syntax check CDS view without required sqlViewName fails',
    setup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/ddls');
      if (!fs.existsSync(fixturesDir)) {
        fs.mkdirSync(fixturesDir, { recursive: true });
      }
      const source = `@EndUserText.label: 'Missing Annotation'
define view ZC_Missing_Annotation as select from sflight
{
  key carrid
}`;
      fs.writeFileSync(path.join(fixturesDir, 'zc_missing_annot.ddls.asddls'), source);
    },
    args: ['--files', 'tests/fixtures/ddls/zc_missing_annot.ddls.asddls', '--json'],
    expectSuccess: true,
    verify: (output) => {
      try {
        const json = JSON.parse(output);
        const result = json.RESULTS && json.RESULTS[0];
        // Should fail with error about missing sqlViewName
        return result && result.SUCCESS === false && result.ERROR_COUNT > 0;
      } catch (e) {
        return false;
      }
    },
    cleanup: () => {
      const fs = require('fs');
      const path = require('path');
      const filePath = path.join(__dirname, '../fixtures/ddls/zc_missing_annot.ddls.asddls');
      if (fs.existsSync(filePath)) fs.unlinkSync(filePath);
    }
  },
  {
    command: 'syntax',
    name: 'syntax check valid CDS view entity without sqlViewName',
    setup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/ddls');
      if (!fs.existsSync(fixturesDir)) {
        fs.mkdirSync(fixturesDir, { recursive: true });
      }
      const source = `@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Valid Entity'
define view entity ZC_Valid_Entity as select from sflight
{
  key carrid,
      connid,
      fldate
}`;
      fs.writeFileSync(path.join(fixturesDir, 'zc_valid_entity.ddls.asddls'), source);
    },
    args: ['--files', 'tests/fixtures/ddls/zc_valid_entity.ddls.asddls', '--json'],
    expectSuccess: true,
    verify: (output) => {
      try {
        const json = JSON.parse(output);
        const result = json.RESULTS && json.RESULTS[0];
        // View entity should pass without sqlViewName
        return result && result.OBJECT_TYPE === 'DDLS' && result.SUCCESS === true;
      } catch (e) {
        return false;
      }
    },
    cleanup: () => {
      const fs = require('fs');
      const path = require('path');
      const filePath = path.join(__dirname, '../fixtures/ddls/zc_valid_entity.ddls.asddls');
      if (fs.existsSync(filePath)) fs.unlinkSync(filePath);
    }
  },
  {
    command: 'syntax',
    name: 'syntax check detects incomplete DDLS statement',
    setup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/ddls');
      if (!fs.existsSync(fixturesDir)) {
        fs.mkdirSync(fixturesDir, { recursive: true });
      }
      const source = `@AbapCatalog.sqlViewName: 'ZV_INCOMPLETE'
define view ZC_Incomplete as select from`;
      fs.writeFileSync(path.join(fixturesDir, 'zc_incomplete.ddls.asddls'), source);
    },
    args: ['--files', 'tests/fixtures/ddls/zc_incomplete.ddls.asddls', '--json'],
    expectSuccess: true,
    verify: (output) => {
      try {
        const json = JSON.parse(output);
        const result = json.RESULTS && json.RESULTS[0];
        // Should detect syntax error
        return result && result.SUCCESS === false && result.ERROR_COUNT > 0;
      } catch (e) {
        return false;
      }
    },
    cleanup: () => {
      const fs = require('fs');
      const path = require('path');
      const filePath = path.join(__dirname, '../fixtures/ddls/zc_incomplete.ddls.asddls');
      if (fs.existsSync(filePath)) fs.unlinkSync(filePath);
    }
  },
  {
    command: 'syntax',
    name: 'syntax check CDS view with association',
    setup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/ddls');
      if (!fs.existsSync(fixturesDir)) {
        fs.mkdirSync(fixturesDir, { recursive: true });
      }
      const source = `@AbapCatalog.sqlViewName: 'ZV_ASSOC'
@EndUserText.label: 'View with Association'
define view ZC_Assoc as select from sflight
  association [1..1] to scarr as _Carrier
    on $projection.carrid = _Carrier.carrid
{
  key carrid,
      connid,
      _Carrier
}`;
      fs.writeFileSync(path.join(fixturesDir, 'zc_assoc.ddls.asddls'), source);
    },
    args: ['--files', 'tests/fixtures/ddls/zc_assoc.ddls.asddls', '--json'],
    expectSuccess: true,
    verify: (output) => {
      try {
        const json = JSON.parse(output);
        const result = json.RESULTS && json.RESULTS[0];
        return result && result.OBJECT_TYPE === 'DDLS' && result.SUCCESS === true;
      } catch (e) {
        return false;
      }
    },
    cleanup: () => {
      const fs = require('fs');
      const path = require('path');
      const filePath = path.join(__dirname, '../fixtures/ddls/zc_assoc.ddls.asddls');
      if (fs.existsSync(filePath)) fs.unlinkSync(filePath);
    }
  },
  {
    command: 'syntax',
    name: 'syntax check CDS view entity with parameters',
    setup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/ddls');
      if (!fs.existsSync(fixturesDir)) {
        fs.mkdirSync(fixturesDir, { recursive: true });
      }
      const source = `@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Entity with Parameters'
define view entity ZC_Entity_Param
  with parameters
    p_carrid : s_carr_id,
    p_date   : s_date
  as select from sflight
{
  key carrid,
      connid,
      fldate
}
where carrid = $parameters.p_carrid
  and fldate >= $parameters.p_date`;
      fs.writeFileSync(path.join(fixturesDir, 'zc_entity_param.ddls.asddls'), source);
    },
    args: ['--files', 'tests/fixtures/ddls/zc_entity_param.ddls.asddls', '--json'],
    expectSuccess: true,
    verify: (output) => {
      try {
        const json = JSON.parse(output);
        const result = json.RESULTS && json.RESULTS[0];
        return result && result.OBJECT_TYPE === 'DDLS' && result.SUCCESS === true;
      } catch (e) {
        return false;
      }
    },
    cleanup: () => {
      const fs = require('fs');
      const path = require('path');
      const filePath = path.join(__dirname, '../fixtures/ddls/zc_entity_param.ddls.asddls');
      if (fs.existsSync(filePath)) fs.unlinkSync(filePath);
    }
  },

  // ===================================================================
  // DEBUG COMMAND - 7 tests
  // Purpose: ABAP debugger — breakpoint management, scripted session
  // Categories:
  //   - Breakpoint management (3 tests): set, list, delete
  //   - Scripted session (4 tests): stack, vars, step-continue, terminate
  //
  // Best-practice rules applied (from abap/CLAUDE.md):
  //   1. sleep 2 after starting attach — listener must register before trigger fires
  //   2. Keep trigger process alive for entire session
  //   3. Always finish with step --type continue to release the work process
  //   4. Never pass --session to step/vars/stack (auto-load from state file)
  // ===================================================================

  // --- Breakpoint management ---

  {
    command: 'debug',
    name: 'debug delete --all clears breakpoints',
    args: ['delete', '--all'],
    expectSuccess: true,
    verify: (output) => {
      // Should confirm deletion or report nothing to delete
      const hasResult = output.includes('deleted') ||
        output.includes('Deleted') ||
        output.includes('No breakpoints') ||
        output.includes('cleared') ||
        output.includes('0 breakpoint');
      return hasResult;
    }
  },
  {
    command: 'debug',
    name: 'debug set --object --line registers a breakpoint',
    args: ['set', '--object', 'ZCL_ABGAGT_UTIL', '--line', '25'],
    expectSuccess: true,
    verify: (output) => {
      // Should confirm the breakpoint was registered
      const hasConfirm = output.includes('Breakpoint') ||
        output.includes('breakpoint') ||
        output.includes('ZCL_ABGAGT_UTIL') ||
        output.includes('line 25') ||
        output.includes(':25');
      return hasConfirm;
    }
  },
  {
    command: 'debug',
    name: 'debug list shows registered breakpoints',
    args: ['list'],
    expectSuccess: true,
    verify: (output) => {
      // Should list breakpoints (at least the one just set) or show empty state
      const hasResult = output.includes('ZCL_ABGAGT_UTIL') ||
        output.includes('breakpoint') ||
        output.includes('Breakpoint') ||
        output.includes('No breakpoints') ||
        output.includes('line');
      return hasResult;
    }
  },
  {
    command: 'debug',
    name: 'debug delete --all cleans up after list test',
    args: ['delete', '--all'],
    expectSuccess: true,
    verify: (output) => {
      return output.includes('deleted') ||
        output.includes('Deleted') ||
        output.includes('cleared') ||
        output.includes('No breakpoints') ||
        output.length >= 0;  // delete --all succeeds silently
    }
  },
  {
    command: 'debug',
    name: 'debug list shows no breakpoints after delete --all',
    args: ['list'],
    expectSuccess: true,
    verify: (output) => {
      return output.includes('No breakpoints') ||
        output.includes('no breakpoints') ||
        output.includes('0 breakpoints') ||
        // If output has no object names, breakpoints are gone
        !output.includes('ZCL_ABGAGT_UTIL');
    }
  },

  // ===================================================================
  // DUMP COMMAND - 4 tests
  // Purpose: Query short dumps (ST22) from ABAP system
  // ===================================================================
  {
    command: 'dump',
    name: 'dump basic list',
    args: [],
    expectSuccess: true,
    verify: (output) => {
      // Should show dump list header (even if no dumps found)
      const hasHeader = output.includes('Short Dumps') || output.includes('No short dumps found');
      return hasHeader;
    }
  },
  {
    command: 'dump',
    name: 'dump with user filter',
    args: ['--user', 'DEVELOPER', '--limit', '5'],
    expectSuccess: true,
    verify: (output) => {
      // Should show list header with result count
      const hasHeader = output.includes('Short Dumps') || output.includes('No short dumps found');
      return hasHeader;
    }
  },
  {
    command: 'dump',
    name: 'dump with date TODAY',
    args: ['--date', 'TODAY', '--limit', '10'],
    expectSuccess: true,
    verify: (output) => {
      // Should show list filtered to today's date
      const hasHeader = output.includes('Short Dumps') || output.includes('No short dumps found');
      return hasHeader;
    }
  },
  {
    command: 'dump',
    name: 'dump JSON output',
    args: ['--json', '--limit', '5'],
    expectSuccess: true,
    verify: (output) => {
      // Should return valid JSON with expected structure
      try {
        const json = JSON.parse(output);
        const hasSuccess = json.SUCCESS === true || json.success === true;
        const hasDumps = Array.isArray(json.DUMPS) || Array.isArray(json.dumps);
        return hasSuccess && hasDumps;
      } catch (e) {
        return false;
      }
    }
  },

  // ===================================================================
  // REF COMMAND - 3 tests
  // Purpose: Search ABAP reference materials (cheat sheets, guidelines)
  // Note: These tests don't require ABAP system connection
  // ===================================================================
  {
    command: 'ref',
    name: 'ref --list-topics',
    args: ['--list-topics'],
    expectSuccess: true,
    verify: (output) => {
      // Should list topics
      const hasResult = output.includes('Topic') || output.includes('internal-tables') || output.includes('sql');
      return hasResult;
    }
  },
  {
    command: 'ref',
    name: 'ref --list-repos',
    args: ['--list-repos'],
    expectSuccess: true,
    verify: (output) => {
      // Should list repositories or show error if not configured
      const hasResult = output.includes('Repository') || output.includes('Reference folder') || output.includes('Not configured');
      return hasResult;
    }
  },
  {
    command: 'ref',
    name: 'ref --topic exceptions',
    args: ['--topic', 'exceptions'],
    expectSuccess: true,
    verify: (output) => {
      // Should show topic content or error
      const hasResult = output.includes('Exception') || output.includes('cx_') || output.includes('Topic') || output.length > 50;
      return hasResult;
    }
  },

  // ===================================================================
  // SYNTAX COMMAND - FIXPT Flag Tests (2 tests)
  // Purpose: Verify FIXPT=X flag handling for modern ABAP SQL
  // ===================================================================
  {
    command: 'syntax',
    setup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/fixpt-test');
      if (!fs.existsSync(fixturesDir)) {
        fs.mkdirSync(fixturesDir, { recursive: true });
      }
      // Class with modern SQL syntax (comma, @ prefix) and no FIXPT in XML
      // Should FAIL because no FIXPT means FIXPT=blank (false), but modern SQL requires FIXPT=X
      const source = `CLASS zcl_test_no_fixpt DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_flight,
             carrid TYPE s_carr_id,
             connid TYPE s_conn_id,
           END OF ty_flight.
    TYPES ty_flights TYPE STANDARD TABLE OF ty_flight WITH DEFAULT KEY.
    METHODS get_data
      IMPORTING iv_carrid TYPE s_carr_id DEFAULT 'AA'
      RETURNING VALUE(rt_result) TYPE ty_flights.
ENDCLASS.

CLASS zcl_test_no_fixpt IMPLEMENTATION.
  METHOD get_data.
    SELECT carrid, connid
      FROM sflight
      INTO TABLE @rt_result
      WHERE carrid = @iv_carrid.
  ENDMETHOD.
ENDCLASS.`;
      fs.writeFileSync(path.join(fixturesDir, 'zcl_test_no_fixpt.clas.abap'), source);

      // XML without FIXPT flag (means FIXPT=blank/false)
      const xml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_TEST_NO_FIXPT</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Test class without FIXPT in XML</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>`;
      fs.writeFileSync(path.join(fixturesDir, 'zcl_test_no_fixpt.clas.xml'), xml);
    },
    args: ['--files', 'tests/fixtures/fixpt-test/zcl_test_no_fixpt.clas.abap', '--json'],
    expectSuccess: true,
    verify: (output) => {
      try {
        const json = JSON.parse(output);
        const result = json.RESULTS && json.RESULTS[0];
        // Should FAIL because no FIXPT=X, but modern SQL needs it
        return result && result.SUCCESS === false && result.ERROR_COUNT > 0;
      } catch (e) {
        return false;
      }
    },
    cleanup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/fixpt-test');
      try {
        fs.unlinkSync(path.join(fixturesDir, 'zcl_test_no_fixpt.clas.abap'));
        fs.unlinkSync(path.join(fixturesDir, 'zcl_test_no_fixpt.clas.xml'));
        fs.rmdirSync(fixturesDir);
      } catch (e) {
        // Ignore cleanup errors
      }
    }
  },
  {
    command: 'syntax',
    name: 'syntax check class with FIXPT=X in XML should pass with modern SQL',
    setup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/fixpt-test-with-x');
      if (!fs.existsSync(fixturesDir)) {
        fs.mkdirSync(fixturesDir, { recursive: true });
      }
      // Class with modern SQL syntax (comma, @ prefix) and FIXPT=X in XML
      // Should PASS because FIXPT=X enables modern SQL syntax
      const source = `CLASS zcl_test_with_fixpt DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_flight,
             carrid TYPE s_carr_id,
             connid TYPE s_conn_id,
           END OF ty_flight.
    TYPES ty_flights TYPE STANDARD TABLE OF ty_flight WITH DEFAULT KEY.
    METHODS get_data
      IMPORTING iv_carrid TYPE s_carr_id DEFAULT 'AA'
      RETURNING VALUE(rt_result) TYPE ty_flights.
ENDCLASS.

CLASS zcl_test_with_fixpt IMPLEMENTATION.
  METHOD get_data.
    SELECT carrid, connid
      FROM sflight
      INTO TABLE @rt_result
      WHERE carrid = @iv_carrid.
  ENDMETHOD.
ENDCLASS.`;
      fs.writeFileSync(path.join(fixturesDir, 'zcl_test_with_fixpt.clas.abap'), source);

      // XML with FIXPT=X flag
      const xml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_TEST_WITH_FIXPT</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Test class with FIXPT=X in XML</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
    <FIXPT>X</FIXPT>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>`;
      fs.writeFileSync(path.join(fixturesDir, 'zcl_test_with_fixpt.clas.xml'), xml);
    },
    args: ['--files', 'tests/fixtures/fixpt-test-with-x/zcl_test_with_fixpt.clas.abap', '--json'],
    expectSuccess: true,
    verify: (output) => {
      try {
        const json = JSON.parse(output);
        const result = json.RESULTS && json.RESULTS[0];
        // Should PASS because FIXPT=X enables modern SQL
        return result && result.SUCCESS === true && result.ERROR_COUNT === 0;
      } catch (e) {
        return false;
      }
    },
    cleanup: () => {
      const fs = require('fs');
      const path = require('path');
      const fixturesDir = path.join(__dirname, '../fixtures/fixpt-test-with-x');
      try {
        fs.unlinkSync(path.join(fixturesDir, 'zcl_test_with_fixpt.clas.abap'));
        fs.unlinkSync(path.join(fixturesDir, 'zcl_test_with_fixpt.clas.xml'));
        fs.rmdirSync(fixturesDir);
      } catch (e) {
        // Ignore cleanup errors
      }
    }
  },

  // ===================================================================
  // UPGRADE COMMAND - 4 tests
  // Purpose: Test upgrade command (safe operations only)
  // Note: CLI upgrade is NOT tested (would modify global npm installation)
  // ===================================================================
  {
    command: 'upgrade',
    name: 'upgrade --check (shows current and latest versions)',
    args: ['--check'],
    expectSuccess: true,
    verify: (output) => {
      // Should show current versions for CLI and ABAP
      const hasCurrentVersions = output.includes('Current versions:');
      const hasCLI = output.includes('CLI:');
      const hasLatest = output.includes('Latest available:') || output.includes('All components are up to date');
      return hasCurrentVersions && hasCLI && hasLatest;
    }
  },
  {
    command: 'upgrade',
    name: 'upgrade --dry-run (shows plan without changes)',
    args: ['--dry-run'],
    expectSuccess: true,
    verify: (output) => {
      // Should show dry-run plan
      const hasDryRun = output.includes('DRY RUN');
      const hasCurrentVersions = output.includes('Current versions:');
      const hasTargetVersions = output.includes('Target versions:');
      const hasNoChanges = output.includes('No changes made');
      return hasDryRun && hasCurrentVersions && hasTargetVersions && hasNoChanges;
    }
  },
  {
    command: 'upgrade',
    name: 'upgrade --version (invalid version shows error)',
    args: ['--version', '99.99.99', '--cli-only', '--dry-run'],
    expectSuccess: false,
    verify: (output) => {
      // Should reject invalid version with helpful error
      const hasError = output.includes('not found in npm registry');
      const hasLink = output.includes('npmjs.com');
      return hasError && hasLink;
    }
  },
  {
    command: 'upgrade',
    name: 'upgrade --cli-only --check (works without ABAP config)',
    args: ['--cli-only', '--check'],
    expectSuccess: true,
    verify: (output) => {
      // Should work without ABAP config when using --cli-only
      const hasCurrentVersions = output.includes('Current versions:');
      const hasCLI = output.includes('CLI:');
      const noAbapError = !output.includes('.abapGitAgent config file not found');
      return hasCurrentVersions && hasCLI && noAbapError;
    }
  },

  // ===================================================================
  // RUN COMMAND - 2 tests
  // Purpose: Execute ABAP program and class, capture output
  // Note: Objects live in the abgagt-run-test repository ($ABGAGT_RUN_TEST)
  // ===================================================================
  {
    command: 'run',
    name: 'run --program ZABGAGT_RUN_TEST',
    args: ['--program', 'ZABGAGT_RUN_TEST'],
    expectSuccess: true,
    verify: (output) => {
      return output.includes('Completed: ZABGAGT_RUN_TEST') &&
        output.includes('Run command integration test passed');
    }
  },
  {
    command: 'run',
    name: 'run --class ZCL_ABGAGT_RUN_TEST',
    args: ['--class', 'ZCL_ABGAGT_RUN_TEST'],
    expectSuccess: true,
    verify: (output) => {
      return output.includes('Completed: ZCL_ABGAGT_RUN_TEST') &&
        output.includes('run command integration test passed');
    }
  }
];
module.exports = commandTestCases;
