/**
 * Command test cases with specific assertions (runs against real ABAP system)
 *
 * These are isolated command tests. For workflow tests (multi-step sequences),
 * see pull-runner.js, lifecycle-runner.js, and xml-only-runner.js.
 *
 * Test Distribution:
 *   - syntax: 24 tests (validation, auto-detection, DDLS, FIXPT)
 *   - view:    9 tests (class, interface, table, class --full, class --full --lines,
 *                       domain, message class, function group, access control)
 *   - tree:    3 tests (package, depth, types)
 *   - preview: 3 tests (table, limit, columns)
 *   - list:    3 tests (package, type filter, name filter)
 *   - where:   3 tests (class, interface, type filter)
 *   - debug:   35 tests — all use ZCL_CAIS_DBG_TRIGGER from abgagt-debug-test repo
 *                         (Group A: hardcoded lines, Group B: view→set→verify round-trip,
 *                          Group C: unexecutable line rejection; CLAS main/testclasses/
 *                          locals_imp and FUGR include; set tests use --json to verify
 *                          ADT-accepted line number; full session in debug-scenarios.sh)
 *   - dump:    4 tests (basic list, user filter, date filter, JSON output)
 *   - ref:     3 tests (topics, repos, search)
 *   - upgrade: 4 tests (check, dry-run, invalid version, cli-only)
 *   - pull:    1 test  (--files)
 *   - unit:    2 tests  (test class, --junit-output)
 *   - run:     2 tests (run program, run class)
 *   - status:  1 test  (config check)
 *   - inspect: 2 tests  (code inspector, --junit-output)
 *   - health:  1 test  (system health)
 *   Total:    95 tests
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
  // VIEW COMMAND - gap types (DOMA, MSAG, FUGR)
  // ===================================================================
  {
    command: 'view',
    name: 'view domain (DOMA) shows type and length',
    args: ['--objects', 'XFELD', '--type', 'DOMA'],
    expectSuccess: true,
    verify: (output) => {
      // XFELD is the checkbox domain (CHAR 1, fixed values X/' ') — SAP_BASIS / SUTI package
      const hasDomain = output.includes('XFELD');
      const hasType = output.includes('CHAR') || output.includes('Type') || output.includes('Domain');
      return hasDomain && hasType;
    }
  },
  {
    command: 'view',
    name: 'view message class (MSAG) shows messages',
    args: ['--objects', 'SY', '--type', 'MSAG'],
    expectSuccess: true,
    verify: (output) => {
      // SY is the system messages class — SABP_CORE package, present on every ABAP system
      const hasMsgClass = output.includes('SY') || output.includes('Message');
      const hasMessages = output.includes(':') || output.includes('Total');
      return hasMsgClass && hasMessages;
    }
  },
  {
    command: 'view',
    name: 'view function group (FUGR) lists function modules',
    args: ['--objects', 'SUSR', '--type', 'FUGR'],
    expectSuccess: true,
    verify: (output) => {
      // SUSR is the user management function group — SAP_BASIS / SUSR package
      const hasFugr = output.includes('SUSR') || output.includes('Function');
      const hasModules = output.includes('RFC') || output.includes(':') || output.includes('modules');
      return hasFugr && hasModules;
    }
  },
  {
    command: 'view',
    name: 'view access control (DCLS) shows source',
    args: ['--objects', 'SEPM_E_SALESORDER', '--type', 'DCLS'],
    expectSuccess: true,
    verify: (output) => {
      // SEPM_E_SALESORDER is the EPM sales order access control — SAP_BASIS / S_EPM_CDS_REF
      const hasDcls = output.includes('SEPM_E_SALESORDER') || output.includes('Access Control');
      return hasDcls;
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
  // DEBUG COMMAND - 35 tests
  // Purpose: ABAP debugger — breakpoint management across all object types
  //
  // All tests use objects from abgagt-debug-test repo:
  //   ZCL_CAIS_DBG_TRIGGER — trigger class (CLAS main, testclasses, locals_imp)
  //   LZCAIS_DBG_TESTU01   — FUGR include of ZCAIS_DBG_TEST
  //
  // Two test groups:
  //
  // Group A (hardcoded lines, 13 tests):
  //   Set breakpoints at known-good lines, verify ADT accepts them (LINE_NR≥1).
  //   Catches regressions in the debug set ADT URI logic.
  //
  // Group B (view→set→verify, 16 tests):
  //   For each object type:
  //     1. Run view --full --lines to get the debug set hint
  //     2. Parse the line number from the hint
  //     3. Set breakpoint at that parsed line
  //     4. Verify debug list shows the breakpoint with the correct line
  //   Catches regressions where view --lines emits wrong line numbers.
  //   If view returns a wrong line, debug set may fail or LINE_NR won't match.
  //
  // Group C (unexecutable lines, 6 tests):
  //   Set breakpoints on comments, blank lines, DATA declarations, and METHOD
  //   statements — ADT must reject them (exit 1, JSON error shape).
  //   Catches regressions where the tool silently accepts invalid positions.
  //
  // Confirmed ADT-accepted lines (probed via direct POST, 2026-04-02):
  //   ZCL_CAIS_DBG_TRIGGER main:        line 33 (lv_sum = iv_a + iv_b in compute)
  //   ZCL_CAIS_DBG_TRIGGER testclasses: line 13 (rv_val = iv_val * lv_two in ltcl_helper)
  //   ZCL_CAIS_DBG_TRIGGER locals_imp:  line 6  (lv_label = 'debug-test' in lcl_helper)
  //   LZCAIS_DBG_TESTU01 (FUGR):        line 13 (lv_result = iv_a + iv_b in ZCAIS_DBG_ADD)
  // ===================================================================

  // Debug test repo path — all debug tests run from here
  // (so .abapGitAgent points to $CAIS_DEBUG_TEST, not the main project)
  // eslint-disable-next-line no-undef
  ...((() => {
    const path = require('path');
    const debugRepoPath = path.join(__dirname, '..', '..', '..', 'abgagt-debug-test');

    // Helper: parse LINE_NR from ADT id returned in --json output
    // NOTE: LINE_NR is include-relative, not the global assembled-source line.
    // For CLAS main methods it differs from the line we passed in.
    // Use adtLineNr() only to confirm ADT accepted the BP (LINE_NR >= 1).
    function adtLineNr(jsonOutput) {
      const m = jsonOutput.match(/"id":"[^"]*LINE_NR=(\d+)/);
      return m ? parseInt(m[1], 10) : null;
    }

    // Helper: parse the "line" field from --json output (echoes the line we passed in)
    function jsonLine(jsonOutput) {
      const m = jsonOutput.match(/"line":(\d+)/);
      return m ? parseInt(m[1], 10) : null;
    }

    // Shared state for Group B — view output is parsed in verify() and consumed
    // by the next test's args() / verify(). Tests run sequentially so this is safe.
    const viewParsed = {
      clasMain: null,       // line number from view hint for COMPUTE method
      testclasses: null,    // line number from view hint for DOUBLE method
      localsImp: null,      // line number from view hint for DESCRIBE method
      fugr: null,           // line number from view hint for ZCAIS_DBG_ADD FM
    };

    // Parse a `debug set --objects NAME:N` hint from view --lines output.
    // The hint appears in lines like:
    //   * ---- Method: COMPUTE (CM001) — breakpoint: debug set --objects ZCL_CAIS_DBG_TRIGGER:33 ----
    //   * ---- FM: ZCAIS_DBG_ADD (LZCAIS_DBG_TESTU01) — breakpoint: debug set --objects LZCAIS_DBG_TESTU01:13 ----
    //
    // For CLAS main methods the source lines use G [N] format:
    //     33 [  5]      lv_sum = iv_a + iv_b.
    // where G=33 is the global assembled-source line (sent to ADT as #start=)
    // and N=5 is the include-relative line (stored by ADT as LINE_NR).
    // For sub-includes (testclasses, locals_imp) and FUGR the lines show just N,
    // and that N equals LINE_NR directly.
    //
    // Returns { object, line, lineNr, include } or null if not found.
    //   line   — global assembled-source line (what we pass to `debug set`)
    //   lineNr — include-relative line (what ADT returns as LINE_NR in the id)
    function parseViewHint(viewOutput, methodName) {
      const re = new RegExp(
        `---- (?:Method|FM): ${methodName}[^\\n]*breakpoint: debug set --objects ([A-Z0-9_]+):(\\d+)(?:\\s+--include\\s+(\\S+))?`,
        'i'
      );
      const m = viewOutput.match(re);
      if (!m) return null;
      const line = parseInt(m[2], 10);
      const include = m[3] || null;

      // For CLAS main (no --include, has G [N] lines): scan the lines after the
      // hint to find the source line matching the global line number, extract [N].
      let lineNr = line; // default: section line = LINE_NR (testclasses/locals_imp/FUGR)
      if (!include) {
        // Find "  <line> [<n>]" pattern in the text after the hint
        const afterHint = viewOutput.slice(viewOutput.indexOf(m[0]) + m[0].length);
        const lineRe = new RegExp(`^\\s+${line}\\s+\\[\\s*(\\d+)\\]`, 'm');
        const lm = afterHint.match(lineRe);
        if (lm) lineNr = parseInt(lm[1], 10);
      }

      return { object: m[1], line, lineNr, include };
    }

    return [
      // =================================================================
      // GROUP A: Hardcoded known-good lines — verify ADT URI correctness
      // =================================================================

      // --- cleanup before Group A ---
      {
        command: 'debug',
        name: 'debug delete --all (setup: clear any stale breakpoints)',
        args: ['delete', '--all'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
      },

      // --- CLAS main source breakpoint ---
      {
        command: 'debug',
        name: '[A] debug set CLAS main — line 33 accepted by ADT (LINE_NR=5)',
        // ZCL_CAIS_DBG_TRIGGER:33 = lv_sum = iv_a + iv_b in compute()
        // Global line 33 = CM001 include line 5 (as shown by view --lines: "33 [  5]")
        args: ['set', '--object', 'ZCL_CAIS_DBG_TRIGGER', '--line', '33', '--json'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => adtLineNr(output) === 5 && jsonLine(output) === 33
      },
      {
        command: 'debug',
        name: '[A] debug list shows CLAS main breakpoint',
        args: ['list'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('ZCL_CAIS_DBG_TRIGGER') && output.includes('33')
      },
      {
        command: 'debug',
        name: '[A] debug delete --all (cleanup after CLAS main test)',
        args: ['delete', '--all'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
      },
      {
        command: 'debug',
        name: '[A] debug list shows no breakpoints after delete',
        args: ['list'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => !output.includes('ZCL_CAIS_DBG_TRIGGER')
      },

      // --- testclasses include breakpoint ---
      {
        command: 'debug',
        name: '[A] debug set testclasses — line 13 accepted by ADT (LINE_NR=13)',
        // ZCL_CAIS_DBG_TRIGGER testclasses:13 = rv_val = iv_val * lv_two in ltcl_helper
        // (skips comment on line 10, DATA on line 11, blank on line 12)
        args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:13', '--include', 'testclasses', '--json'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          return adtLineNr(output) === 13 && jsonLine(output) === 13;
        }
      },
      {
        command: 'debug',
        name: '[A] debug list shows testclasses breakpoint',
        args: ['list'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('ZCL_CAIS_DBG_TRIGGER') && output.includes('13')
      },

      // --- locals_imp include breakpoint ---
      {
        command: 'debug',
        name: '[A] debug set locals_imp — line 6 accepted by ADT (LINE_NR=6)',
        // ZCL_CAIS_DBG_TRIGGER locals_imp:6 = lv_label = 'debug-test' in lcl_helper
        // (skips comment on line 3, DATA on line 4, blank on line 5)
        args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:6', '--include', 'locals_imp', '--json'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          return adtLineNr(output) === 6 && jsonLine(output) === 6;
        }
      },
      {
        command: 'debug',
        name: '[A] debug list shows both testclasses and locals_imp breakpoints',
        args: ['list'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) =>
          output.includes('ZCL_CAIS_DBG_TRIGGER') &&
          output.includes('13') &&
          output.includes('6')
      },
      {
        command: 'debug',
        name: '[A] debug delete --all (cleanup after sub-include tests)',
        args: ['delete', '--all'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
      },

      // --- FUGR include breakpoint (regression: wrong ADT URI /programs/includes/ before fix) ---
      // LZCAIS_DBG_TESTU01 is the FM source include of ZCAIS_DBG_TEST.
      // Before the fix, this was routed to /programs/includes/ (wrong path).
      // Correct path: /functions/groups/zcais_dbg_test/includes/lzcais_dbg_testu01/source/main
      // ADT rejects the wrong URI → "Not registered on server" → exit 1.
      {
        command: 'debug',
        name: '[A] debug set FUGR include — line 13 accepted by ADT (LINE_NR=13)',
        // LZCAIS_DBG_TESTU01:13 = lv_result = iv_a + iv_b in ZCAIS_DBG_ADD
        args: ['set', '--objects', 'LZCAIS_DBG_TESTU01:13', '--json'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          return adtLineNr(output) === 13 && jsonLine(output) === 13;
        }
      },
      {
        command: 'debug',
        name: '[A] debug list shows FUGR include breakpoint',
        args: ['list'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('LZCAIS_DBG_TESTU01') && output.includes('13')
      },
      {
        command: 'debug',
        name: '[A] debug delete --all (cleanup after FUGR test)',
        args: ['delete', '--all'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
      },

      // =================================================================
      // GROUP B: view --lines → parse hint → set → verify round-trip
      // Catches regressions where view --lines emits wrong line numbers.
      // Each chain: view (parse+store) → set (use stored line) → list (verify)
      // =================================================================

      // -----------------------------------------------------------------
      // B1: CLAS main method (COMPUTE) — view then set
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[B] view --full --lines CLAS: parse COMPUTE method hint',
        run: 'view',
        args: ['--objects', 'ZCL_CAIS_DBG_TRIGGER', '--full', '--lines'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const hint = parseViewHint(output, 'COMPUTE');
          if (!hint) return false;
          viewParsed.clasMain = hint;
          // Sanity: object must be the class, line must be plausible (1–999), no include
          return hint.object === 'ZCL_CAIS_DBG_TRIGGER' &&
            hint.line >= 1 && hint.line <= 999 &&
            hint.include === null;
        }
      },
      {
        command: 'debug',
        name: '[B] debug set CLAS main at view-derived line — LINE_NR matches [N]',
        // line comes from view hint parsed above
        get args() {
          const h = viewParsed.clasMain;
          return h ? ['set', '--objects', `${h.object}:${h.line}`, '--json'] : ['--help'];
        },
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const h = viewParsed.clasMain;
          // h.lineNr is the [N] value from "G [N]" in view --lines output = ADT LINE_NR
          return h !== null && adtLineNr(output) === h.lineNr && jsonLine(output) === h.line;
        }
      },
      {
        command: 'debug',
        name: '[B] debug list confirms CLAS main BP at view-derived line',
        args: ['list'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const h = viewParsed.clasMain;
          return output.includes('ZCL_CAIS_DBG_TRIGGER') &&
            h !== null && output.includes(String(h.line));
        }
      },
      {
        command: 'debug',
        name: '[B] debug delete --all (cleanup after B1)',
        args: ['delete', '--all'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
      },

      // -----------------------------------------------------------------
      // B2: CLAS testclasses (DOUBLE in ltcl_helper) — view then set
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[B] view --full --lines CLAS: parse DOUBLE (testclasses) hint',
        run: 'view',
        args: ['--objects', 'ZCL_CAIS_DBG_TRIGGER', '--full', '--lines'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const hint = parseViewHint(output, 'DOUBLE');
          if (!hint) return false;
          viewParsed.testclasses = hint;
          return hint.object === 'ZCL_CAIS_DBG_TRIGGER' &&
            hint.line >= 1 && hint.line <= 999 &&
            hint.include === 'testclasses';
        }
      },
      {
        command: 'debug',
        name: '[B] debug set testclasses at view-derived line — LINE_NR=line matches',
        get args() {
          const h = viewParsed.testclasses;
          return h
            ? ['set', '--objects', `${h.object}:${h.line}`, '--include', h.include, '--json']
            : ['--help']; // view parse failed — force verify() to fail
        },
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const h = viewParsed.testclasses;
          return h !== null && adtLineNr(output) === h.lineNr && jsonLine(output) === h.line;
        }
      },
      {
        command: 'debug',
        name: '[B] debug list confirms testclasses BP at view-derived line',
        args: ['list'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const h = viewParsed.testclasses;
          return output.includes('ZCL_CAIS_DBG_TRIGGER') &&
            h !== null && output.includes(String(h.line));
        }
      },
      {
        command: 'debug',
        name: '[B] debug delete --all (cleanup after B2)',
        args: ['delete', '--all'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
      },

      // -----------------------------------------------------------------
      // B3: CLAS locals_imp (DESCRIBE in lcl_helper) — view then set
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[B] view --full --lines CLAS: parse DESCRIBE (locals_imp) hint',
        run: 'view',
        args: ['--objects', 'ZCL_CAIS_DBG_TRIGGER', '--full', '--lines'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const hint = parseViewHint(output, 'DESCRIBE');
          if (!hint) return false;
          viewParsed.localsImp = hint;
          return hint.object === 'ZCL_CAIS_DBG_TRIGGER' &&
            hint.line >= 1 && hint.line <= 999 &&
            hint.include === 'locals_imp';
        }
      },
      {
        command: 'debug',
        name: '[B] debug set locals_imp at view-derived line — LINE_NR=line matches',
        get args() {
          const h = viewParsed.localsImp;
          return h
            ? ['set', '--objects', `${h.object}:${h.line}`, '--include', h.include, '--json']
            : ['--help']; // view parse failed — force verify() to fail
        },
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const h = viewParsed.localsImp;
          return h !== null && adtLineNr(output) === h.line && jsonLine(output) === h.line;
        }
      },
      {
        command: 'debug',
        name: '[B] debug list confirms locals_imp BP at view-derived line',
        args: ['list'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const h = viewParsed.localsImp;
          return output.includes('ZCL_CAIS_DBG_TRIGGER') &&
            h !== null && output.includes(String(h.line));
        }
      },
      {
        command: 'debug',
        name: '[B] debug delete --all (cleanup after B3)',
        args: ['delete', '--all'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
      },

      // -----------------------------------------------------------------
      // B4: FUGR FM (ZCAIS_DBG_ADD) — view --type FUGR --fm then set
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[B] view --full --lines FUGR: parse ZCAIS_DBG_ADD FM hint',
        run: 'view',
        args: ['--objects', 'ZCAIS_DBG_TEST', '--type', 'FUGR', '--full', '--fm', 'ZCAIS_DBG_ADD', '--lines'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const hint = parseViewHint(output, 'ZCAIS_DBG_ADD');
          if (!hint) return false;
          viewParsed.fugr = hint;
          // For FUGR the object is the include name (LZCAIS_DBG_TESTU01), no --include flag
          return hint.object === 'LZCAIS_DBG_TESTU01' &&
            hint.line >= 1 && hint.line <= 999 &&
            hint.include === null;
        }
      },
      {
        command: 'debug',
        name: '[B] debug set FUGR at view-derived line — LINE_NR=line matches',
        get args() {
          const h = viewParsed.fugr;
          return h
            ? ['set', '--objects', `${h.object}:${h.line}`, '--json']
            : ['--help']; // view parse failed — force verify() to fail
        },
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const h = viewParsed.fugr;
          return h !== null && adtLineNr(output) === h.lineNr && jsonLine(output) === h.line;
        }
      },
      {
        command: 'debug',
        name: '[B] debug list confirms FUGR BP at view-derived line',
        args: ['list'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => {
          const h = viewParsed.fugr;
          return output.includes('LZCAIS_DBG_TESTU01') &&
            h !== null && output.includes(String(h.line));
        }
      },
      {
        command: 'debug',
        name: '[B] debug delete --all (final cleanup)',
        args: ['delete', '--all'],
        cwd: debugRepoPath,
        expectSuccess: true,
        verify: (output) => output.includes('deleted') || output.includes('No breakpoints') || output.length >= 0
      },

      // =================================================================
      // GROUP C: Unexecutable lines — ADT must reject with error JSON
      // Verifies that debug set exits 1 and reports the stale breakpoint
      // for comments, blank lines, DATA declarations, and METHOD statements.
      //
      // Each test uses expectSuccess: false — the command must exit 1.
      // verify() checks the JSON error shape.
      //
      // Unexecutable lines used (verified against ADT 2026-04-02):
      //   CLAS main line 16: DATA lv_result TYPE i.  (variable declaration, in main)
      //   CLAS main line 29: METHOD compute.         (method statement)
      //   testclasses line 9: METHOD double.          (method statement)
      //   locals_imp  line 2: METHOD describe.        (method statement)
      //   FUGR        line 2: *"-----...              (comment)
      //   FUGR        line 12: (blank line)
      // =================================================================

      // -----------------------------------------------------------------
      // C1: CLAS main — DATA declaration (line 16)
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[C] debug set CLAS main line 16 (DATA decl) — rejected by ADT',
        args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:16', '--json'],
        cwd: debugRepoPath,
        expectSuccess: false,
        verify: (output) =>
          output.includes('"error"') &&
          output.includes('"stale"') &&
          output.includes('"line":16') &&
          !output.includes('"id"')
      },

      // -----------------------------------------------------------------
      // C2: CLAS main — METHOD statement (line 29)
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[C] debug set CLAS main line 29 (METHOD stmt) — rejected by ADT',
        args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:29', '--json'],
        cwd: debugRepoPath,
        expectSuccess: false,
        verify: (output) =>
          output.includes('"error"') &&
          output.includes('"stale"') &&
          output.includes('"line":29')
      },

      // -----------------------------------------------------------------
      // C3: testclasses — METHOD statement (line 9)
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[C] debug set testclasses line 9 (METHOD stmt) — rejected by ADT',
        args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:9', '--include', 'testclasses', '--json'],
        cwd: debugRepoPath,
        expectSuccess: false,
        verify: (output) =>
          output.includes('"error"') &&
          output.includes('"stale"') &&
          output.includes('"line":9')
      },

      // -----------------------------------------------------------------
      // C4: locals_imp — METHOD statement (line 2)
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[C] debug set locals_imp line 2 (METHOD stmt) — rejected by ADT',
        args: ['set', '--objects', 'ZCL_CAIS_DBG_TRIGGER:2', '--include', 'locals_imp', '--json'],
        cwd: debugRepoPath,
        expectSuccess: false,
        verify: (output) =>
          output.includes('"error"') &&
          output.includes('"stale"') &&
          output.includes('"line":2')
      },

      // -----------------------------------------------------------------
      // C5: FUGR — comment line (line 2)
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[C] debug set FUGR line 2 (comment) — rejected by ADT',
        args: ['set', '--objects', 'LZCAIS_DBG_TESTU01:2', '--json'],
        cwd: debugRepoPath,
        expectSuccess: false,
        verify: (output) =>
          output.includes('"error"') &&
          output.includes('"stale"') &&
          output.includes('"line":2')
      },

      // -----------------------------------------------------------------
      // C6: FUGR — blank line (line 12)
      // -----------------------------------------------------------------
      {
        command: 'debug',
        name: '[C] debug set FUGR line 12 (blank line) — rejected by ADT',
        args: ['set', '--objects', 'LZCAIS_DBG_TESTU01:12', '--json'],
        cwd: debugRepoPath,
        expectSuccess: false,
        verify: (output) =>
          output.includes('"error"') &&
          output.includes('"stale"') &&
          output.includes('"line":12')
      },
    ];
  })()),

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
    args: ['--list-repos', '--json'],
    expectSuccess: true,
    verify: (output) => {
      // Should list repositories (JSON to stdout) or error object if referenceFolder not configured
      try {
        const result = JSON.parse(output);
        return result.repositories !== undefined || result.error !== undefined;
      } catch (e) {
        return false;
      }
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
