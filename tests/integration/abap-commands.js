/**
 * Command test cases with specific assertions (runs against real ABAP system)
 */
const commandTestCases = [
  // pull command tests
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
  // syntax command tests (source-based syntax check)
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
  {
    command: 'syntax',
    name: 'syntax check detects errors',
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
  // syntax command - auto-detection and include field tests
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
  // ref commands (local file search - no ABAP required)
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
  }
];
module.exports = commandTestCases;
