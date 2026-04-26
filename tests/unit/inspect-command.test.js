/**
 * Unit tests for inspect command in CLI
 * Tests the CLI parsing, file handling, and request building
 */

const verifiers = require('../helpers/output-verifiers');

// Mock fs module to fake file existence
jest.mock('fs', () => ({
  existsSync: jest.fn(() => true), // All files exist in tests
  readFileSync: jest.fn(() => 'mock file content'),
  mkdirSync: jest.fn(),
  writeFileSync: jest.fn()
}));

// Mock path module
jest.mock('path', () => ({
  isAbsolute: jest.fn(() => false),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => '/' + args.join('/')),
  basename: jest.fn((p) => p.split('/').pop()),
  dirname: jest.fn((p) => p.split('/').slice(0, -1).join('/') || '.')
}));

// Mock process.exit to prevent tests from actually exiting
const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

describe('Inspect Command - Logic Tests', () => {
  describe('File parsing', () => {
    test('parses class file for inspection', () => {
      const file = 'src/zcl_my_class.clas.abap';
      const baseName = file.split('/').pop();

      expect(baseName).toBe('zcl_my_class.clas.abap');
      expect(baseName.includes('.clas.')).toBe(true);
    });

    test('parses CDS view file', () => {
      const file = 'src/zc_my_view.ddls.asddls';
      const baseName = file.split('/').pop();

      expect(baseName.includes('.ddls.')).toBe(true);
    });

    test('extracts object name from file', () => {
      const file = 'zcl_test.clas.abap';
      const objName = file.split('.')[0].toUpperCase();

      expect(objName).toBe('ZCL_TEST');
    });
  });

  describe('Object type detection', () => {
    function detectObjectType(fileName) {
      if (fileName.includes('.clas.')) return 'CLAS';
      if (fileName.includes('.intf.')) return 'INTF';
      if (fileName.includes('.ddls.')) return 'DDLS';
      if (fileName.includes('.fugr.')) return 'FUGR';
      return null;
    }

    test('detects CLAS type', () => {
      const file = 'zcl_test.clas.abap';
      const objType = detectObjectType(file);

      expect(objType).toBe('CLAS');
    });

    test('detects INTF type', () => {
      const file = 'zif_test.intf.abap';
      const objType = detectObjectType(file);

      expect(objType).toBe('INTF');
    });

    test('detects DDLS type', () => {
      const file = 'zc_view.ddls.asddls';
      const objType = detectObjectType(file);

      expect(objType).toBe('DDLS');
    });

    test('detects FUGR type', () => {
      const file = 'ztest.fugr.xml';
      const objType = detectObjectType(file);

      expect(objType).toBe('FUGR');
    });
  });

  describe('Variant handling', () => {
    test('uses default variant if not specified', () => {
      const args = ['inspect', '--files', 'src/zcl_test.clas.abap'];
      const variantIndex = args.indexOf('--variant');
      const variant = variantIndex !== -1 ? args[variantIndex + 1] : null;

      expect(variant).toBeNull();
    });

    test('uses specified variant', () => {
      const args = ['inspect', '--files', 'src/zcl_test.clas.abap', '--variant', 'ALL_CHECKS'];
      const variantIndex = args.indexOf('--variant');
      const variant = args[variantIndex + 1];

      expect(variant).toBe('ALL_CHECKS');
    });

    test('handles EMPTY variant', () => {
      const args = ['inspect', '--variant', 'EMPTY'];
      const variantIndex = args.indexOf('--variant');
      const variant = args[variantIndex + 1];

      expect(variant).toBe('EMPTY');
    });

    test('CLI --variant overrides project config variant', () => {
      const variantArg = 'CLI_VARIANT';
      const inspectConfig = { variant: 'PROJECT_VARIANT' };
      const variant = variantArg || inspectConfig.variant || null;

      expect(variant).toBe('CLI_VARIANT');
    });

    test('falls back to project config variant when no --variant flag', () => {
      const variantArg = null;
      const inspectConfig = { variant: 'PROJECT_VARIANT' };
      const variant = variantArg || inspectConfig.variant || null;

      expect(variant).toBe('PROJECT_VARIANT');
    });

    test('variant is null when neither CLI flag nor project config set', () => {
      const variantArg = null;
      const inspectConfig = { variant: null };
      const variant = variantArg || inspectConfig.variant || null;

      expect(variant).toBeNull();
    });
  });

  describe('Response handling', () => {
    test('handles successful inspection with no errors', () => {
      const response = {
        success: 'X',
        results: [{
          object_type: 'CLAS',
          object_name: 'ZCL_TEST',
          success: 'X',
          errors: [],
          warnings: []
        }]
      };

      const success = response.success === 'X' || response.SUCCESS === 'X';
      const results = response.results || response.RESULTS || [];

      expect(success).toBe(true);
      expect(results[0].errors.length).toBe(0);
    });

    test('handles inspection with errors', () => {
      const response = {
        success: '',
        results: [{
          object_type: 'CLAS',
          object_name: 'ZCL_TEST',
          success: '',
          errors: [
            { line: 10, text: 'Syntax error' }
          ]
        }]
      };

      const success = response.success === 'X' || response.SUCCESS === 'X';
      const results = response.results || response.RESULTS || [];

      expect(success).toBe(false);
      expect(results[0].errors.length).toBeGreaterThan(0);
    });

    test('handles inspection with warnings', () => {
      const response = {
        success: 'X',
        results: [{
          object_type: 'CLAS',
          object_name: 'ZCL_TEST',
          success: 'X',
          errors: [],
          warnings: [
            { line: 5, text: 'Unused variable' }
          ]
        }]
      };

      const results = response.results || response.RESULTS || [];

      expect(results[0].warnings.length).toBeGreaterThan(0);
    });

    test('handles multiple objects in response', () => {
      const response = {
        success: 'X',
        results: [
          { object_name: 'ZCL_TEST1', success: 'X' },
          { object_name: 'ZCL_TEST2', success: 'X' }
        ]
      };

      const results = response.results || response.RESULTS || [];

      expect(results.length).toBe(2);
    });
  });

  describe('Error formatting', () => {
    test('formats error with line number', () => {
      const error = {
        line: 10,
        column: 5,
        text: 'Syntax error'
      };

      const line = error.line || error.LINE || '?';
      const column = error.column || error.COLUMN;
      const text = error.text || error.TEXT || 'Unknown error';

      expect(line).toBe(10);
      expect(text).toBeDefined();
    });

    test('formats error with method name', () => {
      const error = {
        line: 10,
        method_name: 'GET_DATA',
        text: 'Undefined variable'
      };

      const methodName = error.method_name || error.METHOD_NAME;

      expect(methodName).toBe('GET_DATA');
    });

    test('handles error without column', () => {
      const error = {
        line: 10,
        text: 'Error message'
      };

      const column = error.column || error.COLUMN;

      expect(column).toBeUndefined();
      expect(error.line).toBeDefined();
    });
  });

  describe('Multiple files handling', () => {
    test('handles inspecting multiple files', () => {
      const filesArg = 'src/zcl_class1.clas.abap,src/zcl_class2.clas.abap';
      const files = filesArg.split(',').map(f => f.trim());

      expect(files.length).toBe(2);
    });

    test('handles mixed file types', () => {
      const files = [
        'src/zcl_class.clas.abap',
        'src/zc_view.ddls.asddls',
        'src/zif_intf.intf.abap'
      ];

      const types = files.map(f => {
        if (f.includes('.clas.')) return 'CLAS';
        if (f.includes('.ddls.')) return 'DDLS';
        if (f.includes('.intf.')) return 'INTF';
        return null;
      });

      expect(types).toContain('CLAS');
      expect(types).toContain('DDLS');
      expect(types).toContain('INTF');
    });
  });
});

describe('Inspect Command - CLI Output Format', () => {
  let consoleOutput;
  let originalConsoleLog;
  let originalConsoleError;

  beforeEach(() => {
    consoleOutput = [];
    originalConsoleLog = console.log;
    originalConsoleError = console.error;
    console.log = (...args) => consoleOutput.push(args.join(' '));
    console.error = (...args) => consoleOutput.push(args.join(' '));
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  test('output matches spec format for passed syntax check', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([
          {
            OBJECT_TYPE: 'CLAS',
            OBJECT_NAME: 'ZCL_MY_CLASS',
            SUCCESS: true,
            ERROR_COUNT: 0,
            WARNING_COUNT: 0,
            ERRORS: [],
            WARNINGS: [],
            MESSAGE: 'Syntax check passed'
          }
        ])
      }))
    };

    await inspectCommand.execute(['--files', 'zcl_my_class.clas.abap'], mockContext);

    const output = consoleOutput.join('\n');

    // Verify format using verifier
    const verified = verifiers.verifyInspectOutput(output, 'ZCL_MY_CLASS');
    expect(verified).toBe(true);

    // Additional specific checks
    expect(output).toMatch(/✅/);
    expect(output).toMatch(/CLAS/);
    expect(output).toMatch(/ZCL_MY_CLASS/);
    expect(output).toMatch(/Syntax check passed/);
  });

  test('output matches spec format for syntax check with warnings', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([
          {
            OBJECT_TYPE: 'CLAS',
            OBJECT_NAME: 'ZCL_MY_CLASS',
            SUCCESS: true,
            ERROR_COUNT: 0,
            WARNING_COUNT: 2,
            ERRORS: [],
            WARNINGS: [
              {
                SOBJNAME: 'ZCL_MY_CLASS========CM002',
                LINE: 49,
                COLUMN: 0,
                METHOD_NAME: 'MY_METHOD',
                TEXT: 'The exception CX_DD_DDL_READ is not caught'
              },
              {
                SOBJNAME: 'ZCL_MY_CLASS========CM002',
                LINE: 31,
                COLUMN: 0,
                METHOD_NAME: 'MY_METHOD',
                TEXT: 'Another warning'
              }
            ],
            MESSAGE: 'Syntax check passed with warnings'
          }
        ])
      }))
    };

    await inspectCommand.execute(['--files', 'zcl_my_class.clas.abap'], mockContext);

    const output = consoleOutput.join('\n');

    // Verify format
    const verified = verifiers.verifyInspectOutput(output, 'ZCL_MY_CLASS');
    expect(verified).toBe(true);

    // Specific checks for warnings
    expect(output).toMatch(/⚠️/);
    expect(output).toMatch(/Warnings:/);
    expect(output).toMatch(/─{20,}/); // Separator line
    expect(output).toMatch(/MY_METHOD/);
    expect(output).toMatch(/Line.*49/);
  });

  test('output matches spec format for failed syntax check', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([
          {
            OBJECT_TYPE: 'CLAS',
            OBJECT_NAME: 'ZCL_MY_CLASS',
            SUCCESS: false,
            ERROR_COUNT: 1,
            WARNING_COUNT: 0,
            ERRORS: [
              {
                SOBJNAME: 'ZCL_MY_CLASS========CM002',
                LINE: 21,
                COLUMN: 12,
                METHOD_NAME: 'MY_METHOD',
                TEXT: 'Syntax error: Unknown statement'
              }
            ],
            WARNINGS: [],
            MESSAGE: 'Syntax check failed'
          }
        ])
      }))
    };

    await expect(
      inspectCommand.execute(['--files', 'zcl_my_class.clas.abap'], mockContext)
    ).rejects.toThrow('process.exit(1)');

    const output = consoleOutput.join('\n');

    // Verify format
    const verified = verifiers.verifyInspectOutput(output, 'ZCL_MY_CLASS');
    expect(verified).toBe(true);

    // Specific checks for errors
    expect(output).toMatch(/❌/);
    expect(output).toMatch(/Errors:/);
    expect(output).toMatch(/─{20,}/); // Separator line
    expect(output).toMatch(/Line.*21/);
    expect(output).toMatch(/Column.*12/);
    expect(output).toMatch(/Syntax error/);
  });

  test('JSON output mode suppresses progress messages', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([
          {
            OBJECT_TYPE: 'CLAS',
            OBJECT_NAME: 'ZCL_MY_CLASS',
            SUCCESS: true,
            ERROR_COUNT: 0,
            ERRORS: [],
            WARNINGS: []
          }
        ])
      }))
    };

    await inspectCommand.execute(['--files', 'zcl_my_class.clas.abap', '--json'], mockContext);

    const output = consoleOutput.join('\n');

    // JSON mode should NOT show progress messages
    expect(output).not.toMatch(/Inspect for/i);
    expect(output).not.toMatch(/✅/);

    // Should only contain JSON
    expect(() => JSON.parse(output)).not.toThrow();
    const parsed = JSON.parse(output);
    expect(Array.isArray(parsed)).toBe(true);
    expect(parsed[0].OBJECT_TYPE).toBe('CLAS');
  });

  test('JSON output mode returns complete result array', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockResult = [
      {
        OBJECT_TYPE: 'CLAS',
        OBJECT_NAME: 'ZCL_CLASS1',
        SUCCESS: false,
        ERROR_COUNT: 2,
        ERRORS: [
          { LINE: '10', COLUMN: '5', TEXT: 'Variable not defined', METHOD_NAME: 'METHOD1' },
          { LINE: '25', COLUMN: '12', TEXT: 'Type mismatch', METHOD_NAME: 'METHOD2' }
        ],
        WARNINGS: [
          { LINE: '15', MESSAGE: 'Obsolete statement' }
        ]
      },
      {
        OBJECT_TYPE: 'INTF',
        OBJECT_NAME: 'ZIF_INTERFACE',
        SUCCESS: true,
        ERROR_COUNT: 0,
        ERRORS: [],
        WARNINGS: []
      }
    ];

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue(mockResult)
      }))
    };

    await inspectCommand.execute(['--files', 'zcl_class1.clas.abap,zif_interface.intf.abap', '--json'], mockContext);

    const output = consoleOutput.join('\n');
    const parsed = JSON.parse(output);

    // Verify structure
    expect(Array.isArray(parsed)).toBe(true);
    expect(parsed).toHaveLength(2);

    // Verify first result (with errors)
    expect(parsed[0].OBJECT_TYPE).toBe('CLAS');
    expect(parsed[0].ERROR_COUNT).toBe(2);
    expect(parsed[0].ERRORS).toHaveLength(2);
    expect(parsed[0].WARNINGS).toHaveLength(1);

    // Verify second result (no errors)
    expect(parsed[1].OBJECT_TYPE).toBe('INTF');
    expect(parsed[1].ERROR_COUNT).toBe(0);
  });
});

describe('Inspect Command - JUnit Output', () => {
  let consoleOutput;
  let originalConsoleLog;
  let originalConsoleError;

  beforeEach(() => {
    consoleOutput = [];
    originalConsoleLog = console.log;
    originalConsoleError = console.error;
    console.log = (...args) => consoleOutput.push(args.join(' '));
    console.error = (...args) => consoleOutput.push(args.join(' '));
    jest.resetModules();
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.mkdirSync.mockReset();
    fs.writeFileSync.mockReset();
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  test('writes JUnit XML file when --junit-output is specified', async () => {
    const fs = require('fs');
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([{
          OBJECT_TYPE: 'CLAS',
          OBJECT_NAME: 'ZCL_MY_CLASS',
          SUCCESS: true,
          ERROR_COUNT: 0,
          ERRORS: [],
          WARNINGS: []
        }])
      }))
    };

    await inspectCommand.execute(
      ['--files', 'zcl_my_class.clas.abap', '--junit-output', 'reports/inspect.xml'],
      mockContext
    );

    expect(fs.writeFileSync).toHaveBeenCalledWith(
      expect.stringContaining('inspect.xml'),
      expect.stringContaining('<?xml version="1.0"'),
      'utf8'
    );
  });

  test('JUnit XML contains testsuite for each object', async () => {
    const fs = require('fs');
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([
          { OBJECT_TYPE: 'CLAS', OBJECT_NAME: 'ZCL_FOO', SUCCESS: true, ERROR_COUNT: 0, ERRORS: [], WARNINGS: [] },
          { OBJECT_TYPE: 'INTF', OBJECT_NAME: 'ZIF_BAR', SUCCESS: true, ERROR_COUNT: 0, ERRORS: [], WARNINGS: [] }
        ])
      }))
    };

    await inspectCommand.execute(
      ['--files', 'zcl_foo.clas.abap,zif_bar.intf.abap', '--junit-output', 'out.xml'],
      mockContext
    );

    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).toContain('ZCL_FOO');
    expect(writtenXml).toContain('ZIF_BAR');
    expect(writtenXml).toContain('<testsuite');
    expect(writtenXml).toContain('<testsuites>');
  });

  test('JUnit XML contains failure element for syntax errors', async () => {
    const fs = require('fs');
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([{
          OBJECT_TYPE: 'CLAS',
          OBJECT_NAME: 'ZCL_MY_CLASS',
          SUCCESS: false,
          ERROR_COUNT: 1,
          ERRORS: [{ LINE: '21', COLUMN: '12', TEXT: 'Field "LV_VAR" is unknown', METHOD_NAME: 'MY_METHOD' }],
          WARNINGS: []
        }])
      }))
    };

    await expect(
      inspectCommand.execute(['--files', 'zcl_my_class.clas.abap', '--junit-output', 'out.xml'], mockContext)
    ).rejects.toThrow('process.exit(1)');

    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).toContain('<failure');
    expect(writtenXml).toContain('SyntaxError');
    expect(writtenXml).toContain('MY_METHOD');
    expect(writtenXml).toContain('LV_VAR');
  });

  test('JUnit XML escapes special XML characters', async () => {
    const fs = require('fs');
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([{
          OBJECT_TYPE: 'CLAS',
          OBJECT_NAME: 'ZCL_MY_CLASS',
          SUCCESS: false,
          ERROR_COUNT: 1,
          ERRORS: [{ LINE: '5', COLUMN: '1', TEXT: 'Use ">" instead of \'<\'', METHOD_NAME: 'M' }],
          WARNINGS: []
        }])
      }))
    };

    await expect(
      inspectCommand.execute(['--files', 'zcl_my_class.clas.abap', '--junit-output', 'out.xml'], mockContext)
    ).rejects.toThrow('process.exit(1)');

    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).not.toContain('Use ">');      // raw > in attribute is ok but < must be escaped
    expect(writtenXml).toContain('&lt;');
    expect(writtenXml).toContain('&quot;');
  });

  test('creates output directory if it does not exist', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(false);
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([{
          OBJECT_TYPE: 'CLAS', OBJECT_NAME: 'ZCL_FOO',
          SUCCESS: true, ERROR_COUNT: 0, ERRORS: [], WARNINGS: []
        }])
      }))
    };

    await inspectCommand.execute(
      ['--files', 'zcl_foo.clas.abap', '--junit-output', 'reports/sub/out.xml'],
      mockContext
    );

    expect(fs.mkdirSync).toHaveBeenCalledWith(expect.any(String), { recursive: true });
  });

  test('still prints normal output when --junit-output is set', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: [], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([{
          OBJECT_TYPE: 'CLAS', OBJECT_NAME: 'ZCL_FOO',
          SUCCESS: true, ERROR_COUNT: 0, ERRORS: [], WARNINGS: []
        }])
      }))
    };

    await inspectCommand.execute(
      ['--files', 'zcl_foo.clas.abap', '--junit-output', 'out.xml'],
      mockContext
    );

    const output = consoleOutput.join('\n');
    expect(output).toContain('ZCL_FOO');
    expect(output).toContain('Syntax check passed');
    expect(output).toContain('JUnit report written to');
  });

  test('excludes files matching inspect.exclude patterns', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockPost = jest.fn().mockResolvedValue([{
      OBJECT_TYPE: 'CLAS', OBJECT_NAME: 'ZCL_KEPT',
      SUCCESS: true, ERROR_COUNT: 0, ERRORS: [], WARNINGS: []
    }]);

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: ['zcl_skip_*', 'zcl_exact'] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: mockPost
      }))
    };

    await inspectCommand.execute(
      ['--files', 'zcl_kept.clas.abap,zcl_skip_this.clas.abap,zcl_exact.clas.abap'],
      mockContext
    );

    // Only zcl_kept should reach the API
    expect(mockPost).toHaveBeenCalledTimes(1);
    const postedFiles = mockPost.mock.calls[0][1].files;
    expect(postedFiles).toEqual(['ZCL_KEPT.CLAS.ABAP']);

    const output = consoleOutput.join('\n');
    expect(output).toContain('Skipped 2 file(s) excluded');
  });

  test('outputs empty JSON when all files are excluded', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockPost = jest.fn();

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({ variant: null, exclude: ['zcl_*'], suppress: [] })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: mockPost
      }))
    };

    await inspectCommand.execute(
      ['--files', 'zcl_foo.clas.abap', '--json'],
      mockContext
    );

    // Should not call the API
    expect(mockPost).not.toHaveBeenCalled();
    const output = consoleOutput.join('\n');
    expect(output).toContain('[]');
  });

  test('suppress downgrades matching errors to infos', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({
        variant: null, exclude: [],
        suppress: [{ object: 'zcl_foo', message: '*pragma*' }]
      })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([{
          OBJECT_TYPE: 'CLAS', OBJECT_NAME: 'ZCL_FOO',
          SUCCESS: false, ERROR_COUNT: 2,
          ERRORS: [
            { LINE: '10', TEXT: 'Missing pragma ##NO_TEXT' },
            { LINE: '20', TEXT: 'Variable not used' }
          ],
          WARNINGS: [], INFOS: []
        }])
      }))
    };

    await inspectCommand.execute(
      ['--files', 'zcl_foo.clas.abap', '--json'],
      mockContext
    );

    const output = JSON.parse(consoleOutput.join(''));
    const result = output[0];
    // Pragma error should be downgraded: removed from ERRORS, added to INFOS
    expect(result.ERROR_COUNT).toBe(1);
    expect(result.ERRORS).toHaveLength(1);
    expect(result.ERRORS[0].TEXT).toBe('Variable not used');
    expect(result.INFOS).toHaveLength(1);
    expect(result.INFOS[0].MESSAGE).toContain('[suppressed]');
    expect(result.INFOS[0].MESSAGE).toContain('pragma');
  });

  test('suppress does not affect non-matching objects', async () => {
    const inspectCommand = require('../../src/commands/inspect');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getInspectConfig: jest.fn(() => ({
        variant: null, exclude: [],
        suppress: [{ object: 'zcl_other', message: '*' }]
      })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue([{
          OBJECT_TYPE: 'CLAS', OBJECT_NAME: 'ZCL_FOO',
          SUCCESS: false, ERROR_COUNT: 1,
          ERRORS: [{ LINE: '10', TEXT: 'Some error' }],
          WARNINGS: [], INFOS: []
        }])
      }))
    };

    await inspectCommand.execute(
      ['--files', 'zcl_foo.clas.abap', '--json'],
      mockContext
    );

    const output = JSON.parse(consoleOutput.join(''));
    // Rule targets zcl_other, not zcl_foo — errors should remain
    expect(output[0].ERROR_COUNT).toBe(1);
    expect(output[0].ERRORS).toHaveLength(1);
  });

});
