/**
 * Unit tests for syntax command in CLI
 * Tests the CLI parsing, file handling, and response formatting
 */

const verifiers = require('../helpers/output-verifiers');

const fs = require('fs');
const path = require('path');
const https = require('https');
const { EventEmitter } = require('events');

// Mock fs
jest.mock('fs');

// Mock https
jest.mock('https');

// Mock config
jest.mock('../../src/config', () => ({
  getAbapConfig: jest.fn().mockReturnValue({
    host: 'test.sap.com',
    sapport: 443,
    client: '100',
    user: 'TEST_USER',
    password: 'TEST_PASS',
    language: 'EN'
  })
}));

// Mock process.exit
const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

describe('Syntax Command - Logic Tests', () => {
  let mockRequest;
  let mockReq;
  let mockRes;

  beforeEach(() => {
    jest.clearAllMocks();

    // Setup mock request/response
    mockReq = new EventEmitter();
    mockReq.write = jest.fn();
    mockReq.end = jest.fn();

    mockRes = new EventEmitter();
    mockRes.statusCode = 200;
    mockRes.headers = {
      'x-csrf-token': 'test-csrf-token',
      'set-cookie': ['SAP_SESSION=abc123']
    };

    mockRequest = jest.fn((options, callback) => {
      // Simulate async response
      process.nextTick(() => {
        callback(mockRes);
      });
      return mockReq;
    });

    https.request = mockRequest;
    https.Agent = jest.fn();

    // Mock fs methods
    fs.existsSync = jest.fn().mockReturnValue(true);
    fs.readFileSync = jest.fn().mockReturnValue('CLASS zcl_test DEFINITION. ENDCLASS.');
    fs.writeFileSync = jest.fn();
    fs.unlinkSync = jest.fn();
  });

  describe('File parsing', () => {
    test('parses class file correctly', () => {
      const baseName = 'zcl_my_class.clas.abap';

      expect(baseName.includes('.clas.')).toBe(true);
      expect(baseName.split('.')[0].toUpperCase()).toBe('ZCL_MY_CLASS');
    });

    test('parses interface file correctly', () => {
      const baseName = 'zif_my_interface.intf.abap';

      expect(baseName.includes('.intf.')).toBe(true);
      expect(baseName.split('.')[0].toUpperCase()).toBe('ZIF_MY_INTERFACE');
    });

    test('parses program file correctly', () => {
      const baseName = 'zmy_program.prog.abap';

      expect(baseName.includes('.prog.')).toBe(true);
      expect(baseName.split('.')[0].toUpperCase()).toBe('ZMY_PROGRAM');
    });

    test('parses locals_def file correctly', () => {
      const baseName = 'zcl_my_class.clas.locals_def.abap';

      expect(baseName.includes('.clas.locals_def.')).toBe(true);
      expect(baseName.split('.')[0].toUpperCase()).toBe('ZCL_MY_CLASS');
    });

    test('parses locals_imp file correctly', () => {
      const baseName = 'zcl_my_class.clas.locals_imp.abap';

      expect(baseName.includes('.clas.locals_imp.')).toBe(true);
      expect(baseName.split('.')[0].toUpperCase()).toBe('ZCL_MY_CLASS');
    });
  });

  describe('Object type detection', () => {
    function detectObjectType(baseName) {
      if (baseName.includes('.clas.locals_def.')) return { type: 'CLAS', kind: 'locals_def' };
      if (baseName.includes('.clas.locals_imp.')) return { type: 'CLAS', kind: 'locals_imp' };
      if (baseName.includes('.clas.')) return { type: 'CLAS', kind: 'main' };
      if (baseName.includes('.intf.')) return { type: 'INTF', kind: 'main' };
      if (baseName.includes('.prog.')) return { type: 'PROG', kind: 'main' };
      return { type: 'PROG', kind: 'main' }; // default
    }

    test('detects CLAS type from .clas.abap', () => {
      const result = detectObjectType('zcl_test.clas.abap');
      expect(result.type).toBe('CLAS');
      expect(result.kind).toBe('main');
    });

    test('detects INTF type from .intf.abap', () => {
      const result = detectObjectType('zif_test.intf.abap');
      expect(result.type).toBe('INTF');
      expect(result.kind).toBe('main');
    });

    test('detects PROG type from .prog.abap', () => {
      const result = detectObjectType('ztest.prog.abap');
      expect(result.type).toBe('PROG');
      expect(result.kind).toBe('main');
    });

    test('detects locals_def from .clas.locals_def.abap', () => {
      const result = detectObjectType('zcl_test.clas.locals_def.abap');
      expect(result.type).toBe('CLAS');
      expect(result.kind).toBe('locals_def');
    });

    test('detects locals_imp from .clas.locals_imp.abap', () => {
      const result = detectObjectType('zcl_test.clas.locals_imp.abap');
      expect(result.type).toBe('CLAS');
      expect(result.kind).toBe('locals_imp');
    });

    test('defaults to PROG for unknown extensions', () => {
      const result = detectObjectType('unknown_file.txt');
      expect(result.type).toBe('PROG');
    });
  });

  describe('Request building', () => {
    test('builds correct request data for single class', () => {
      const objects = [{
        type: 'CLAS',
        name: 'ZCL_TEST',
        source: 'CLASS zcl_test DEFINITION. ENDCLASS.'
      }];

      const data = {
        objects: objects,
        mode: 'working_area',
        uccheck: 'X'
      };

      expect(data.objects.length).toBe(1);
      expect(data.objects[0].type).toBe('CLAS');
      expect(data.objects[0].name).toBe('ZCL_TEST');
      expect(data.uccheck).toBe('X');
    });

    test('builds correct request data for cloud mode', () => {
      const objects = [{
        type: 'CLAS',
        name: 'ZCL_TEST',
        source: 'CLASS zcl_test DEFINITION. ENDCLASS.'
      }];

      const data = {
        objects: objects,
        mode: 'working_area',
        uccheck: '5' // Cloud mode
      };

      expect(data.uccheck).toBe('5');
    });

    test('includes locals_def and locals_imp when present', () => {
      const objects = [{
        type: 'CLAS',
        name: 'ZCL_TEST',
        source: 'CLASS zcl_test DEFINITION. ENDCLASS.',
        locals_def: 'CLASS lcl_helper DEFINITION. ENDCLASS.',
        locals_imp: 'CLASS lcl_helper IMPLEMENTATION. ENDCLASS.'
      }];

      expect(objects[0].locals_def).toBeDefined();
      expect(objects[0].locals_imp).toBeDefined();
    });

    test('builds correct request data for multiple objects', () => {
      const objects = [
        { type: 'CLAS', name: 'ZCL_TEST1', source: 'CLASS zcl_test1...' },
        { type: 'INTF', name: 'ZIF_TEST', source: 'INTERFACE zif_test...' },
        { type: 'PROG', name: 'ZTEST_PROG', source: 'REPORT ztest_prog.' }
      ];

      const data = {
        objects: objects,
        mode: 'working_area',
        uccheck: 'X'
      };

      expect(data.objects.length).toBe(3);
      expect(data.objects[0].type).toBe('CLAS');
      expect(data.objects[1].type).toBe('INTF');
      expect(data.objects[2].type).toBe('PROG');
    });
  });

  describe('Response handling', () => {
    test('handles successful response with uppercase keys', () => {
      const response = {
        SUCCESS: 'X',
        MESSAGE: 'Syntax check completed',
        RESULTS: [{
          OBJECT_TYPE: 'CLAS',
          OBJECT_NAME: 'ZCL_TEST',
          SUCCESS: 'X',
          ERROR_COUNT: 0,
          ERRORS: [],
          WARNINGS: []
        }]
      };

      const success = response.SUCCESS === 'X' || response.success === 'X';
      const results = response.RESULTS || response.results || [];

      expect(success).toBe(true);
      expect(results.length).toBe(1);
      expect(results[0].OBJECT_NAME).toBe('ZCL_TEST');
    });

    test('handles successful response with lowercase keys', () => {
      const response = {
        success: 'X',
        message: 'Syntax check completed',
        results: [{
          object_type: 'CLAS',
          object_name: 'ZCL_TEST',
          success: 'X',
          error_count: 0,
          errors: [],
          warnings: []
        }]
      };

      const success = response.SUCCESS === 'X' || response.success === 'X';
      const results = response.RESULTS || response.results || [];

      expect(success).toBe(true);
      expect(results.length).toBe(1);
      expect(results[0].object_name).toBe('ZCL_TEST');
    });

    test('handles failed response with errors', () => {
      const response = {
        SUCCESS: '',
        MESSAGE: 'Syntax errors found',
        RESULTS: [{
          OBJECT_TYPE: 'CLAS',
          OBJECT_NAME: 'ZCL_TEST',
          SUCCESS: '',
          ERROR_COUNT: 2,
          ERRORS: [
            { LINE: 10, COLUMN: 5, TEXT: 'Unknown statement' },
            { LINE: 15, COLUMN: 1, TEXT: 'Missing period' }
          ],
          WARNINGS: []
        }]
      };

      const success = response.SUCCESS === 'X' || response.success === 'X';
      const results = response.RESULTS || response.results || [];

      expect(success).toBe(false);
      expect(results[0].ERROR_COUNT).toBe(2);
      expect(results[0].ERRORS.length).toBe(2);
      expect(results[0].ERRORS[0].LINE).toBe(10);
    });

    test('handles response with warnings', () => {
      const response = {
        SUCCESS: 'X',
        MESSAGE: 'Syntax check completed with warnings',
        RESULTS: [{
          OBJECT_TYPE: 'CLAS',
          OBJECT_NAME: 'ZCL_TEST',
          SUCCESS: 'X',
          ERROR_COUNT: 0,
          ERRORS: [],
          WARNINGS: [
            { LINE: 20, TEXT: 'Unused variable' }
          ]
        }]
      };

      const results = response.RESULTS || response.results || [];

      expect(results[0].SUCCESS).toBe('X');
      expect(results[0].WARNINGS.length).toBe(1);
    });

    test('handles unsupported object type response', () => {
      const response = {
        SUCCESS: '',
        MESSAGE: 'Unsupported object type',
        RESULTS: [{
          OBJECT_TYPE: 'DDLS',
          OBJECT_NAME: 'ZC_MY_VIEW',
          SUCCESS: '',
          ERROR_COUNT: 1,
          ERRORS: [{
            LINE: 1,
            TEXT: "Unsupported object type: DDLS. Syntax command only supports CLAS, INTF, PROG. Use 'pull' command for other object types."
          }],
          MESSAGE: "Unsupported object type: DDLS. Use 'pull' command instead."
        }]
      };

      const results = response.RESULTS || response.results || [];

      expect(results[0].SUCCESS).toBe('');
      expect(results[0].ERRORS[0].TEXT).toContain('Unsupported object type');
      expect(results[0].ERRORS[0].TEXT).toContain('pull');
    });
  });

  describe('Class file grouping', () => {
    test('groups main and locals files for same class', () => {
      const classFilesMap = new Map();
      const files = [
        'src/zcl_test.clas.abap',
        'src/zcl_test.clas.locals_def.abap',
        'src/zcl_test.clas.locals_imp.abap'
      ];

      for (const file of files) {
        const baseName = path.basename(file);
        const className = baseName.split('.')[0].toUpperCase();
        let fileKind = 'main';

        if (baseName.includes('.clas.locals_def.')) {
          fileKind = 'locals_def';
        } else if (baseName.includes('.clas.locals_imp.')) {
          fileKind = 'locals_imp';
        }

        if (!classFilesMap.has(className)) {
          classFilesMap.set(className, { main: null, locals_def: null, locals_imp: null });
        }
        classFilesMap.get(className)[fileKind] = `source for ${fileKind}`;
      }

      expect(classFilesMap.has('ZCL_TEST')).toBe(true);
      expect(classFilesMap.get('ZCL_TEST').main).toBe('source for main');
      expect(classFilesMap.get('ZCL_TEST').locals_def).toBe('source for locals_def');
      expect(classFilesMap.get('ZCL_TEST').locals_imp).toBe('source for locals_imp');
    });

    test('handles multiple classes separately', () => {
      const classFilesMap = new Map();
      const files = [
        'src/zcl_class1.clas.abap',
        'src/zcl_class2.clas.abap',
        'src/zcl_class1.clas.locals_def.abap'
      ];

      for (const file of files) {
        const baseName = path.basename(file);
        const className = baseName.split('.')[0].toUpperCase();
        let fileKind = 'main';

        if (baseName.includes('.clas.locals_def.')) {
          fileKind = 'locals_def';
        } else if (baseName.includes('.clas.locals_imp.')) {
          fileKind = 'locals_imp';
        }

        if (!classFilesMap.has(className)) {
          classFilesMap.set(className, { main: null, locals_def: null, locals_imp: null });
        }
        classFilesMap.get(className)[fileKind] = `source for ${className} ${fileKind}`;
      }

      expect(classFilesMap.size).toBe(2);
      expect(classFilesMap.has('ZCL_CLASS1')).toBe(true);
      expect(classFilesMap.has('ZCL_CLASS2')).toBe(true);
      expect(classFilesMap.get('ZCL_CLASS1').locals_def).toBeDefined();
      expect(classFilesMap.get('ZCL_CLASS2').locals_def).toBeNull();
    });
  });

  describe('Mode options', () => {
    test('defaults to working_area mode', () => {
      const args = ['syntax', '--files', 'src/zcl_test.clas.abap'];
      const modeArgIndex = args.indexOf('--mode');
      const mode = modeArgIndex !== -1 ? args[modeArgIndex + 1] : 'working_area';

      expect(mode).toBe('working_area');
    });

    test('uses syntax_statement mode when specified', () => {
      const args = ['syntax', '--files', 'src/zcl_test.clas.abap', '--mode', 'syntax_statement'];
      const modeArgIndex = args.indexOf('--mode');
      const mode = modeArgIndex !== -1 ? args[modeArgIndex + 1] : 'working_area';

      expect(mode).toBe('syntax_statement');
    });

    test('detects cloud mode flag', () => {
      const args = ['syntax', '--files', 'src/zcl_test.clas.abap', '--cloud'];
      const cloudMode = args.includes('--cloud');

      expect(cloudMode).toBe(true);
    });

    test('detects json output flag', () => {
      const args = ['syntax', '--files', 'src/zcl_test.clas.abap', '--json'];
      const jsonOutput = args.includes('--json');

      expect(jsonOutput).toBe(true);
    });
  });

  describe('Error formatting', () => {
    test('formats error with line and column', () => {
      const error = { LINE: 10, COLUMN: 5, TEXT: 'Unknown statement' };

      const line = error.LINE || error.line || '?';
      const column = error.COLUMN || error.column || '';
      const text = error.TEXT || error.text || 'Unknown error';

      expect(line).toBe(10);
      expect(column).toBe(5);
      expect(text).toBe('Unknown statement');
    });

    test('formats error with method name', () => {
      const error = {
        LINE: 10,
        TEXT: 'Unknown statement',
        METHOD_NAME: 'CONSTRUCTOR'
      };

      const methodName = error.METHOD_NAME || error.method_name || '';

      expect(methodName).toBe('CONSTRUCTOR');
    });

    test('formats error without column', () => {
      const error = { LINE: 10, TEXT: 'Unknown statement' };

      const column = error.COLUMN || error.column || '';

      expect(column).toBe('');
    });
  });

  describe('File validation', () => {
    test('checks file existence', () => {
      fs.existsSync.mockReturnValue(true);

      const filePath = '/path/to/zcl_test.clas.abap';
      const exists = fs.existsSync(filePath);

      expect(exists).toBe(true);
      expect(fs.existsSync).toHaveBeenCalledWith(filePath);
    });

    test('handles missing file', () => {
      fs.existsSync.mockReturnValue(false);

      const filePath = '/path/to/nonexistent.clas.abap';
      const exists = fs.existsSync(filePath);

      expect(exists).toBe(false);
    });

    test('reads file content', () => {
      fs.existsSync.mockReturnValue(true);
      fs.readFileSync.mockReturnValue('CLASS zcl_test DEFINITION PUBLIC. ENDCLASS.');

      const content = fs.readFileSync('/path/to/zcl_test.clas.abap', 'utf8');

      expect(content).toContain('CLASS zcl_test DEFINITION');
    });
  });

  describe('Multiple files handling', () => {
    test('splits comma-separated files', () => {
      const filesArg = 'src/zcl_class1.clas.abap,src/zcl_class2.clas.abap,src/zif_test.intf.abap';
      const files = filesArg.split(',').map(f => f.trim());

      expect(files.length).toBe(3);
      expect(files[0]).toBe('src/zcl_class1.clas.abap');
      expect(files[1]).toBe('src/zcl_class2.clas.abap');
      expect(files[2]).toBe('src/zif_test.intf.abap');
    });

    test('handles files with spaces after comma', () => {
      const filesArg = 'src/zcl_class1.clas.abap, src/zcl_class2.clas.abap';
      const files = filesArg.split(',').map(f => f.trim());

      expect(files.length).toBe(2);
      expect(files[1]).toBe('src/zcl_class2.clas.abap');
    });
  });

  describe('uccheck parameter', () => {
    test('uses X for standard ABAP', () => {
      const cloudMode = false;
      const uccheck = cloudMode ? '5' : 'X';

      expect(uccheck).toBe('X');
    });

    test('uses 5 for ABAP Cloud', () => {
      const cloudMode = true;
      const uccheck = cloudMode ? '5' : 'X';

      expect(uccheck).toBe('5');
    });
  });
});

describe('Syntax Command - CLI Output Format', () => {
  let consoleOutput;
  let originalConsoleLog;
  let originalConsoleError;

  beforeEach(() => {
    consoleOutput = [];
    originalConsoleLog = console.log;
    originalConsoleError = console.error;
    console.log = (...args) => consoleOutput.push(args.join(' '));
    console.error = (...args) => consoleOutput.push(args.join(' '));

    // Reset mocks
    jest.clearAllMocks();
    // Main file exists, companion files don't
    fs.existsSync = jest.fn((path) => {
      return !path.includes('locals_def') && !path.includes('locals_imp');
    });
    fs.readFileSync = jest.fn().mockReturnValue('CLASS zcl_test DEFINITION. ENDCLASS.');
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  test('output matches spec format for passed syntax check', async () => {
    const syntaxCommand = require('../../src/commands/syntax');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          COMMAND: 'SYNTAX',
          MESSAGE: 'All 1 object(s) passed syntax check',
          RESULTS: [
            {
              OBJECT_TYPE: 'CLAS',
              OBJECT_NAME: 'ZCL_MY_CLASS',
              SUCCESS: true,
              ERROR_COUNT: 0,
              ERRORS: [],
              MESSAGE: 'Syntax check passed'
            }
          ]
        })
      }))
    };

    await syntaxCommand.execute(['--files', 'zcl_my_class.clas.abap'], mockContext);

    const output = consoleOutput.join('\n');

    // Verify format using verifier
    const verified = verifiers.verifySyntaxOutput(output, 'ZCL_MY_CLASS');
    expect(verified).toBe(true);

    // Additional specific checks
    expect(output).toMatch(/Syntax check for 1 file\(s\)/);
    expect(output).toMatch(/✅/);
    expect(output).toMatch(/CLAS/);
    expect(output).toMatch(/ZCL_MY_CLASS/);
    expect(output).toMatch(/All 1 object\(s\) passed/);
  });

  test('output matches spec format for failed syntax check', async () => {
    const syntaxCommand = require('../../src/commands/syntax');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: false,
          COMMAND: 'SYNTAX',
          MESSAGE: '1 of 1 object(s) have syntax errors',
          RESULTS: [
            {
              OBJECT_TYPE: 'CLAS',
              OBJECT_NAME: 'ZCL_MY_CLASS',
              SUCCESS: false,
              ERROR_COUNT: 1,
              ERRORS: [
                {
                  LINE: 9,
                  TEXT: 'The statement "UNKNOWN_STATEMENT" is invalid.'
                }
              ],
              MESSAGE: 'Syntax check failed'
            }
          ]
        })
      }))
    };

    await syntaxCommand.execute(['--files', 'zcl_my_class.clas.abap'], mockContext);

    const output = consoleOutput.join('\n');

    // Verify format
    const verified = verifiers.verifySyntaxOutput(output, 'ZCL_MY_CLASS');
    expect(verified).toBe(true);

    // Additional specific checks
    expect(output).toMatch(/❌/);
    expect(output).toMatch(/Syntax check failed/);
    expect(output).toMatch(/Errors:/);
    expect(output).toMatch(/─{20,}/); // Separator line
    expect(output).toMatch(/Line 9:/);
    expect(output).toMatch(/1 of 1 object\(s\) have syntax errors/);
  });

  test('output includes cloud mode when --cloud specified', async () => {
    const syntaxCommand = require('../../src/commands/syntax');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: true,
          COMMAND: 'SYNTAX',
          MESSAGE: 'All 1 object(s) passed syntax check',
          RESULTS: [
            {
              OBJECT_TYPE: 'CLAS',
              OBJECT_NAME: 'ZCL_MY_CLASS',
              SUCCESS: true,
              ERROR_COUNT: 0,
              ERRORS: []
            }
          ]
        })
      }))
    };

    await syntaxCommand.execute(['--files', 'zcl_my_class.clas.abap', '--cloud'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/Mode: ABAP Cloud/);
    expect(output).toMatch(/✅/);
  });
});
