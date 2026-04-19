/**
 * Unit tests for unit command
 * Tests AUnit test execution
 */

const verifiers = require('../helpers/output-verifiers');

// Mock fs module
jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn(() => 'mock content'),
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

// Mock process.exit
const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

describe('Unit Command - Logic Tests', () => {
  describe('File parsing', () => {
    test('parses test class file', () => {
      const file = 'src/zcl_my_test.clas.testclasses.abap';
      const isTestClass = file.includes('.testclasses.abap');

      expect(isTestClass).toBe(true);
    });

    test('extracts class name from test file', () => {
      const file = 'zcl_my_test.clas.testclasses.abap';
      const className = file.split('.')[0].toUpperCase();

      expect(className).toBe('ZCL_MY_TEST');
    });
  });

  describe('Multiple test files', () => {
    test('handles multiple test class files', () => {
      const filesArg = 'src/zcl_test1.clas.testclasses.abap,src/zcl_test2.clas.testclasses.abap';
      const files = filesArg.split(',').map(f => f.trim());

      expect(files.length).toBe(2);
      expect(files.every(f => f.includes('.testclasses.abap'))).toBe(true);
    });
  });

  describe('Response handling', () => {
    test('handles successful test run', () => {
      const response = {
        success: 'X',
        test_count: 10,
        passed_count: 10,
        failed_count: 0
      };

      const success = response.success === 'X' || response.SUCCESS === 'X';

      expect(success).toBe(true);
      expect(response.failed_count).toBe(0);
    });

    test('handles failed tests', () => {
      const response = {
        success: '',
        test_count: 10,
        passed_count: 8,
        failed_count: 2,
        errors: [
          { class_name: 'ZCL_TEST', method_name: 'TEST_FAIL', error_text: 'Assertion failed' }
        ]
      };

      const errors = response.errors || response.ERRORS || [];

      expect(response.failed_count).toBe(2);
      expect(errors.length).toBeGreaterThan(0);
    });
  });
});

describe('Unit Command - CLI Output Format', () => {
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

  test('output matches spec format for all tests passed', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: 'X',
          message: 'All tests passed',
          test_count: 10,
          passed_count: 10,
          failed_count: 0,
          errors: []
        })
      }))
    };

    await unitCommand.execute(['--files', 'zcl_my_test.clas.testclasses.abap'], mockContext);

    const output = consoleOutput.join('\n');

    // Verify format using verifier
    const verified = verifiers.verifyUnitOutput(output, 'ZCL_MY_TEST');
    expect(verified).toBe(true);

    // Additional checks
    expect(output).toMatch(/✅/);
    expect(output).toMatch(/All tests passed/);
    expect(output).toMatch(/Tests: 10/);
    expect(output).toMatch(/Passed: 10/);
    expect(output).toMatch(/Failed: 0/);
  });

  test('output matches spec format for failed tests', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: '',
          message: '2 of 10 tests failed',
          test_count: 10,
          passed_count: 8,
          failed_count: 2,
          errors: [
            {
              class_name: 'ZCL_MY_TEST',
              method_name: 'TEST_METHOD_1',
              error_text: 'Expected X but got Y'
            },
            {
              class_name: 'ZCL_MY_TEST',
              method_name: 'TEST_METHOD_2',
              error_text: 'Another error'
            }
          ]
        })
      }))
    };

    await expect(
      unitCommand.execute(['--files', 'zcl_my_test.clas.testclasses.abap'], mockContext)
    ).rejects.toThrow('process.exit(1)');

    const output = consoleOutput.join('\n');

    // Verify format
    const verified = verifiers.verifyUnitOutput(output, 'ZCL_MY_TEST');
    expect(verified).toBe(true);

    // Additional checks for failed tests
    expect(output).toMatch(/❌/);
    expect(output).toMatch(/Tests failed/);
    expect(output).toMatch(/✗/); // Failed test marker
    expect(output).toMatch(/=>/); // Method separator
    expect(output).toMatch(/TEST_METHOD_1/);
    expect(output).toMatch(/Expected X but got Y/);
  });

  test('output handles no unit tests', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: 'X',
          test_count: 0,
          passed_count: 0,
          failed_count: 0,
          errors: []
        })
      }))
    };

    await unitCommand.execute(['--files', 'zcl_my_test.clas.testclasses.abap'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/➖/); // No tests marker
    expect(output).toMatch(/No unit tests/);
    expect(output).toMatch(/Tests: 0/);
  });

  test('JSON output mode suppresses progress messages', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          TEST_COUNT: 5,
          PASSED_COUNT: 5,
          FAILED_COUNT: 0,
          MESSAGE: '5 tests passed',
          ERRORS: []
        })
      }))
    };

    await unitCommand.execute(['--files', 'zcl_my_test.clas.testclasses.abap', '--json'], mockContext);

    const output = consoleOutput.join('\n');

    // JSON mode should NOT show progress messages
    expect(output).not.toMatch(/Running unit tests/i);
    expect(output).not.toMatch(/✅/);

    // Should only contain JSON array
    expect(() => JSON.parse(output)).not.toThrow();
    const parsed = JSON.parse(output);
    expect(Array.isArray(parsed)).toBe(true);
    expect(parsed[0].SUCCESS).toBe('X');
  });

  test('JSON output mode returns complete result array', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockResult = {
      SUCCESS: 'X',
      TEST_COUNT: 10,
      PASSED_COUNT: 8,
      FAILED_COUNT: 2,
      MESSAGE: '8 passed, 2 failed',
      ERRORS: [
        {
          CLASS_NAME: 'ZCL_TEST',
          METHOD_NAME: 'TEST_FAIL_1',
          ERROR_KIND: 'FAILURE',
          ERROR_TEXT: 'Assertion failed: expected 5 but got 3'
        },
        {
          CLASS_NAME: 'ZCL_TEST',
          METHOD_NAME: 'TEST_FAIL_2',
          ERROR_KIND: 'FAILURE',
          ERROR_TEXT: 'Expected exception not raised'
        }
      ],
      COVERAGE_STATS: {
        TOTAL_LINES: 100,
        COVERED_LINES: 85,
        COVERAGE_RATE: 85
      }
    };

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue(mockResult)
      }))
    };

    let output;
    await expect(async () => {
      await unitCommand.execute(['--files', 'zcl_test.clas.testclasses.abap', '--json', '--coverage'], mockContext);
    }).rejects.toThrow('process.exit(1)');

    output = consoleOutput.join('\n');
    // JSON output is the entire console output in JSON mode (pretty-printed array)
    const parsed = JSON.parse(output);

    // Verify structure
    expect(Array.isArray(parsed)).toBe(true);
    expect(parsed).toHaveLength(1);

    const result = parsed[0];
    expect(result.SUCCESS).toBe('X');
    expect(result.TEST_COUNT).toBe(10);
    expect(result.PASSED_COUNT).toBe(8);
    expect(result.FAILED_COUNT).toBe(2);
    expect(result.ERRORS).toHaveLength(2);
    expect(result.COVERAGE_STATS.COVERAGE_RATE).toBe(85);
  });
});

describe('Unit Command - Error Handling', () => {
  let consoleOutput;
  let originalConsoleLog;
  let originalConsoleError;

  beforeEach(() => {
    consoleOutput = [];
    originalConsoleLog = console.log;
    originalConsoleError = console.error;
    console.log = (...args) => consoleOutput.push(args.join(' '));
    console.error = (...args) => consoleOutput.push(args.join(' '));
    mockExit.mockClear();
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  test('should exit with code 1 on HTTP 500 error', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockRejectedValue({
          statusCode: 500,
          message: 'HTTP 500 error',
          body: '<html>Internal Server Error</html>'
        })
      }))
    };

    await expect(async () => {
      await unitCommand.execute(['--files', 'zcl_my_test.clas.testclasses.abap'], mockContext);
    }).rejects.toThrow('process.exit(1)');

    expect(mockExit).toHaveBeenCalledWith(1);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/Error/i);
    expect(output).toMatch(/500/);
  });

  test('should exit with code 1 on JSON parse error', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockRejectedValue({
          statusCode: 200,
          message: 'Failed to parse JSON response',
          body: 'Invalid JSON',
          error: 'Unexpected token'
        })
      }))
    };

    await expect(async () => {
      await unitCommand.execute(['--files', 'zcl_my_test.clas.testclasses.abap'], mockContext);
    }).rejects.toThrow('process.exit(1)');

    expect(mockExit).toHaveBeenCalledWith(1);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/Error/i);
  });

  test('should collect all errors from multiple files and exit with code 1', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockPost = jest.fn()
      // First file: success
      .mockResolvedValueOnce({
        SUCCESS: 'X',
        TEST_COUNT: 3,
        PASSED_COUNT: 3,
        FAILED_COUNT: 0
      })
      // Second file: HTTP error
      .mockRejectedValueOnce({
        statusCode: 500,
        message: 'HTTP 500 error',
        body: '<html>Error</html>'
      })
      // Third file: success
      .mockResolvedValueOnce({
        SUCCESS: 'X',
        TEST_COUNT: 2,
        PASSED_COUNT: 2,
        FAILED_COUNT: 0
      });

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: mockPost
      }))
    };

    const files = 'file1.clas.testclasses.abap,file2.clas.testclasses.abap,file3.clas.testclasses.abap';

    await expect(async () => {
      await unitCommand.execute(['--files', files], mockContext);
    }).rejects.toThrow('process.exit(1)');

    // Should process all 3 files
    expect(mockPost).toHaveBeenCalledTimes(3);

    // Should exit with error code due to second file
    expect(mockExit).toHaveBeenCalledWith(1);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/completed with errors/i);
  });

  test('should output JSON with error details on backend error', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockRejectedValue({
          statusCode: 500,
          message: 'HTTP 500 error',
          body: '<html>Internal Server Error</html>'
        })
      }))
    };

    await expect(async () => {
      await unitCommand.execute(['--files', 'zcl_test.clas.testclasses.abap', '--json'], mockContext);
    }).rejects.toThrow('process.exit(1)');

    const output = consoleOutput.join('\n');

    // Should output JSON
    expect(() => JSON.parse(output)).not.toThrow();

    const parsed = JSON.parse(output);
    expect(Array.isArray(parsed)).toBe(true);
    expect(parsed[0].error).toBeDefined();
    expect(parsed[0].statusCode).toBe(500);

    // Should NOT show progress messages in JSON mode
    expect(output).not.toMatch(/Running unit test/);
  });

  test('should handle file not found error', async () => {
    const fs = require('fs');
    const originalExistsSync = fs.existsSync;
    fs.existsSync = jest.fn().mockReturnValue(false);

    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn()
      }))
    };

    await expect(async () => {
      await unitCommand.execute(['--files', 'nonexistent.clas.testclasses.abap'], mockContext);
    }).rejects.toThrow('process.exit(1)');

    expect(mockExit).toHaveBeenCalledWith(1);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/File not found/i);

    // Restore
    fs.existsSync = originalExistsSync;
  });

  test('should handle invalid file format error', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn()
      }))
    };

    await expect(async () => {
      await unitCommand.execute(['--files', 'invalid.txt'], mockContext);
    }).rejects.toThrow('process.exit(1)');

    expect(mockExit).toHaveBeenCalledWith(1);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/Invalid file format/i);
  });

  test('should NOT exit on successful run', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          TEST_COUNT: 5,
          PASSED_COUNT: 5,
          FAILED_COUNT: 0,
          ERRORS: []
        })
      }))
    };

    await unitCommand.execute(['--files', 'zcl_test.clas.testclasses.abap'], mockContext);

    // Should NOT exit
    expect(mockExit).not.toHaveBeenCalled();

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/All tests passed/);
    expect(output).not.toMatch(/completed with errors/i);
  });
});


describe('Unit Command - JUnit Output', () => {
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
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X', TEST_COUNT: 5, PASSED_COUNT: 5, FAILED_COUNT: 0, ERRORS: []
        })
      }))
    };

    await unitCommand.execute(
      ['--files', 'zcl_my_test.clas.testclasses.abap', '--junit-output', 'reports/unit.xml'],
      mockContext
    );

    expect(fs.writeFileSync).toHaveBeenCalledWith(
      expect.stringContaining('unit.xml'),
      expect.stringContaining('<?xml version="1.0"'),
      'utf8'
    );
  });

  test('JUnit XML contains testsuite with class name', async () => {
    const fs = require('fs');
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X', TEST_COUNT: 3, PASSED_COUNT: 3, FAILED_COUNT: 0, ERRORS: []
        })
      }))
    };

    await unitCommand.execute(
      ['--files', 'zcl_my_test.clas.testclasses.abap', '--junit-output', 'out.xml'],
      mockContext
    );

    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).toContain('ZCL_MY_TEST');
    expect(writtenXml).toContain('<testsuite');
    expect(writtenXml).toContain('<testsuites>');
  });

  test('JUnit XML contains failure element for failed tests', async () => {
    const fs = require('fs');
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: '', TEST_COUNT: 3, PASSED_COUNT: 1, FAILED_COUNT: 2,
          ERRORS: [
            { CLASS_NAME: 'ZCL_MY_TEST', METHOD_NAME: 'TEST_CALC', ERROR_KIND: 'FAILURE', ERROR_TEXT: 'Expected 5 but got 3' },
            { CLASS_NAME: 'ZCL_MY_TEST', METHOD_NAME: 'TEST_NULL', ERROR_KIND: 'ERROR',   ERROR_TEXT: 'Null pointer exception' }
          ]
        })
      }))
    };

    await expect(
      unitCommand.execute(['--files', 'zcl_my_test.clas.testclasses.abap', '--junit-output', 'out.xml'], mockContext)
    ).rejects.toThrow('process.exit(1)');

    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).toContain('<failure');
    expect(writtenXml).toContain('TEST_CALC');
    expect(writtenXml).toContain('TEST_NULL');
    expect(writtenXml).toContain('Expected 5 but got 3');
    expect(writtenXml).toContain('failures="2"');
  });

  test('JUnit XML is written before process.exit on failure', async () => {
    const fs = require('fs');
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: '', TEST_COUNT: 1, PASSED_COUNT: 0, FAILED_COUNT: 1,
          ERRORS: [{ CLASS_NAME: 'ZCL_T', METHOD_NAME: 'M1', ERROR_KIND: 'FAILURE', ERROR_TEXT: 'Fail' }]
        })
      }))
    };

    await expect(
      unitCommand.execute(['--files', 'zcl_t.clas.testclasses.abap', '--junit-output', 'out.xml'], mockContext)
    ).rejects.toThrow('process.exit(1)');

    // XML must have been written even though we exit(1)
    expect(fs.writeFileSync).toHaveBeenCalled();
  });

  test('creates output directory if it does not exist', async () => {
    const fs = require('fs');
    // Only the junit output dir doesn't exist; source file still exists
    fs.existsSync.mockImplementation((p) => !String(p).includes('reports'));
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X', TEST_COUNT: 1, PASSED_COUNT: 1, FAILED_COUNT: 0, ERRORS: []
        })
      }))
    };

    await unitCommand.execute(
      ['--files', 'zcl_my_test.clas.testclasses.abap', '--junit-output', 'reports/deep/unit.xml'],
      mockContext
    );

    expect(fs.mkdirSync).toHaveBeenCalledWith(expect.any(String), { recursive: true });
  });

  test('still prints normal output when --junit-output is set', async () => {
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X', TEST_COUNT: 3, PASSED_COUNT: 3, FAILED_COUNT: 0, ERRORS: []
        })
      }))
    };

    await unitCommand.execute(
      ['--files', 'zcl_my_test.clas.testclasses.abap', '--junit-output', 'out.xml'],
      mockContext
    );

    const output = consoleOutput.join('\n');
    expect(output).toContain('All tests passed');
    expect(output).toContain('JUnit report written to');
  });

  test('JUnit XML escapes special XML characters in error text', async () => {
    const fs = require('fs');
    const unitCommand = require('../../src/commands/unit');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: '', TEST_COUNT: 1, PASSED_COUNT: 0, FAILED_COUNT: 1,
          ERRORS: [{ CLASS_NAME: 'ZCL_T', METHOD_NAME: 'M', ERROR_KIND: 'FAILURE', ERROR_TEXT: '<value> & "expected"' }]
        })
      }))
    };

    await expect(
      unitCommand.execute(['--files', 'zcl_t.clas.testclasses.abap', '--junit-output', 'out.xml'], mockContext)
    ).rejects.toThrow('process.exit(1)');

    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).toContain('&lt;value&gt;');
    expect(writtenXml).toContain('&amp;');
    expect(writtenXml).toContain('&quot;expected&quot;');
  });
});

describe('Unit Command - Coverage Threshold', () => {
  let consoleOutput = [];
  let consoleErrors = [];
  let consoleWarns  = [];
  const originalLog   = console.log;
  const originalError = console.error;
  const originalWarn  = console.warn;

  beforeEach(() => {
    jest.resetModules();
    jest.mock('fs', () => ({
      existsSync: jest.fn(() => true),
      mkdirSync:  jest.fn(),
      writeFileSync: jest.fn(),
      readFileSync:  jest.fn(() => 'mock content'),
    }));
    consoleOutput = [];
    consoleErrors = [];
    consoleWarns  = [];
    console.log   = (...args) => consoleOutput.push(args.join(' '));
    console.error = (...args) => consoleErrors.push(args.join(' '));
    console.warn  = (...args) => consoleWarns.push(args.join(' '));
  });

  afterEach(() => {
    console.log   = originalLog;
    console.error = originalError;
    console.warn  = originalWarn;
  });

  // Helper: build a mock context returning the given coverage_stats in the ABAP response
  const makeMockContext = (coverageStats) => ({
    loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
    AbapHttp: jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('token'),
      post: jest.fn().mockResolvedValue({
        SUCCESS: 'X', TEST_COUNT: 5, PASSED_COUNT: 5, FAILED_COUNT: 0, ERRORS: [],
        coverage_stats: coverageStats
      })
    }))
  });

  // ── threshold enforcement ────────────────────────────────────────────────

  test('passes when coverage meets threshold', async () => {
    const unitCommand = require('../../src/commands/unit');
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '60'],
      makeMockContext({ total_lines: 100, covered_lines: 70, coverage_rate: 70 })
    );
    expect(consoleOutput.join('\n')).toContain('✅ Coverage 70% meets threshold 60%');
  });

  test('passes when coverage exactly equals threshold (boundary)', async () => {
    const unitCommand = require('../../src/commands/unit');
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '70'],
      makeMockContext({ total_lines: 100, covered_lines: 70, coverage_rate: 70 })
    );
    expect(consoleOutput.join('\n')).toContain('✅ Coverage 70% meets threshold 70%');
  });

  test('exits with code 1 in fail mode when coverage is below threshold', async () => {
    const unitCommand = require('../../src/commands/unit');
    await expect(
      unitCommand.execute(
        ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '80', '--coverage-mode', 'fail'],
        makeMockContext({ total_lines: 100, covered_lines: 50, coverage_rate: 50 })
      )
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.join('\n')).toContain('Coverage 50% is below threshold 80%');
  });

  test('fail is the default coverage-mode when not specified', async () => {
    const unitCommand = require('../../src/commands/unit');
    // --coverage-mode not specified → should default to fail
    await expect(
      unitCommand.execute(
        ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '80'],
        makeMockContext({ total_lines: 100, covered_lines: 50, coverage_rate: 50 })
      )
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.join('\n')).toContain('Coverage 50% is below threshold 80%');
  });

  test('warns but does not exit in warn mode when coverage is below threshold', async () => {
    const unitCommand = require('../../src/commands/unit');
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '80', '--coverage-mode', 'warn'],
      makeMockContext({ total_lines: 100, covered_lines: 50, coverage_rate: 50 })
    );
    expect(consoleWarns.join('\n')).toContain('Coverage 50% is below threshold 80%');
    // Should NOT have exited with error
    expect(consoleErrors.join('\n')).not.toContain('below threshold');
  });

  test('no enforcement when --coverage-threshold is 0 (default off)', async () => {
    const unitCommand = require('../../src/commands/unit');
    // threshold=0 means disabled — even with terrible coverage nothing fires
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage'],
      makeMockContext({ total_lines: 100, covered_lines: 1, coverage_rate: 1 })
    );
    expect(consoleOutput.join('\n')).not.toContain('below threshold');
    expect(consoleWarns.join('\n')).not.toContain('below threshold');
  });

  test('no enforcement when --coverage flag is not passed', async () => {
    const unitCommand = require('../../src/commands/unit');
    // Without --coverage the threshold flag is ignored
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage-threshold', '80'],
      makeMockContext(null)
    );
    expect(consoleOutput.join('\n')).not.toContain('threshold');
    expect(consoleWarns.join('\n')).not.toContain('threshold');
  });

  test('warns when coverage data is unavailable', async () => {
    const unitCommand = require('../../src/commands/unit');
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '80'],
      makeMockContext(null)
    );
    expect(consoleWarns.join('\n')).toContain('Coverage data unavailable');
  });

  // ── multi-file aggregation ───────────────────────────────────────────────

  test('aggregates coverage across multiple files before applying threshold', async () => {
    const unitCommand = require('../../src/commands/unit');
    // File 1: 40/100 (40%), File 2: 80/100 (80%) → aggregate 120/200 = 60%
    // Threshold is 55% → should pass
    const mockPost = jest.fn()
      .mockResolvedValueOnce({
        SUCCESS: 'X', TEST_COUNT: 5, PASSED_COUNT: 5, FAILED_COUNT: 0, ERRORS: [],
        coverage_stats: { total_lines: 100, covered_lines: 40, coverage_rate: 40 }
      })
      .mockResolvedValueOnce({
        SUCCESS: 'X', TEST_COUNT: 3, PASSED_COUNT: 3, FAILED_COUNT: 0, ERRORS: [],
        coverage_stats: { total_lines: 100, covered_lines: 80, coverage_rate: 80 }
      });
    const ctx = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token'),
        post: mockPost
      }))
    };
    await unitCommand.execute(
      ['--files', 'zcl_a.clas.testclasses.abap,zcl_b.clas.testclasses.abap',
       '--coverage', '--coverage-threshold', '55'],
      ctx
    );
    expect(consoleOutput.join('\n')).toContain('✅ Coverage 60% meets threshold 55%');
  });

  test('aggregated coverage below threshold fails the build', async () => {
    const unitCommand = require('../../src/commands/unit');
    // File 1: 20/100, File 2: 20/100 → aggregate 40/200 = 20% < 50%
    const mockPost = jest.fn()
      .mockResolvedValueOnce({
        SUCCESS: 'X', TEST_COUNT: 2, PASSED_COUNT: 2, FAILED_COUNT: 0, ERRORS: [],
        coverage_stats: { total_lines: 100, covered_lines: 20, coverage_rate: 20 }
      })
      .mockResolvedValueOnce({
        SUCCESS: 'X', TEST_COUNT: 2, PASSED_COUNT: 2, FAILED_COUNT: 0, ERRORS: [],
        coverage_stats: { total_lines: 100, covered_lines: 20, coverage_rate: 20 }
      });
    const ctx = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token'),
        post: mockPost
      }))
    };
    await expect(
      unitCommand.execute(
        ['--files', 'zcl_a.clas.testclasses.abap,zcl_b.clas.testclasses.abap',
         '--coverage', '--coverage-threshold', '50'],
        ctx
      )
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.join('\n')).toContain('Coverage 20% is below threshold 50%');
  });

  // ── uppercase ABAP key variant ───────────────────────────────────────────

  test('handles uppercase COVERAGE_STATS keys returned by ABAP', async () => {
    const unitCommand = require('../../src/commands/unit');
    const ctx = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X', TEST_COUNT: 5, PASSED_COUNT: 5, FAILED_COUNT: 0, ERRORS: [],
          COVERAGE_STATS: { TOTAL_LINES: 200, COVERED_LINES: 160, COVERAGE_RATE: 80 }
        })
      }))
    };
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '70'],
      ctx
    );
    expect(consoleOutput.join('\n')).toContain('✅ Coverage 80% meets threshold 70%');
  });

  // ── JUnit XML coverage properties ────────────────────────────────────────

  test('JUnit XML includes coverage properties when coverage data present', async () => {
    const fs = require('fs');
    const unitCommand = require('../../src/commands/unit');
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '60', '--junit-output', 'out.xml'],
      makeMockContext({ total_lines: 100, covered_lines: 70, coverage_rate: 70 })
    );
    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).toContain('<properties>');
    expect(writtenXml).toContain('name="coverage.rate"');
    expect(writtenXml).toContain('value="70"');
    expect(writtenXml).toContain('name="coverage.lines.total"');
    expect(writtenXml).toContain('value="100"');
    expect(writtenXml).toContain('name="coverage.lines.covered"');
    expect(writtenXml).toContain('value="70"');
  });

  test('JUnit XML has no coverage properties when coverage data absent', async () => {
    const fs = require('fs');
    const unitCommand = require('../../src/commands/unit');
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--junit-output', 'out.xml'],
      makeMockContext(null)
    );
    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).not.toContain('<properties>');
    expect(writtenXml).not.toContain('coverage.rate');
  });

  test('JUnit XML contains synthetic failure testcase when coverage gate fires in fail mode', async () => {
    const fs = require('fs');
    const unitCommand = require('../../src/commands/unit');
    let exitCode;
    jest.spyOn(process, 'exit').mockImplementation((code) => { exitCode = code; });
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '90', '--junit-output', 'out.xml'],
      makeMockContext({ total_lines: 10, covered_lines: 5, coverage_rate: 50 })
    );
    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).toContain('<testsuite name="Coverage"');
    expect(writtenXml).toContain('failures="1"');
    expect(writtenXml).toContain('<failure');
    expect(writtenXml).toContain('coverage_threshold');
    expect(writtenXml).toContain('Coverage 50% is below threshold 90%');
    expect(exitCode).toBe(1);
  });

  test('JUnit XML does NOT contain synthetic failure in warn mode', async () => {
    const fs = require('fs');
    const unitCommand = require('../../src/commands/unit');
    await unitCommand.execute(
      ['--files', 'zcl_t.clas.testclasses.abap', '--coverage', '--coverage-threshold', '90', '--coverage-mode', 'warn', '--junit-output', 'out.xml'],
      makeMockContext({ total_lines: 10, covered_lines: 5, coverage_rate: 50 })
    );
    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).not.toContain('coverage_threshold');
    expect(writtenXml).not.toContain('<testsuite name="Coverage"');
  });
});
