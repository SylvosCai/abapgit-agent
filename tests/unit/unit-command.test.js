/**
 * Unit tests for unit command
 * Tests AUnit test execution
 */

const verifiers = require('../helpers/output-verifiers');

// Mock fs module
jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn(() => 'mock content')
}));

// Mock path module
jest.mock('path', () => ({
  isAbsolute: jest.fn(() => false),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => '/' + args.join('/')),
  basename: jest.fn((p) => p.split('/').pop())
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

    await unitCommand.execute(['--files', 'zcl_my_test.clas.testclasses.abap'], mockContext);

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

    await unitCommand.execute(['--files', 'zcl_test.clas.testclasses.abap', '--json', '--coverage'], mockContext);

    const output = consoleOutput.join('\n');
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

