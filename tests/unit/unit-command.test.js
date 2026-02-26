/**
 * Unit tests for unit command
 * Tests AUnit test execution
 */

const verifiers = require('../helpers/verify-output-spec');

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
});
