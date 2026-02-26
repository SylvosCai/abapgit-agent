/**
 * Output Format Tests - Verify CLI output matches CLAUDE.md specifications
 *
 * These tests use mocks to ensure fast, deterministic verification of output formats.
 * Integration tests verify the commands actually work; these tests verify the OUTPUT
 * format matches the documented specification.
 */

const verifiers = require('../integration/verify-output-spec');

describe('CLI Output Format Verification', () => {
  let originalConsoleLog;
  let consoleOutput;

  beforeEach(() => {
    // Capture console output
    consoleOutput = [];
    originalConsoleLog = console.log;
    console.log = (...args) => {
      consoleOutput.push(args.join(' '));
    };
  });

  afterEach(() => {
    console.log = originalConsoleLog;
  });

  describe('inspect command output format', () => {
    test('should match spec format for passed syntax check', async () => {
      // Mock ABAP response
      const mockAbapResponse = {
        SUCCESS: true,
        COMMAND: 'INSPECT',
        MESSAGE: 'Syntax check completed',
        RESULTS: [
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
        ]
      };

      // Mock context
      const mockContext = {
        loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
        AbapHttp: jest.fn().mockImplementation(() => ({
          fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
          post: jest.fn().mockResolvedValue(mockAbapResponse)
        }))
      };

      // Execute command
      const inspectCommand = require('../../src/commands/inspect');
      await inspectCommand.execute(['--files', 'src/zcl_my_class.clas.abap'], mockContext);

      // Get output
      const output = consoleOutput.join('\n');

      // Verify format matches spec
      const verified = verifiers.verifyInspectOutput(output, 'ZCL_MY_CLASS');

      if (!verified) {
        console.log = originalConsoleLog;
        console.log('Expected format per CLAUDE.md:');
        console.log('✅ CLAS ZCL_MY_CLASS - Syntax check passed');
        console.log('\nActual output:');
        console.log(output);
      }

      expect(verified).toBe(true);
    });

    test('should match spec format for syntax check with warnings', async () => {
      const mockAbapResponse = {
        SUCCESS: true,
        COMMAND: 'INSPECT',
        MESSAGE: 'Syntax check completed',
        RESULTS: [
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
        ]
      };

      const mockContext = {
        loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
        AbapHttp: jest.fn().mockImplementation(() => ({
          fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
          post: jest.fn().mockResolvedValue(mockAbapResponse)
        }))
      };

      const inspectCommand = require('../../src/commands/inspect');
      await inspectCommand.execute(['--files', 'src/zcl_my_class.clas.abap'], mockContext);

      const output = consoleOutput.join('\n');
      const verified = verifiers.verifyInspectOutput(output, 'ZCL_MY_CLASS');

      // Should have warning emoji and section
      expect(output).toContain('⚠️');
      expect(output).toContain('Warnings:');
      expect(output).toContain('─'.repeat(20)); // Separator
      expect(verified).toBe(true);
    });

    test('should match spec format for failed syntax check', async () => {
      const mockAbapResponse = {
        SUCCESS: false,
        COMMAND: 'INSPECT',
        MESSAGE: 'Syntax check completed',
        RESULTS: [
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
        ]
      };

      const mockContext = {
        loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
        AbapHttp: jest.fn().mockImplementation(() => ({
          fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
          post: jest.fn().mockResolvedValue(mockAbapResponse)
        }))
      };

      const inspectCommand = require('../../src/commands/inspect');
      await inspectCommand.execute(['--files', 'src/zcl_my_class.clas.abap'], mockContext);

      const output = consoleOutput.join('\n');
      const verified = verifiers.verifyInspectOutput(output, 'ZCL_MY_CLASS');

      // Should have error emoji and section
      expect(output).toContain('❌');
      expect(output).toContain('Errors:');
      expect(output).toContain('─'.repeat(20)); // Separator
      expect(output).toContain('Line 21');
      expect(verified).toBe(true);
    });
  });

  describe('unit command output format', () => {
    test('should match spec format for all tests passed', async () => {
      const mockAbapResponse = {
        success: 'X',
        message: 'All tests passed',
        test_count: 10,
        passed_count: 10,
        failed_count: 0,
        errors: []
      };

      const mockContext = {
        loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
        AbapHttp: jest.fn().mockImplementation(() => ({
          fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
          post: jest.fn().mockResolvedValue(mockAbapResponse)
        }))
      };

      const unitCommand = require('../../src/commands/unit');
      await unitCommand.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], mockContext);

      const output = consoleOutput.join('\n');
      const verified = verifiers.verifyUnitOutput(output, 'ZCL_MY_TEST');

      expect(output).toContain('✅');
      expect(output).toContain('All tests passed');
      expect(output).toContain('Tests:');
      expect(output).toContain('Passed:');
      expect(output).toContain('Failed:');
      expect(verified).toBe(true);
    });

    test('should match spec format for failed tests', async () => {
      const mockAbapResponse = {
        success: '',
        message: '2 of 10 tests failed',
        test_count: 10,
        passed_count: 8,
        failed_count: 2,
        errors: [
          {
            class_name: 'ZCL_MY_TEST',
            method_name: 'TEST_METHOD_1',
            error_kind: 'ERROR',
            error_text: 'Expected X but got Y'
          },
          {
            class_name: 'ZCL_MY_TEST',
            method_name: 'TEST_METHOD_2',
            error_kind: 'ERROR',
            error_text: 'Another error'
          }
        ]
      };

      const mockContext = {
        loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
        AbapHttp: jest.fn().mockImplementation(() => ({
          fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
          post: jest.fn().mockResolvedValue(mockAbapResponse)
        }))
      };

      const unitCommand = require('../../src/commands/unit');
      await unitCommand.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], mockContext);

      const output = consoleOutput.join('\n');
      const verified = verifiers.verifyUnitOutput(output, 'ZCL_MY_TEST');

      expect(output).toContain('❌');
      expect(output).toContain('Tests failed');
      expect(output).toContain('✗');
      expect(output).toContain('=>'); // Method separator
      expect(verified).toBe(true);
    });
  });

  describe('syntax command output format', () => {
    test('should match spec format for passed syntax check', async () => {
      const mockAbapResponse = {
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
      };

      const mockContext = {
        loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
        AbapHttp: jest.fn().mockImplementation(() => ({
          fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
          post: jest.fn().mockResolvedValue(mockAbapResponse)
        }))
      };

      const syntaxCommand = require('../../src/commands/syntax');
      await syntaxCommand.execute(['--files', 'src/zcl_my_class.clas.abap'], mockContext);

      const output = consoleOutput.join('\n');
      const verified = verifiers.verifySyntaxOutput(output, 'ZCL_MY_CLASS');

      expect(output).toContain('Syntax check for');
      expect(output).toContain('file(s)');
      expect(output).toContain('✅');
      expect(output).toContain('All');
      expect(output).toContain('object(s) passed');
      expect(verified).toBe(true);
    });
  });

  describe('tree command output format', () => {
    test('should match spec format', async () => {
      const mockAbapResponse = {
        SUCCESS: true,
        COMMAND: 'TREE',
        PACKAGE: '$ZMAIN_PACKAGE',
        PARENT_PACKAGE: '$ZSAP_BASE',
        NODES: [
          { PACKAGE: '$ZMAIN_PACKAGE', PARENT: '', DEPTH: 0, OBJECT_COUNT: 11 },
          { PACKAGE: '$ZMAIN_SUB1', PARENT: '$ZMAIN_PACKAGE', DEPTH: 1, OBJECT_COUNT: 5 },
          { PACKAGE: '$ZMAIN_SUB2', PARENT: '$ZMAIN_PACKAGE', DEPTH: 1, OBJECT_COUNT: 3 }
        ],
        TOTAL_PACKAGES: 3,
        TOTAL_OBJECTS: 19,
        OBJECTS: [],
        ERROR: ''
      };

      const mockContext = {
        loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
        AbapHttp: jest.fn().mockImplementation(() => ({
          fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
          post: jest.fn().mockResolvedValue(mockAbapResponse)
        }))
      };

      const treeCommand = require('../../src/commands/tree');
      await treeCommand.execute(['--package', '$ZMAIN_PACKAGE'], mockContext);

      const output = consoleOutput.join('\n');
      const verified = verifiers.verifyTreeOutput(output, '$ZMAIN_PACKAGE');

      expect(output).toContain('📦');
      expect(output).toContain('├─');
      expect(output).toContain('Summary');
      expect(output).toContain('PACKAGES:');
      expect(output).toContain('OBJECTS:');
      expect(verified).toBe(true);
    });
  });
});
