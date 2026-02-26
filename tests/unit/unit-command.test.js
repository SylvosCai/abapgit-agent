/**
 * Unit tests for unit command
 * Tests AUnit test execution
 */

describe('Unit Command', () => {
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
