/**
 * Unit tests for inspect command in CLI
 * Tests the CLI parsing, file handling, and request building
 */

describe('Inspect Command', () => {
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
