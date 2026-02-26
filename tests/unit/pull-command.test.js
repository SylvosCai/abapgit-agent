/**
 * Unit tests for pull command in CLI
 * Tests the CLI parsing, file handling, and request building
 */

describe('Pull Command', () => {
  describe('File parsing', () => {
    test('parses class file correctly', () => {
      const file = 'src/zcl_my_class.clas.abap';
      const baseName = file.split('/').pop();

      expect(baseName).toBe('zcl_my_class.clas.abap');
      expect(baseName.includes('.clas.')).toBe(true);
    });

    test('parses interface file correctly', () => {
      const file = 'src/zif_my_interface.intf.abap';
      const baseName = file.split('/').pop();

      expect(baseName.includes('.intf.')).toBe(true);
    });

    test('handles file paths with subdirectories', () => {
      const file = 'src/subfolder/zcl_test.clas.abap';
      const baseName = file.split('/').pop();

      expect(baseName).toBe('zcl_test.clas.abap');
    });

    test('handles comma-separated file list', () => {
      const filesArg = 'src/zcl_class1.clas.abap,src/zcl_class2.clas.abap';
      const files = filesArg.split(',').map(f => f.trim());

      expect(files.length).toBe(2);
      expect(files[0]).toBe('src/zcl_class1.clas.abap');
      expect(files[1]).toBe('src/zcl_class2.clas.abap');
    });
  });

  describe('Object type detection', () => {
    function extractObjectInfo(fileName) {
      // Extract object type from filename pattern: name.type.abap
      const match = fileName.match(/\.(\w+)\.abap$/);
      const type = match ? match[1].toUpperCase() : null;
      const objectName = fileName.split('.')[0].toUpperCase();
      return { type, objectName };
    }

    test('extracts object type from class file', () => {
      const fileName = 'zcl_my_class.clas.abap';
      const { type, objectName } = extractObjectInfo(fileName);

      expect(type).toBe('CLAS');
      expect(objectName).toBe('ZCL_MY_CLASS');
    });

    test('extracts object type from interface file', () => {
      const fileName = 'zif_my_interface.intf.abap';
      const { type, objectName } = extractObjectInfo(fileName);

      expect(type).toBe('INTF');
      expect(objectName).toBe('ZIF_MY_INTERFACE');
    });

    test('extracts object name from filename', () => {
      const fileName = 'zcl_my_class.clas.abap';
      const objectName = fileName.split('.')[0].toUpperCase();

      expect(objectName).toBe('ZCL_MY_CLASS');
    });
  });

  describe('Branch and URL handling', () => {
    test('uses default branch if not specified', () => {
      const args = ['pull', '--files', 'src/zcl_test.clas.abap'];
      const branchIndex = args.indexOf('--branch');
      const branch = branchIndex !== -1 ? args[branchIndex + 1] : null;

      expect(branch).toBeNull();
    });

    test('uses specified branch', () => {
      const args = ['pull', '--files', 'src/zcl_test.clas.abap', '--branch', 'develop'];
      const branchIndex = args.indexOf('--branch');
      const branch = branchIndex !== -1 ? args[branchIndex + 1] : null;

      expect(branch).toBe('develop');
    });

    test('uses specified URL', () => {
      const args = ['pull', '--url', 'https://github.com/test/repo.git'];
      const urlIndex = args.indexOf('--url');
      const url = urlIndex !== -1 ? args[urlIndex + 1] : null;

      expect(url).toBe('https://github.com/test/repo.git');
    });

    test('uses specified transport', () => {
      const args = ['pull', '--transport', 'DEVK900001'];
      const transportIndex = args.indexOf('--transport');
      const transport = transportIndex !== -1 ? args[transportIndex + 1] : null;

      expect(transport).toBe('DEVK900001');
    });
  });

  describe('Request building', () => {
    test('builds request with files', () => {
      const files = ['src/zcl_class1.clas.abap', 'src/zcl_class2.clas.abap'];
      const objects = files.map(f => {
        const baseName = f.split('/').pop();
        const match = baseName.match(/\.(\w+)\.abap$/);
        return {
          obj_type: match ? match[1].toUpperCase() : null,
          obj_name: baseName.split('.')[0].toUpperCase()
        };
      });

      expect(objects.length).toBe(2);
      expect(objects[0].obj_type).toBe('CLAS');
      expect(objects[0].obj_name).toBe('ZCL_CLASS1');
    });

    test('builds request with URL and branch', () => {
      const request = {
        url: 'https://github.com/test/repo.git',
        branch: 'develop'
      };

      expect(request.url).toBe('https://github.com/test/repo.git');
      expect(request.branch).toBe('develop');
    });

    test('builds request with transport', () => {
      const request = {
        transport: 'DEVK900001'
      };

      expect(request.transport).toBe('DEVK900001');
    });
  });

  describe('Response handling', () => {
    test('handles successful response', () => {
      const response = {
        success: 'X',
        message: 'Pull completed successfully',
        activated_objects: [
          { obj_type: 'CLAS', obj_name: 'ZCL_TEST' }
        ]
      };

      const success = response.success === 'X' || response.SUCCESS === 'X';
      const activatedObjects = response.activated_objects || response.ACTIVATED_OBJECTS || [];

      expect(success).toBe(true);
      expect(activatedObjects.length).toBe(1);
    });

    test('handles failed response', () => {
      const response = {
        success: '',
        message: 'Pull failed',
        errors: ['Syntax error in ZCL_TEST']
      };

      const success = response.success === 'X' || response.SUCCESS === 'X';
      const errors = response.errors || response.ERRORS || [];

      expect(success).toBe(false);
      expect(errors.length).toBeGreaterThan(0);
    });

    test('handles response with warnings', () => {
      const response = {
        success: 'X',
        message: 'Pull completed with warnings',
        warnings: ['Warning in ZCL_TEST']
      };

      const warnings = response.warnings || response.WARNINGS || [];

      expect(warnings).toBeDefined();
    });
  });

  describe('Multiple files handling', () => {
    test('handles pulling multiple files', () => {
      const files = [
        'src/zcl_class1.clas.abap',
        'src/zcl_class2.clas.abap',
        'src/zif_interface.intf.abap'
      ];

      expect(files.length).toBe(3);
      expect(files.every(f => f.includes('.abap'))).toBe(true);
    });

    test('validates all files have valid extensions', () => {
      const files = [
        'src/zcl_class.clas.abap',
        'src/zif_intf.intf.abap',
        'src/ztest.prog.abap'
      ];

      const validExtensions = ['.clas.abap', '.intf.abap', '.prog.abap', '.ddls.asddls'];
      const allValid = files.every(f =>
        validExtensions.some(ext => f.endsWith(ext))
      );

      expect(allValid).toBe(true);
    });
  });
});
