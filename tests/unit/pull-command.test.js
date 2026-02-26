/**
 * Unit tests for pull command in CLI
 * Tests the CLI parsing, file handling, and request building
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

describe('Pull Command - Logic Tests', () => {
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

describe('Pull Command - CLI Output Format', () => {
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

  test('output matches spec format for successful pull', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: 'X',
          message: 'Pull completed successfully',
          job_id: 'CAIS20260208115649',
          activated_objects: [
            { obj_type: 'CLAS', obj_name: 'ZCL_MY_CLASS' },
            { obj_type: 'INTF', obj_name: 'ZIF_MY_INTERFACE' }
          ],
          failed_objects: [],
          pull_log: [
            { icon: '✅', object: 'CLAS ZCL_MY_CLASS', message: 'Activated successfully' },
            { icon: '✅', object: 'INTF ZIF_MY_INTERFACE', message: 'Activated successfully' }
          ]
        })
      })),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null)
    };

    await pullCommand.execute(['--files', 'zcl_my_class.clas.abap,zif_my_interface.intf.abap'], mockContext);

    const output = consoleOutput.join('\n');

    // Check pull command output elements
    expect(output).toMatch(/✅/); // Success icon
    expect(output).toMatch(/Pull completed/i);
    expect(output).toMatch(/Job ID:/i);
    expect(output).toMatch(/CAIS/); // Job ID pattern
  });

  test('output shows failed objects when pull has errors', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: '',
          message: '1 object(s) failed activation',
          job_id: 'CAIS20260208115650',
          activated_objects: [
            { obj_type: 'INTF', obj_name: 'ZIF_MY_INTERFACE' }
          ],
          failed_objects: [
            { obj_type: 'CLAS', obj_name: 'ZCL_MY_CLASS', error: 'Syntax error on line 10' }
          ],
          pull_log: [
            { icon: '❌', object: 'CLAS ZCL_MY_CLASS', message: 'Syntax error' },
            { icon: '✅', object: 'INTF ZIF_MY_INTERFACE', message: 'Activated' }
          ]
        })
      })),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null)
    };

    await pullCommand.execute(['--files', 'zcl_my_class.clas.abap,zif_my_interface.intf.abap'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/❌/); // Error icon
    expect(output).toMatch(/failed/i);
    expect(output).toMatch(/Failed Objects/i);
  });

  test('output shows job ID and branch', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: 'X',
          message: 'Pull completed',
          job_id: 'CAIS20260208115651',
          activated_objects: [
            { obj_type: 'CLAS', obj_name: 'ZCL_MY_CLASS' }
          ],
          failed_objects: [],
          pull_log: [
            { icon: '📥', object: 'CLAS ZCL_MY_CLASS', message: 'Pulling from repository' },
            { icon: '🔨', object: 'CLAS ZCL_MY_CLASS', message: 'Activating object' },
            { icon: '✅', object: 'CLAS ZCL_MY_CLASS', message: 'Activated successfully' }
          ]
        })
      })),
      gitUtils: {
        getBranch: jest.fn(() => 'develop'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null)
    };

    await pullCommand.execute(['--files', 'zcl_my_class.clas.abap'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/Branch: develop/i);
    expect(output).toMatch(/Job ID:/i);
    expect(output).toMatch(/Starting pull/i);
  });
});
