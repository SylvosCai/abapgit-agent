/**
 * Unit tests for pull command in CLI
 * Tests the CLI parsing, file handling, and request building
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

// Mock transport-selector for transport_required tests
jest.mock('../../src/utils/transport-selector', () => ({
  selectTransport: jest.fn(),
  isNonInteractive: jest.fn(() => false),
  _getTransportHookConfig: jest.fn(() => null),
  interactivePicker: jest.fn(() => Promise.resolve(null))
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

  // Helper that mirrors the --files validation in pull.js
  function findNonSourceFiles(files) {
    const ABAP_SOURCE_EXTS = new Set(['abap', 'asddls']);
    return files.filter(f => {
      const base = f.split('/').pop();
      const parts = base.split('.');
      const ext = parts[parts.length - 1].toLowerCase();
      return parts.length < 3 || !ABAP_SOURCE_EXTS.has(ext);
    });
  }

  describe('Non-source file rejection', () => {
    test('accepts .clas.abap files', () => {
      expect(findNonSourceFiles(['src/zcl_my_class.clas.abap'])).toHaveLength(0);
    });

    test('accepts .intf.abap files', () => {
      expect(findNonSourceFiles(['zif_my_intf.intf.abap'])).toHaveLength(0);
    });

    test('accepts .ddls.asddls files', () => {
      expect(findNonSourceFiles(['zc_my_view.ddls.asddls'])).toHaveLength(0);
    });

    test('accepts .testclasses.abap files', () => {
      expect(findNonSourceFiles(['zcl_foo.clas.testclasses.abap'])).toHaveLength(0);
    });

    test('rejects .clas.xml files', () => {
      const bad = findNonSourceFiles(['abap/zcl_abgagt_agent.clas.xml']);
      expect(bad).toHaveLength(1);
      expect(bad[0]).toBe('abap/zcl_abgagt_agent.clas.xml');
    });

    test('rejects .intf.xml files', () => {
      expect(findNonSourceFiles(['zif_foo.intf.xml'])).toHaveLength(1);
    });

    test('rejects files with only one dot-segment', () => {
      expect(findNonSourceFiles(['zcl_foo.abap'])).toHaveLength(1);
    });

    test('rejects mixed list — flags only the invalid file', () => {
      const bad = findNonSourceFiles([
        'src/zcl_good.clas.abap',
        'abap/zcl_abgagt_agent.clas.xml',
      ]);
      expect(bad).toHaveLength(1);
      expect(bad[0]).toBe('abap/zcl_abgagt_agent.clas.xml');
    });

    test('accepts a list where every file is a valid source file', () => {
      expect(findNonSourceFiles([
        'src/zcl_a.clas.abap',
        'src/zif_b.intf.abap',
        'src/zc_c.ddls.asddls',
      ])).toHaveLength(0);
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
      getTransport: jest.fn(() => null),
      getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false, reason: null })),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
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
      getTransport: jest.fn(() => null),
      getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false, reason: null })),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    await expect(
      pullCommand.execute(['--files', 'zcl_my_class.clas.abap,zif_my_interface.intf.abap'], mockContext)
    ).rejects.toThrow('1 object(s) failed activation');

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
      getTransport: jest.fn(() => null),
      getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false, reason: null })),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    await pullCommand.execute(['--files', 'zcl_my_class.clas.abap'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/Branch: develop/i);
    expect(output).toMatch(/Job ID:/i);
    expect(output).toMatch(/Starting pull/i);
  });

  test('JSON output mode suppresses progress messages', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          JOB_ID: 'CAIS20260301120000',
          MESSAGE: 'Pull completed successfully',
          ACTIVATED_COUNT: 1,
          FAILED_COUNT: 0,
          ACTIVATED_OBJECTS: [
            { OBJ_TYPE: 'CLAS', OBJ_NAME: 'ZCL_MY_CLASS' }
          ]
        })
      })),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false, reason: null })),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    await pullCommand.execute(['--files', 'zcl_my_class.clas.abap', '--json'], mockContext);

    const output = consoleOutput.join('\n');

    // JSON mode should NOT show progress messages
    expect(output).not.toMatch(/🚀/);
    expect(output).not.toMatch(/Starting pull/i);
    expect(output).not.toMatch(/Branch:/i);

    // Should only contain JSON
    expect(() => JSON.parse(output)).not.toThrow();
    const parsed = JSON.parse(output);
    expect(parsed.SUCCESS).toBe('X');
    expect(parsed.JOB_ID).toBe('CAIS20260301120000');
  });

  test('JSON output mode returns complete result object', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockResult = {
      SUCCESS: 'X',
      JOB_ID: 'CAIS20260301120000',
      MESSAGE: 'Pull completed successfully',
      ACTIVATED_COUNT: 2,
      FAILED_COUNT: 0,
      ACTIVATED_OBJECTS: [
        { OBJ_TYPE: 'CLAS', OBJ_NAME: 'ZCL_CLASS1' },
        { OBJ_TYPE: 'INTF', OBJ_NAME: 'ZIF_INTERFACE' }
      ],
      LOG_MESSAGES: [
        { TYPE: 'S', OBJ_TYPE: 'CLAS', OBJ_NAME: 'ZCL_CLASS1', TEXT: 'Activated' },
        { TYPE: 'S', OBJ_TYPE: 'INTF', OBJ_NAME: 'ZIF_INTERFACE', TEXT: 'Activated' }
      ]
    };

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue(mockResult)
      })),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false, reason: null })),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    await pullCommand.execute(['--files', 'zcl_class1.clas.abap', '--json'], mockContext);

    const output = consoleOutput.join('\n');
    const parsed = JSON.parse(output);

    // Verify all expected fields are present
    expect(parsed.SUCCESS).toBe('X');
    expect(parsed.ACTIVATED_COUNT).toBe(2);
    expect(parsed.FAILED_COUNT).toBe(0);
    expect(parsed.ACTIVATED_OBJECTS).toHaveLength(2);
    expect(parsed.LOG_MESSAGES).toHaveLength(2);
  });
});

describe('Pull Command - Safeguard Validation', () => {
  let consoleOutput = [];
  let consoleErrorOutput = [];
  const originalConsoleLog = console.log;
  const originalConsoleError = console.error;

  beforeEach(() => {
    consoleOutput = [];
    consoleErrorOutput = [];
    console.log = jest.fn((...args) => consoleOutput.push(args.join(' ')));
    console.error = jest.fn((...args) => consoleErrorOutput.push(args.join(' ')));
    jest.clearAllMocks();
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  test('rejects pull without --files when requireFilesForPull is true', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({
        requireFilesForPull: true,
        disablePull: false,
        reason: 'Large project with 500+ objects'
      })),
      AbapHttp: jest.fn(),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    await expect(async () => {
      await pullCommand.execute([], mockContext);
    }).rejects.toThrow('process.exit(1)');

    const errorOutput = consoleErrorOutput.join('\n');
    expect(errorOutput).toMatch(/--files parameter is required/);
    expect(errorOutput).toMatch(/Large project with 500\+ objects/);
    expect(errorOutput).toMatch(/\.abapgit-agent\.json/);
  });

  test('allows pull with --files when requireFilesForPull is true', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({
        requireFilesForPull: true,
        disablePull: false,
        reason: null
      })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: 'X',
          message: 'Success'
        })
      })),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    // Should NOT throw
    await pullCommand.execute(['--files', 'zcl_test.clas.abap'], mockContext);

    // Should have attempted to make the pull request
    expect(mockContext.AbapHttp).toHaveBeenCalled();
  });

  test('rejects all pull commands when disablePull is true', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({
        requireFilesForPull: false,
        disablePull: true,
        reason: 'CI/CD only'
      })),
      AbapHttp: jest.fn(),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    // Should reject even with --files
    await expect(async () => {
      await pullCommand.execute(['--files', 'zcl_test.clas.abap'], mockContext);
    }).rejects.toThrow('process.exit(1)');

    const errorOutput = consoleErrorOutput.join('\n');
    expect(errorOutput).toMatch(/pull command is disabled/);
    expect(errorOutput).toMatch(/CI\/CD only/);
  });

  test('allows pull when no safeguards are configured', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({
        requireFilesForPull: false,
        disablePull: false,
        reason: null
      })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: 'X',
          message: 'Success'
        })
      })),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    // Should work without --files when no safeguards
    await pullCommand.execute([], mockContext);

    expect(mockContext.AbapHttp).toHaveBeenCalled();
  });

  test('shows reason in error message when provided', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({
        requireFilesForPull: true,
        disablePull: false,
        reason: 'Project contains 100+ ABAP objects'
      })),
      AbapHttp: jest.fn(),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    await expect(async () => {
      await pullCommand.execute([], mockContext);
    }).rejects.toThrow('process.exit(1)');

    const errorOutput = consoleErrorOutput.join('\n');
    expect(errorOutput).toMatch(/Reason: Project contains 100\+ ABAP objects/);
  });
});

describe('Pull Command - Transport Required Detection', () => {
  let consoleOutput = [];
  const originalConsoleLog = console.log;
  const originalConsoleError = console.error;

  beforeEach(() => {
    consoleOutput = [];
    console.log = jest.fn((...args) => consoleOutput.push(args.join(' ')));
    console.error = jest.fn((...args) => consoleOutput.push(args.join(' ')));
    jest.clearAllMocks();
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  const makeMockHttp = (statusResult, pullResult) => ({
    fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
    post: jest.fn().mockImplementation((url) => {
      if (url.includes('/status')) return Promise.resolve(statusResult);
      return Promise.resolve(pullResult);
    })
  });

  const baseContext = (mockHttp) => ({
    loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
    getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false })),
    AbapHttp: jest.fn().mockImplementation(() => mockHttp),
    gitUtils: {
      getBranch: jest.fn(() => 'main'),
      getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
    },
    getTransport: jest.fn(() => null),
    getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
  });

  const successPull = { success: 'X', job_id: 'JOB1', message: 'Done', log_messages: [], activated_objects: [] };

  test('skips transport selector when transport_required is false', async () => {
    const { selectTransport } = jest.requireMock('../../src/utils/transport-selector');
    selectTransport.mockResolvedValue(null);

    const mockHttp = makeMockHttp(
      { success: true, status: 'Found', transport_required: false },
      successPull
    );
    const pullCommand = require('../../src/commands/pull');
    await pullCommand.execute(['--files', 'src/zcl_test.clas.abap', '--url', 'https://github.com/test/repo.git'], baseContext(mockHttp));

    expect(selectTransport).not.toHaveBeenCalled();
  });

  test('skips transport selector when transport_required is string "false"', async () => {
    const { selectTransport } = jest.requireMock('../../src/utils/transport-selector');
    selectTransport.mockResolvedValue(null);

    const mockHttp = makeMockHttp(
      { success: true, status: 'Found', transport_required: 'false' },
      successPull
    );
    const pullCommand = require('../../src/commands/pull');
    await pullCommand.execute(['--files', 'src/zcl_test.clas.abap', '--url', 'https://github.com/test/repo.git'], baseContext(mockHttp));

    expect(selectTransport).not.toHaveBeenCalled();
  });

  test('runs transport selector when transport_required is true', async () => {
    const { selectTransport } = jest.requireMock('../../src/utils/transport-selector');
    selectTransport.mockResolvedValue('DEVK123456');

    const mockHttp = makeMockHttp(
      { success: true, status: 'Found', transport_required: true },
      successPull
    );
    const pullCommand = require('../../src/commands/pull');
    await pullCommand.execute(['--files', 'src/zcl_test.clas.abap', '--url', 'https://github.com/test/repo.git'], baseContext(mockHttp));

    expect(selectTransport).toHaveBeenCalled();
  });

  test('runs transport selector as safe default when status check throws', async () => {
    const { selectTransport } = jest.requireMock('../../src/utils/transport-selector');
    selectTransport.mockResolvedValue('DEVK999999');

    const mockHttp = {
      fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
      post: jest.fn().mockImplementation((url) => {
        if (url.includes('/status')) return Promise.reject(new Error('Connection failed'));
        return Promise.resolve(successPull);
      })
    };
    const pullCommand = require('../../src/commands/pull');
    await pullCommand.execute(['--files', 'src/zcl_test.clas.abap', '--url', 'https://github.com/test/repo.git'], baseContext(mockHttp));

    expect(selectTransport).toHaveBeenCalled();
  });

  test('fails with error when hook is configured but returns no transport', async () => {
    const { selectTransport, isNonInteractive, _getTransportHookConfig } = jest.requireMock('../../src/utils/transport-selector');
    selectTransport.mockResolvedValue(null);
    isNonInteractive.mockReturnValue(true);
    _getTransportHookConfig.mockReturnValue({ hook: './scripts/get-transport.js', description: 'Sprint transport' });

    const mockHttp = makeMockHttp(
      { success: true, status: 'Found', transport_required: true },
      successPull
    );
    const pullCommand = require('../../src/commands/pull');

    await expect(
      pullCommand.execute(['--files', 'src/zcl_test.clas.abap', '--url', 'https://github.com/test/repo.git'], baseContext(mockHttp))
    ).rejects.toThrow('process.exit(1)');

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/transport hook returned no transport request/);
    expect(output).toMatch(/scripts\/get-transport\.js/);
  });

  test('warns and falls through to picker when hook returns null in interactive (TTY) mode', async () => {
    const { selectTransport, isNonInteractive, _getTransportHookConfig, interactivePicker } = jest.requireMock('../../src/utils/transport-selector');
    selectTransport.mockResolvedValue(null);
    isNonInteractive.mockReturnValue(false);  // TTY / manual mode
    _getTransportHookConfig.mockReturnValue({ hook: './scripts/get-transport.js' });
    interactivePicker.mockResolvedValue('DEVK900001');

    const mockHttp = makeMockHttp(
      { success: true, status: 'Found', transport_required: true },
      successPull
    );
    const pullCommand = require('../../src/commands/pull');

    // Should not throw — falls through to picker
    await pullCommand.execute(['--files', 'src/zcl_test.clas.abap', '--url', 'https://github.com/test/repo.git'], baseContext(mockHttp));
    expect(mockExit).not.toHaveBeenCalled();
    expect(interactivePicker).toHaveBeenCalled();
  });
});
