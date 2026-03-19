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

// Mock format-error so we can verify printHttpError is called correctly
jest.mock('../../src/utils/format-error', () => ({
  printHttpError: jest.fn()
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

  test('shows error (not "already active") when message contains HTTP/SSL error', async () => {
    jest.resetModules();
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: '',
          message: 'HTTP error 421 occurred: SSL handshake with github.tools.sap:443 failed after 16ms: SSSLERR_PEER_CERT_UNTRUSTED (-102)',
          activated_count: 0,
          failed_count: 0,
          log_messages: [],
          activated_objects: [],
          failed_objects: []
        })
      }))
    };

    await expect(
      pullCommand.pull('https://github.tools.sap/test/repo.git', 'main', null, null,
        mockContext.loadConfig, mockContext.AbapHttp, false)
    ).rejects.toThrow();

    expect(consoleOutput.join('\n')).not.toContain('already active');
    expect(consoleOutput.join('\n')).toContain('SSL');
  });

  test('shows "already active" when message is "Activation cancelled"', async () => {
    jest.resetModules();
    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          success: '',
          message: 'Activation cancelled',
          activated_count: 0,
          failed_count: 0,
          log_messages: [],
          activated_objects: [],
          failed_objects: []
        })
      }))
    };

    await pullCommand.pull('https://github.tools.sap/test/repo.git', 'main', null, null,
      mockContext.loadConfig, mockContext.AbapHttp, false);

    expect(consoleOutput.join('\n')).toContain('already active');
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

  test('exits with error when a specified file does not exist on disk', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValueOnce(false); // first file missing

    const pullCommand = require('../../src/commands/pull');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false, reason: null })),
      AbapHttp: jest.fn(),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
    };

    await expect(async () => {
      await pullCommand.execute(['--files', 'abap/zcl_nonexistent.clas.abap'], mockContext);
    }).rejects.toThrow('process.exit(1)');

    const errorOutput = consoleErrorOutput.join('\n');
    expect(errorOutput).toMatch(/do not exist/);
    expect(errorOutput).toMatch(/zcl_nonexistent\.clas\.abap/);
  });

  test('skips file existence check when --url is explicitly provided', async () => {
    const pullCommand = require('../../src/commands/pull');

    const mockHttp = {
      fetchCsrfToken: jest.fn(() => 'token'),
      post: jest.fn(() => ({ success: true, message: 'Pull completed successfully', log_messages: [], activated_objects: [], failed_objects: [] }))
    };

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false, reason: null })),
      AbapHttp: jest.fn(() => mockHttp),
      gitUtils: {
        getBranch: jest.fn(() => 'master'),
        getRemoteUrl: jest.fn(() => null)
      },
      getTransport: jest.fn(() => null),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null })),
      getTransportSettings: jest.fn(() => ({}))
    };

    const fs = require('fs');
    // Should NOT call existsSync for the --files paths when --url is explicit
    await pullCommand.execute(
      ['--url', 'https://example.com/other-repo.git', '--files', 'src/zif_simple_test.intf.abap'],
      mockContext
    );
    const checkedPaths = fs.existsSync.mock.calls.map(c => c[0]);
    expect(checkedPaths.some(p => String(p).includes('zif_simple_test'))).toBe(false);
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

  const successPull = { success: 'X',  message: 'Done', log_messages: [], activated_objects: [] };

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

  test('exits with error when status check throws', async () => {
    const mockHttp = {
      fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
      post: jest.fn().mockImplementation((url) => {
        if (url.includes('/status')) return Promise.reject(new Error('Connection failed'));
        return Promise.resolve(successPull);
      })
    };
    const pullCommand = require('../../src/commands/pull');
    await expect(
      pullCommand.execute(['--files', 'src/zcl_test.clas.abap', '--url', 'https://github.com/test/repo.git'], baseContext(mockHttp))
    ).rejects.toThrow('process.exit(1)');

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/status check failed/);
    expect(output).toMatch(/Connection failed/);
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

describe('Pull Command - --verbose flag', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  const makeContext = (mockPost) => ({
    loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
    AbapHttp: jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
      post: mockPost
    })),
    gitUtils: {
      getBranch: jest.fn(() => 'master'),
      getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
    },
    getTransport: jest.fn(() => null),
    getTransportSettings: jest.fn(() => ({})),
    getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false })),
    getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
  });

  test('passes verbose: true to printHttpError when --verbose is set', async () => {
    const { printHttpError } = jest.requireMock('../../src/utils/format-error');
    const pullCommand = require('../../src/commands/pull');

    const httpError = new Error('Connection refused');
    const mockPost = jest.fn().mockImplementation((url) => {
      if (url.includes('/status')) return Promise.resolve({ success: true, status: 'Found', transport_required: false });
      return Promise.reject(httpError);
    });

    await expect(
      pullCommand.execute(['--verbose', '--files', 'src/zcl_test.clas.abap'], makeContext(mockPost))
    ).rejects.toThrow('process.exit(1)');

    expect(printHttpError).toHaveBeenCalledWith(httpError, { verbose: true });
  });

  test('passes verbose: false to printHttpError when --verbose is not set', async () => {
    const { printHttpError } = jest.requireMock('../../src/utils/format-error');
    const pullCommand = require('../../src/commands/pull');

    const httpError = new Error('Connection refused');
    const mockPost = jest.fn().mockImplementation((url) => {
      if (url.includes('/status')) return Promise.resolve({ success: true, status: 'Found', transport_required: false });
      return Promise.reject(httpError);
    });

    await expect(
      pullCommand.execute(['--files', 'src/zcl_test.clas.abap'], makeContext(mockPost))
    ).rejects.toThrow('process.exit(1)');

    expect(printHttpError).toHaveBeenCalledWith(httpError, { verbose: false });
  });
});

describe('Pull Command - .abapgit.xml detection', () => {
  let consoleOutput;
  let consoleWarnOutput;
  const originalConsoleLog = console.log;
  const originalConsoleWarn = console.warn;
  const originalConsoleError = console.error;

  beforeEach(() => {
    jest.clearAllMocks();
    consoleOutput = [];
    consoleWarnOutput = [];
    console.log = jest.fn((...args) => consoleOutput.push(args.join(' ')));
    console.warn = jest.fn((...args) => consoleWarnOutput.push(args.join(' ')));
    console.error = jest.fn();
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.warn = originalConsoleWarn;
    console.error = originalConsoleError;
  });

  const makeContext = (mockPost) => ({
    loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
    AbapHttp: jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('token'),
      post: mockPost
    })),
    gitUtils: {
      getBranch: jest.fn(() => 'master'),
      getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
    },
    getTransport: jest.fn(() => 'DEVK900001'),
    getTransportSettings: jest.fn(() => ({})),
    getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false })),
    getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null }))
  });

  const successResult = {
    success: 'X',
    message: 'Pull completed successfully',
    log_messages: [],
    activated_objects: [],
    failed_objects: []
  };

  test('warns when .abapgit.xml is missing (non-JSON mode)', async () => {
    const fs = require('fs');
    // .git exists, .abapgit.xml does not
    fs.existsSync.mockImplementation((p) => !String(p).endsWith('.abapgit.xml'));

    const pullCommand = require('../../src/commands/pull');
    const mockPost = jest.fn().mockResolvedValue(successResult);

    await pullCommand.pull(
      'https://github.com/test/repo.git', 'master', null, 'DEVK900001',
      jest.fn(() => ({ host: 'test', port: 443 })),
      jest.fn().mockImplementation(() => ({ fetchCsrfToken: jest.fn().mockResolvedValue('token'), post: mockPost })),
      false
    );

    const warnings = consoleWarnOutput.join('\n');
    expect(warnings).toMatch(/\.abapgit\.xml not found/);
    expect(warnings).toMatch(/ACTIVATED_COUNT=0/);
    expect(warnings).toMatch(/abapgit-agent init/);
  });

  test('does NOT warn when .abapgit.xml exists (non-JSON mode)', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true); // both .git and .abapgit.xml exist

    const pullCommand = require('../../src/commands/pull');
    const mockPost = jest.fn().mockResolvedValue(successResult);

    await pullCommand.pull(
      'https://github.com/test/repo.git', 'master', null, 'DEVK900001',
      jest.fn(() => ({ host: 'test', port: 443 })),
      jest.fn().mockImplementation(() => ({ fetchCsrfToken: jest.fn().mockResolvedValue('token'), post: mockPost })),
      false
    );

    expect(consoleWarnOutput).toHaveLength(0);
  });

  test('does NOT warn when not in a git repo (non-JSON mode)', async () => {
    const fs = require('fs');
    // neither .git nor .abapgit.xml exist
    fs.existsSync.mockReturnValue(false);

    const pullCommand = require('../../src/commands/pull');
    const mockPost = jest.fn().mockResolvedValue(successResult);

    await pullCommand.pull(
      'https://github.com/test/repo.git', 'master', null, 'DEVK900001',
      jest.fn(() => ({ host: 'test', port: 443 })),
      jest.fn().mockImplementation(() => ({ fetchCsrfToken: jest.fn().mockResolvedValue('token'), post: mockPost })),
      false
    );

    expect(consoleWarnOutput).toHaveLength(0);
  });

  test('sets missing_abapgit_xml: true in JSON output when .abapgit.xml is missing', async () => {
    const fs = require('fs');
    fs.existsSync.mockImplementation((p) => !String(p).endsWith('.abapgit.xml'));

    const pullCommand = require('../../src/commands/pull');
    const mockPost = jest.fn().mockResolvedValue({ ...successResult });

    const result = await pullCommand.pull(
      'https://github.com/test/repo.git', 'master', null, 'DEVK900001',
      jest.fn(() => ({ host: 'test', port: 443 })),
      jest.fn().mockImplementation(() => ({ fetchCsrfToken: jest.fn().mockResolvedValue('token'), post: mockPost })),
      true  // jsonOutput
    );

    expect(result.missing_abapgit_xml).toBe(true);
    const jsonPrinted = consoleOutput.join('\n');
    expect(JSON.parse(jsonPrinted)).toMatchObject({ missing_abapgit_xml: true });
  });

  test('does NOT set missing_abapgit_xml in JSON output when .abapgit.xml exists', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);

    const pullCommand = require('../../src/commands/pull');
    const mockPost = jest.fn().mockResolvedValue({ ...successResult });

    const result = await pullCommand.pull(
      'https://github.com/test/repo.git', 'master', null, 'DEVK900001',
      jest.fn(() => ({ host: 'test', port: 443 })),
      jest.fn().mockImplementation(() => ({ fetchCsrfToken: jest.fn().mockResolvedValue('token'), post: mockPost })),
      true  // jsonOutput
    );

    expect(result.missing_abapgit_xml).toBeUndefined();
  });

  test('does NOT warn in JSON mode even when .abapgit.xml is missing', async () => {
    const fs = require('fs');
    fs.existsSync.mockImplementation((p) => !String(p).endsWith('.abapgit.xml'));

    const pullCommand = require('../../src/commands/pull');
    const mockPost = jest.fn().mockResolvedValue({ ...successResult });

    await pullCommand.pull(
      'https://github.com/test/repo.git', 'master', null, 'DEVK900001',
      jest.fn(() => ({ host: 'test', port: 443 })),
      jest.fn().mockImplementation(() => ({ fetchCsrfToken: jest.fn().mockResolvedValue('token'), post: mockPost })),
      true  // jsonOutput
    );

    expect(consoleWarnOutput).toHaveLength(0);
  });
});

describe('Pull Command - calcWidth()', () => {
  let pullCommand;

  beforeEach(() => {
    jest.resetModules();
    pullCommand = require('../../src/commands/pull');
  });

  const calcWidth = () => pullCommand._calcWidth;

  test('returns 0 for null/undefined/empty string', () => {
    const fn = pullCommand._calcWidth;
    expect(fn(null)).toBe(0);
    expect(fn(undefined)).toBe(0);
    expect(fn('')).toBe(0);
  });

  test('counts ASCII characters as width 1 each', () => {
    expect(pullCommand._calcWidth('hello')).toBe(5);
    expect(pullCommand._calcWidth('abc')).toBe(3);
    expect(pullCommand._calcWidth('A')).toBe(1);
  });

  test('counts emoji as width 2', () => {
    expect(pullCommand._calcWidth('✅')).toBe(2);
    expect(pullCommand._calcWidth('❌')).toBe(2);
    expect(pullCommand._calcWidth('⚠️')).toBe(2);
  });

  test('counts supplementary plane emoji (U+1F4XX) as width 2', () => {
    // 🛑 is U+1F6D1 (> 0xFFFF) — two JS chars (surrogate pair), counts as width 2
    expect(pullCommand._calcWidth('🛑')).toBe(2);
  });

  test('skips variation selectors (zero-width)', () => {
    // ⚠️ is ⚠ (U+26A0) + variation selector U+FE0F
    // Total visual width = 2 (one wide char, variation selector adds 0)
    const w = pullCommand._calcWidth('⚠️');
    expect(w).toBe(2);
  });

  test('handles mixed ASCII + emoji string', () => {
    // '✅ OK' = emoji (2) + space (1) + 'O' (1) + 'K' (1) = 5
    expect(pullCommand._calcWidth('✅ OK')).toBe(5);
  });
});

describe('Pull Command - padToWidth()', () => {
  let pullCommand;

  beforeEach(() => {
    jest.resetModules();
    pullCommand = require('../../src/commands/pull');
  });

  test('pads short string to exact width with spaces', () => {
    const result = pullCommand._padToWidth('hi', 6);
    expect(result).toBe('hi    ');
    expect(result.length).toBe(6);
  });

  test('does not truncate string already at target width', () => {
    const result = pullCommand._padToWidth('hello', 5);
    expect(result).toBe('hello');
  });

  test('does not add negative padding (string wider than target)', () => {
    const result = pullCommand._padToWidth('hello world', 5);
    expect(result).toBe('hello world');
  });

  test('treats null/undefined as empty string', () => {
    expect(pullCommand._padToWidth(null, 3)).toBe('   ');
    expect(pullCommand._padToWidth(undefined, 3)).toBe('   ');
  });

  test('accounts for emoji display width when padding', () => {
    // '✅' has visual width 2, target 5 → needs 3 spaces
    const result = pullCommand._padToWidth('✅', 5);
    expect(result).toBe('✅   ');
  });
});

// ---------------------------------------------------------------------------
// Conflict detection logic
// ---------------------------------------------------------------------------

describe('Pull Command - Conflict Detection', () => {
  let consoleOutput;
  let originalConsoleLog;
  let originalConsoleError;
  let fs;

  beforeEach(() => {
    jest.resetModules();
    jest.mock('fs', () => ({ existsSync: jest.fn(() => true), readFileSync: jest.fn(() => 'mock content') }));
    jest.mock('path', () => ({ isAbsolute: jest.fn(() => false), join: jest.fn((...args) => args.join('/')), resolve: jest.fn((...args) => '/' + args.join('/')), basename: jest.fn((p) => p.split('/').pop()) }));
    jest.mock('../../src/utils/format-error', () => ({ printHttpError: jest.fn() }));
    jest.mock('../../src/utils/transport-selector', () => ({ selectTransport: jest.fn(), isNonInteractive: jest.fn(() => false), _getTransportHookConfig: jest.fn(() => null), interactivePicker: jest.fn(() => Promise.resolve(null)) }));
    fs = require('fs');
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

  function makeContext({ conflictSettingsMode = 'abort', postResult, postMock = null }) {
    const resolvedPost = postMock || jest.fn().mockResolvedValue(postResult);
    return {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token'),
        post: resolvedPost
      })),
      gitUtils: {
        getBranch: jest.fn(() => 'main'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false, reason: null })),
      getConflictSettings: jest.fn(() => ({ mode: conflictSettingsMode, reason: null })),
      _postMock: resolvedPost
    };
  }

  // ── conflict_mode sent in request body ────────────────────────────────────

  test('sends conflict_mode: abort by default (from getConflictSettings)', async () => {
    const pullCommand = require('../../src/commands/pull');
    const successResult = { success: 'X', message: 'Pull completed', activated_objects: [], failed_objects: [], pull_log: [] };
    const ctx = makeContext({ conflictSettingsMode: 'abort', postResult: successResult });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx);

    // post() is called twice: first for status check, second for the actual pull
    const pullCallBody = ctx._postMock.mock.calls.find(c => c[0].includes('/pull'))[1];
    expect(pullCallBody.conflict_mode).toBe('abort');
  });

  test('sends conflict_mode: ignore when project config returns ignore', async () => {
    const pullCommand = require('../../src/commands/pull');
    const successResult = { success: 'X', message: 'Pull completed', activated_objects: [], failed_objects: [], pull_log: [] };
    const ctx = makeContext({ conflictSettingsMode: 'ignore', postResult: successResult });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx);

    const pullCallBody = ctx._postMock.mock.calls.find(c => c[0].includes('/pull'))[1];
    expect(pullCallBody.conflict_mode).toBe('ignore');
  });

  test('--conflict-mode flag overrides project config', async () => {
    const pullCommand = require('../../src/commands/pull');
    const successResult = { success: 'X', message: 'Pull completed', activated_objects: [], failed_objects: [], pull_log: [] };
    // project config says abort, CLI flag says ignore
    const ctx = makeContext({ conflictSettingsMode: 'abort', postResult: successResult });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap', '--conflict-mode', 'ignore'], ctx);

    const pullCallBody = ctx._postMock.mock.calls.find(c => c[0].includes('/pull'))[1];
    expect(pullCallBody.conflict_mode).toBe('ignore');
  });

  test('--conflict-mode abort overrides project config ignore', async () => {
    const pullCommand = require('../../src/commands/pull');
    const successResult = { success: 'X', message: 'Pull completed', activated_objects: [], failed_objects: [], pull_log: [] };
    const ctx = makeContext({ conflictSettingsMode: 'ignore', postResult: successResult });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap', '--conflict-mode', 'abort'], ctx);

    const pullCallBody = ctx._postMock.mock.calls.find(c => c[0].includes('/pull'))[1];
    expect(pullCallBody.conflict_mode).toBe('abort');
  });

  // ── conflict response handling ────────────────────────────────────────────

  test('throws and prints warning when API returns conflict_count > 0', async () => {
    const pullCommand = require('../../src/commands/pull');
    const ctx = makeContext({
      postResult: {
        success: '',
        message: 'Pull aborted due to conflicts',
        conflict_count: 2,
        conflict_report: 'ZCL_FOO changed by USER1\nZCL_BAR changed by USER2'
      }
    });

    await expect(
      pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx)
    ).rejects.toThrow(/conflict/i);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/Pull aborted/i);
    expect(output).toMatch(/2 conflict/i);
  });

  test('includes conflict_report text in output', async () => {
    const pullCommand = require('../../src/commands/pull');
    const ctx = makeContext({
      postResult: {
        success: '',
        message: 'Pull aborted due to conflicts',
        conflict_count: 1,
        conflict_report: 'ZCL_MY_CLASS last changed by DEVELOPER1 on branch feature/x'
      }
    });

    await expect(
      pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx)
    ).rejects.toThrow();

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/ZCL_MY_CLASS/);
    expect(output).toMatch(/DEVELOPER1/);
  });

  test('uses CONFLICT_COUNT / CONFLICT_REPORT uppercase keys (ABAP response style)', async () => {
    const pullCommand = require('../../src/commands/pull');
    const ctx = makeContext({
      postResult: {
        SUCCESS: '',
        MESSAGE: 'Aborted',
        CONFLICT_COUNT: 1,
        CONFLICT_REPORT: 'Conflict on ZCL_FOO'
      }
    });

    await expect(
      pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx)
    ).rejects.toThrow();

    expect(consoleOutput.join('\n')).toMatch(/Conflict on ZCL_FOO/);
  });

  test('does NOT throw when conflict_count is 0 even if conflict_report present', async () => {
    const pullCommand = require('../../src/commands/pull');
    const ctx = makeContext({
      postResult: {
        success: 'X',
        message: 'Pull completed',
        conflict_count: 0,
        conflict_report: '',
        activated_objects: [], failed_objects: [], pull_log: []
      }
    });

    await expect(
      pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx)
    ).resolves.not.toThrow();
  });

  test('error has _isPullError flag set', async () => {
    const pullCommand = require('../../src/commands/pull');
    const ctx = makeContext({
      postResult: {
        success: '',
        message: 'Conflict detected',
        conflict_count: 1,
        conflict_report: 'ZCL_X changed by USER'
      }
    });

    let thrownError;
    try {
      await pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx);
    } catch (e) {
      thrownError = e;
    }

    expect(thrownError._isPullError).toBe(true);
  });
});

// ─── XML Sync Tests ───────────────────────────────────────────────────────────

describe('Pull Command - XML Sync (--sync-xml)', () => {
  let consoleOutput;
  let originalConsoleLog;
  let originalConsoleError;

  // Base64 helper
  const encode = (str) => Buffer.from(str).toString('base64');

  // Stable holder for execSync mock — prefixed "mock" so Jest's hoist allows it.
  const mockExecSyncHolder = { fn: jest.fn() };

  beforeEach(() => {
    jest.resetModules();
    mockExecSyncHolder.fn = jest.fn();

    jest.mock('child_process', () => ({ execSync: (...a) => mockExecSyncHolder.fn(...a) }));
    jest.mock('fs', () => ({
      existsSync: jest.fn(() => true),
      readFileSync: jest.fn(() => Buffer.from('original content')),
      writeFileSync: jest.fn()
    }));
    jest.mock('path', () => ({
      isAbsolute: jest.fn(() => false),
      join: jest.fn((...args) => args.join('/')),
      resolve: jest.fn((...args) => '/' + args.join('/')),
      basename: jest.fn((p) => p.split('/').pop())
    }));
    jest.mock('../../src/utils/format-error', () => ({ printHttpError: jest.fn() }));
    jest.mock('../../src/utils/transport-selector', () => ({
      selectTransport: jest.fn(),
      isNonInteractive: jest.fn(() => false),
      _getTransportHookConfig: jest.fn(() => null),
      interactivePicker: jest.fn(() => Promise.resolve(null))
    }));

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

  function makeXmlContext({ postResult, postMock = null }) {
    const resolvedPost = postMock || jest.fn().mockResolvedValue(postResult);
    return {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token'),
        post: resolvedPost
      })),
      gitUtils: {
        getBranch: jest.fn(() => 'main'),
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      getTransport: jest.fn(() => null),
      getSafeguards: jest.fn(() => ({ requireFilesForPull: false, disablePull: false, reason: null })),
      getConflictSettings: jest.fn(() => ({ mode: 'abort', reason: null })),
      getTransportSettings: jest.fn(() => ({})),
      _postMock: resolvedPost
    };
  }

  // ── no local_xml_files in response ─────────────────────────────────────────

  test('no warning when local_xml_files is absent', async () => {
    const pullCommand = require('../../src/commands/pull');
    const ctx = makeXmlContext({
      postResult: { success: 'X', message: 'Pull completed successfully' }
    });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx);

    const output = consoleOutput.join('\n');
    expect(output).not.toMatch(/XML file/i);
    expect(output).not.toMatch(/sync-xml/i);
  });

  test('no warning when local_xml_files is empty array', async () => {
    const pullCommand = require('../../src/commands/pull');
    const ctx = makeXmlContext({
      postResult: { success: 'X', message: 'Pull completed successfully', local_xml_files: [] }
    });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx);

    const output = consoleOutput.join('\n');
    expect(output).not.toMatch(/XML file/i);
  });

  // ── files differ, no --sync-xml ────────────────────────────────────────────

  test('prints warning when XML files differ and --sync-xml not passed', async () => {
    const pullCommand = require('../../src/commands/pull');
    const fs = require('fs');

    const serializedXml = '<xml>serialized</xml>';
    fs.readFileSync.mockReturnValue(Buffer.from('<xml>hand-crafted</xml>'));

    const ctx = makeXmlContext({
      postResult: {
        success: 'X',
        message: 'Pull completed successfully',
        local_xml_files: [
          { path: '/src/', filename: 'zcl_test.clas.xml', data: encode(serializedXml) }
        ]
      }
    });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/1 XML file\(s\) differ/);
    expect(output).toMatch(/src\/zcl_test\.clas\.xml/);
    expect(output).toMatch(/--sync-xml/);
    expect(fs.writeFileSync).not.toHaveBeenCalled();
  });

  test('lists all differing XML files in the warning', async () => {
    const pullCommand = require('../../src/commands/pull');
    const fs = require('fs');

    fs.readFileSync.mockReturnValue(Buffer.from('old'));

    const ctx = makeXmlContext({
      postResult: {
        success: 'X',
        message: 'Pull completed successfully',
        local_xml_files: [
          { path: '/src/', filename: 'zcl_a.clas.xml', data: encode('new-a') },
          { path: '/src/', filename: 'zcl_b.clas.xml', data: encode('new-b') }
        ]
      }
    });

    await pullCommand.execute(['--files', 'zcl_a.clas.abap,zcl_b.clas.abap'], ctx);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/2 XML file\(s\) differ/);
    expect(output).toMatch(/zcl_a\.clas\.xml/);
    expect(output).toMatch(/zcl_b\.clas\.xml/);
  });

  // ── file not on disk ────────────────────────────────────────────────────────

  test('skips XML file that does not exist on disk', async () => {
    const pullCommand = require('../../src/commands/pull');
    const fs = require('fs');

    fs.existsSync.mockImplementation((p) => !p.includes('zcl_test.clas.xml'));

    const ctx = makeXmlContext({
      postResult: {
        success: 'X',
        message: 'Pull completed successfully',
        local_xml_files: [
          { path: '/src/', filename: 'zcl_test.clas.xml', data: encode('<xml>new</xml>') }
        ]
      }
    });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx);

    const output = consoleOutput.join('\n');
    expect(output).not.toMatch(/XML file/i);
    expect(fs.writeFileSync).not.toHaveBeenCalled();
  });

  // ── bytes match after decode ────────────────────────────────────────────────

  test('no warning when bytes on disk already match serializer output', async () => {
    const pullCommand = require('../../src/commands/pull');
    const fs = require('fs');

    const content = '<xml>identical</xml>';
    fs.readFileSync.mockReturnValue(Buffer.from(content));

    const ctx = makeXmlContext({
      postResult: {
        success: 'X',
        message: 'Pull completed successfully',
        local_xml_files: [
          { path: '/src/', filename: 'zcl_test.clas.xml', data: encode(content) }
        ]
      }
    });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx);

    const output = consoleOutput.join('\n');
    expect(output).not.toMatch(/XML file/i);
    expect(fs.writeFileSync).not.toHaveBeenCalled();
  });

  // ── --sync-xml: write + amend + push + re-pull ──────────────────────────────

  test('--sync-xml writes files, stages, amends, pushes and re-pulls', async () => {
    const statusResponse = { status: 'Found', transport_required: false };
    const postMock = jest.fn()
      .mockResolvedValueOnce(statusResponse)           // status check (execute)
      .mockResolvedValueOnce({                          // pull
        success: 'X',
        message: 'Pull completed successfully',
        local_xml_files: [
          { path: '/src/', filename: 'zcl_test.clas.xml', data: encode('<xml>serialized</xml>') }
        ]
      })
      .mockResolvedValueOnce({                          // re-pull (this.pull() skips status check)
        success: 'X',
        message: 'Pull completed successfully',
        local_xml_files: []
      });

    const pullCommand = require('../../src/commands/pull');
    const fs = require('fs');
    fs.readFileSync.mockReturnValue(Buffer.from('<xml>hand-crafted</xml>'));

    const ctx = makeXmlContext({ postMock });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap', '--sync-xml'], ctx);

    // 1. File written with serializer content
    expect(fs.writeFileSync).toHaveBeenCalledWith(
      expect.stringContaining('zcl_test.clas.xml'),
      Buffer.from('<xml>serialized</xml>')
    );

    // 2. git add called with the file path
    expect(mockExecSyncHolder.fn).toHaveBeenCalledWith(
      expect.stringMatching(/git add.*zcl_test\.clas\.xml/),
      expect.any(Object)
    );

    // 3. git commit --amend called
    expect(mockExecSyncHolder.fn).toHaveBeenCalledWith(
      'git commit --amend --no-edit',
      expect.any(Object)
    );

    // 4. git push --force-with-lease called
    expect(mockExecSyncHolder.fn).toHaveBeenCalledWith(
      'git push --force-with-lease',
      expect.any(Object)
    );

    // 5. ABAP HTTP post: status + pull + re-pull = 3
    // (re-pull calls this.pull() directly, skipping the status check in execute())
    expect(postMock).toHaveBeenCalledTimes(3);

    // 6. Success message printed
    const output = consoleOutput.join('\n');
    expect(output).toMatch(/Synced 1 XML file\(s\)/);
    expect(output).toMatch(/amended commit/);
    expect(output).toMatch(/re-pulled/);
  });

  test('--sync-xml retries with --set-upstream when branch has no upstream', async () => {
    const statusResponse = { status: 'Found', transport_required: false };
    const postMock = jest.fn()
      .mockResolvedValueOnce(statusResponse)
      .mockResolvedValueOnce({
        success: 'X',
        message: 'Pull completed successfully',
        local_xml_files: [
          { path: '/src/', filename: 'zcl_test.clas.xml', data: encode('<xml>new</xml>') }
        ]
      })
      .mockResolvedValueOnce({
        success: 'X',
        message: 'Pull completed successfully',
        local_xml_files: []
      });

    mockExecSyncHolder.fn.mockImplementation((cmd) => {
      if (cmd === 'git push --force-with-lease') {
        const err = new Error('no upstream branch');
        err.stderr = 'fatal: The current branch feature/foo has no upstream branch.';
        throw err;
      }
      // --set-upstream push succeeds
    });

    const pullCommand = require('../../src/commands/pull');
    const fs = require('fs');
    fs.readFileSync.mockReturnValue(Buffer.from('<xml>old</xml>'));

    const ctx = makeXmlContext({ postMock });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap', '--sync-xml'], ctx);

    // --force-with-lease --set-upstream push was called
    expect(mockExecSyncHolder.fn).toHaveBeenCalledWith(
      expect.stringMatching(/git push --force-with-lease --set-upstream origin/),
      expect.any(Object)
    );

    // Re-pull happened: status + pull + re-pull = 3
    expect(postMock).toHaveBeenCalledTimes(3);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/Synced 1 XML file\(s\)/);
    expect(output).toMatch(/re-pulled/);
  });

  test('--sync-xml skips push and re-pull when no remote exists', async () => {
    const statusResponse = { status: 'Found', transport_required: false };
    const postMock = jest.fn()
      .mockResolvedValueOnce(statusResponse)
      .mockResolvedValueOnce({
        success: 'X',
        message: 'Pull completed successfully',
        local_xml_files: [
          { path: '/src/', filename: 'zcl_test.clas.xml', data: encode('<xml>new</xml>') }
        ]
      });

    mockExecSyncHolder.fn.mockImplementation((cmd) => {
      if (cmd === 'git push --force-with-lease') throw new Error('remote: not found');
      // --set-upstream would also fail for truly no remote, but we never reach it
    });

    const pullCommand = require('../../src/commands/pull');
    const fs = require('fs');
    fs.readFileSync.mockReturnValue(Buffer.from('<xml>old</xml>'));

    const ctx = makeXmlContext({ postMock });

    await expect(
      pullCommand.execute(['--files', 'zcl_test.clas.abap', '--sync-xml'], ctx)
    ).resolves.not.toThrow();

    // No re-pull — only 2 HTTP calls (status + pull)
    expect(postMock).toHaveBeenCalledTimes(2);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/Synced 1 XML file\(s\)/);
    expect(output).toMatch(/Push skipped/);
    expect(output).not.toMatch(/re-pulled/);
  });

  test('--sync-xml does nothing when no files differ', async () => {
    const content = '<xml>same</xml>';

    const postMock = jest.fn().mockResolvedValue({
      success: 'X',
      message: 'Pull completed successfully',
      local_xml_files: [
        { path: '/src/', filename: 'zcl_test.clas.xml', data: encode(content) }
      ]
    });

    const pullCommand = require('../../src/commands/pull');
    const fs = require('fs');
    fs.readFileSync.mockReturnValue(Buffer.from(content));

    const ctx = makeXmlContext({ postMock });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap', '--sync-xml'], ctx);

    expect(fs.writeFileSync).not.toHaveBeenCalled();
    expect(mockExecSyncHolder.fn).not.toHaveBeenCalled();
    // postMock is called twice: once for status check, once for the actual pull
    expect(postMock).toHaveBeenCalledTimes(2);  });

  // ── uppercase keys (ABAP JSON) ──────────────────────────────────────────────

  test('handles uppercase LOCAL_XML_FILES from ABAP JSON serialization', async () => {
    const pullCommand = require('../../src/commands/pull');
    const fs = require('fs');
    fs.readFileSync.mockReturnValue(Buffer.from('old'));

    const ctx = makeXmlContext({
      postResult: {
        success: 'X',
        message: 'Pull completed successfully',
        LOCAL_XML_FILES: [
          { PATH: '/src/', FILENAME: 'zcl_test.clas.xml', DATA: encode('new') }
        ]
      }
    });

    await pullCommand.execute(['--files', 'zcl_test.clas.abap'], ctx);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/1 XML file\(s\) differ/);
  });

  // ── --sync-xml flag detection ───────────────────────────────────────────────

  test('--sync-xml flag is detected in args', () => {
    const args = ['--files', 'zcl_test.clas.abap', '--sync-xml'];
    expect(args.includes('--sync-xml')).toBe(true);
  });

  test('--sync-xml flag absent by default', () => {
    const args = ['--files', 'zcl_test.clas.abap'];
    expect(args.includes('--sync-xml')).toBe(false);
  });
});
