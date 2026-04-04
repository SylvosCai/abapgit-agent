/**
 * Unit tests for drop command
 */

jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn(() => 'mock content')
}));

jest.mock('path', () => ({
  isAbsolute: jest.fn(() => false),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => '/' + args.join('/')),
  basename: jest.fn((p) => p.split('/').pop())
}));

const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

describe('Drop Command - objectFromFile helper', () => {
  // Access the internal helper by loading the module
  let dropCommand;
  beforeAll(() => {
    dropCommand = require('../../src/commands/drop');
  });

  test('module has correct name', () => {
    expect(dropCommand.name).toBe('drop');
  });

  test('module requires ABAP config', () => {
    expect(dropCommand.requiresAbapConfig).toBe(true);
  });
});

describe('Drop Command - missing --files', () => {
  let consoleOutput;
  let originalConsoleError;

  beforeEach(() => {
    consoleOutput = [];
    originalConsoleError = console.error;
    console.error = (...args) => consoleOutput.push(args.join(' '));
    jest.resetModules();
  });

  afterEach(() => {
    console.error = originalConsoleError;
  });

  test('exits with error when --files is missing', async () => {
    const dropCommand = require('../../src/commands/drop');
    const mockContext = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn(),
      gitUtils: {},
      getTransport: jest.fn(() => null)
    };

    await expect(dropCommand.execute([], mockContext))
      .rejects.toThrow('process.exit(1)');
    expect(consoleOutput.join('\n')).toMatch(/--files is required/);
  });

  test('exits with error when file does not exist', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(false);

    const dropCommand = require('../../src/commands/drop');
    const mockContext = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn(),
      gitUtils: {},
      getTransport: jest.fn(() => null)
    };

    await expect(dropCommand.execute(['--files', 'abap/nonexistent.clas.abap'], mockContext))
      .rejects.toThrow('process.exit(1)');
    expect(consoleOutput.join('\n')).toMatch(/file not found/);
  });

  test('exits with error for unrecognized file extension', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);

    const dropCommand = require('../../src/commands/drop');
    const mockContext = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn(),
      gitUtils: {},
      getTransport: jest.fn(() => null)
    };

    await expect(dropCommand.execute(['--files', 'abap/config.json'], mockContext))
      .rejects.toThrow('process.exit(1)');
    expect(consoleOutput.join('\n')).toMatch(/ABAP source file/);
  });

  test('exits with error for unsupported DTEL type', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);

    const dropCommand = require('../../src/commands/drop');
    const mockContext = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn(),
      gitUtils: {},
      getTransport: jest.fn(() => null)
    };

    await expect(dropCommand.execute(['--files', 'abap/zabgagt_drp_dtel.dtel.xml'], mockContext))
      .rejects.toThrow('process.exit(1)');
    expect(consoleOutput.join('\n')).toMatch(/does not support DTEL/);
  });
});

describe('Drop Command - success response', () => {
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
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  test('displays success message on successful drop', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);

    const dropCommand = require('../../src/commands/drop');
    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          OBJECT: 'ZCL_FOO',
          TYPE: 'CLAS',
          MESSAGE: 'Object deleted successfully'
        })
      })),
      gitUtils: {},
      getTransport: jest.fn(() => null)
    };

    await dropCommand.execute(['--files', 'abap/zcl_foo.clas.abap'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/✅/);
    expect(output).toMatch(/ZCL_FOO/);
  });

  test('displays error message on failed drop', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);

    const dropCommand = require('../../src/commands/drop');
    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: '',
          ERROR: 'Object ZCL_FOO (CLAS) not found in TADIR'
        })
      })),
      gitUtils: {},
      getTransport: jest.fn(() => null)
    };

    await expect(dropCommand.execute(['--files', 'abap/zcl_foo.clas.abap'], mockContext))
      .rejects.toThrow('process.exit(1)');
    expect(consoleOutput.join('\n')).toMatch(/not found in TADIR/);
  });

  test('--pull triggers pull command after successful drop', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);

    // Mock the pull module
    const mockPull = jest.fn().mockResolvedValue({ success: 'X' });
    jest.doMock('../../src/commands/pull', () => ({
      pull: mockPull
    }));

    const dropCommand = require('../../src/commands/drop');
    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          OBJECT: 'ZCL_FOO',
          TYPE: 'CLAS',
          MESSAGE: 'Object deleted successfully'
        })
      })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git'),
        getBranch: jest.fn(() => 'main')
      },
      getTransport: jest.fn(() => null)
    };

    await dropCommand.execute(['--files', 'abap/zcl_foo.clas.abap', '--pull'], mockContext);

    expect(mockPull).toHaveBeenCalledWith(
      'https://github.com/test/repo.git',
      'main',
      ['abap/zcl_foo.clas.abap'],
      null,
      expect.any(Function),
      expect.any(Function),
      false, undefined, 'abort', false, false
    );
  });
});
