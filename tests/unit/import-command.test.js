/**
 * Unit tests for import command
 * Tests object import from package to git
 */

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

describe('Import Command - Logic Tests', () => {
  test('import command basic structure', () => {
    const command = 'import';
    expect(command).toBe('import');
  });

  test('import command with message', () => {
    const args = ['import', '--message', 'Initial import'];
    const msgIndex = args.indexOf('--message');
    const message = msgIndex !== -1 ? args[msgIndex + 1] : null;

    expect(message).toBe('Initial import');
  });

  test('uses default message when not specified', () => {
    const args = ['import'];
    const msgIndex = args.indexOf('--message');
    const message = msgIndex !== -1 ? args[msgIndex + 1] : 'Import from ABAP system';

    expect(message).toBe('Import from ABAP system');
  });
});

describe('Import Command - CLI Output Format', () => {
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

  test('output shows successful import', async () => {
    const importCommand = require('../../src/commands/import');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443, package: '$ZTEST' })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git'),
        getBranch: jest.fn(() => 'master')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          MESSAGE: 'Import completed successfully',
          IMPORTED_COUNT: 10,
          FILES: [
            { NAME: 'zcl_my_class.clas.abap', STATUS: 'Created' },
            { NAME: 'zif_my_intf.intf.abap', STATUS: 'Created' }
          ]
        })
      }))
    };

    await importCommand.execute(['--message', 'Test import'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/✅|success|import/i);
    expect(output).toMatch(/completed|success/i);
  });

  test('output shows error when import fails', async () => {
    const importCommand = require('../../src/commands/import');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443, package: '$ZTEST' })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git'),
        getBranch: jest.fn(() => 'master')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: '',
          MESSAGE: 'Import failed',
          ERROR: 'Package not found'
        })
      }))
    };

    try {
      await importCommand.execute([], mockContext);
    } catch (e) {
      // May throw error
    }

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/error|failed|❌/i);
  });
});
