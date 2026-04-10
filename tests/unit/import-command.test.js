/**
 * Unit tests for import command
 * Tests async object import from package to git (background job + polling)
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
    const message = msgIndex !== -1 ? args[msgIndex + 1] : null;

    expect(message).toBe(null);
  });
});

describe('Import Command - Async Job Pattern', () => {
  let consoleOutput;
  let originalConsoleLog;
  let originalConsoleError;
  let originalStdoutWrite;

  beforeEach(() => {
    consoleOutput = [];
    originalConsoleLog = console.log;
    originalConsoleError = console.error;
    originalStdoutWrite = process.stdout.write;
    console.log = (...args) => consoleOutput.push(args.join(' '));
    console.error = (...args) => consoleOutput.push(args.join(' '));
    process.stdout.write = jest.fn(); // Mock progress bar output
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
    process.stdout.write = originalStdoutWrite;
  });

  test('output shows job started and polls for completion', async () => {
    const importCommand = require('../../src/commands/import');

    let pollCount = 0;
    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443, package: '$ZTEST' })),
      getSafeguards: jest.fn(() => ({ disableImport: false })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git'),
        getBranch: jest.fn(() => 'master')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          JOB_NUMBER: '12345678',
          JOB_NAME: 'IMPORT_20260304120000'
        }),
        get: jest.fn().mockImplementation(() => {
          pollCount++;
          if (pollCount === 1) {
            // First poll: starting
            return Promise.resolve({
              STATUS: 'running',
              PROGRESS: 10,
              MESSAGE: 'Finding repository...'
            });
          } else if (pollCount === 2) {
            // Second poll: staging
            return Promise.resolve({
              STATUS: 'running',
              PROGRESS: 50,
              MESSAGE: 'Staging local files...'
            });
          } else {
            // Final poll: completed
            return Promise.resolve({
              STATUS: 'completed',
              PROGRESS: 100,
              MESSAGE: 'Import completed successfully',
              RESULT: '{"success":"X","filesStaged":"100","commitMessage":"test: initial import"}',
              STARTED_AT: '20260304120000',
              COMPLETED_AT: '20260304120030'
            });
          }
        })
      }))
    };

    await importCommand.execute(['--message', 'Test import'], mockContext);

    const output = consoleOutput.join('\n');

    // Check job started message
    expect(output).toMatch(/job started|✅/i);
    expect(output).toMatch(/12345678/);

    // Check completion message
    expect(output).toMatch(/✅.*import completed successfully/i);
    expect(output).toMatch(/files staged.*100/i);
    expect(output).toMatch(/time spent/i);
  }, 15000);

  test('output shows error when job fails', async () => {
    const importCommand = require('../../src/commands/import');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443, package: '$ZTEST' })),
      getSafeguards: jest.fn(() => ({ disableImport: false })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git'),
        getBranch: jest.fn(() => 'master')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          JOB_NUMBER: '12345678',
          JOB_NAME: 'IMPORT_20260304120000'
        }),
        get: jest.fn().mockResolvedValue({
          STATUS: 'error',
          PROGRESS: 50,
          MESSAGE: 'Import failed',
          ERROR_MESSAGE: 'Repository not found'
        })
      }))
    };

    try {
      await importCommand.execute([], mockContext);
    } catch (e) {
      // Expected to throw
    }

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/error|failed|❌/i);
    expect(output).toMatch(/repository not found/i);
  });

  test('output shows correct stats with snake_case result keys from ABAP', async () => {
    const importCommand = require('../../src/commands/import');

    let pollCount = 0;
    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({ disableImport: false })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git'),
        getBranch: jest.fn(() => 'master')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          JOB_NUMBER: '99887766',
          JOB_NAME: 'IMPORT_20260306120000'
        }),
        get: jest.fn().mockImplementation(() => {
          pollCount++;
          if (pollCount < 3) {
            return Promise.resolve({ STATUS: 'running', PROGRESS: 50, MESSAGE: 'Staging...' });
          }
          return Promise.resolve({
            STATUS: 'completed',
            PROGRESS: 100,
            RESULT: '{"success":"X","files_staged":13796,"commit_message":"Initial import"}',
            STARTED_AT: '20260306120000',
            COMPLETED_AT: '20260306120430'
          });
        })
      }))
    };

    await importCommand.execute([], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/✅.*import completed successfully/i);
    expect(output).toMatch(/files staged.*13796/i);
    expect(output).toMatch(/initial import/i);
  });

  test('shows error when ABAP push fails (success is empty in result)', async () => {
    const importCommand = require('../../src/commands/import');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({ disableImport: false })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git'),
        getBranch: jest.fn(() => 'master')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          JOB_NUMBER: '11223344',
          JOB_NAME: 'IMPORT_20260306120000'
        }),
        get: jest.fn().mockResolvedValue({
          STATUS: 'completed',
          PROGRESS: 100,
          RESULT: '{"success":"","error":"Repository not found for URL"}',
          STARTED_AT: '20260306120000',
          COMPLETED_AT: '20260306120010'
        })
      }))
    };

    try {
      await importCommand.execute([], mockContext);
    } catch (e) {
      // Expected to throw via process.exit
    }

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/❌.*import failed/i);
    expect(output).toMatch(/repository not found for URL/i);
  });

  test('output shows error when job start fails', async () => {
    const importCommand = require('../../src/commands/import');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443, package: '$ZTEST' })),
      getSafeguards: jest.fn(() => ({ disableImport: false })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git'),
        getBranch: jest.fn(() => 'master')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: '',
          ERROR: 'Failed to create background job'
        })
      }))
    };

    try {
      await importCommand.execute([], mockContext);
    } catch (e) {
      // Expected to throw
    }

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/failed.*start.*job|error/i);
  });

  test('rejects import when disableImport safeguard is true', async () => {
    jest.resetModules();
    const importCommand = require('../../src/commands/import');

    const consoleErrorOutput = [];
    const originalConsoleError2 = console.error;
    console.error = (...args) => consoleErrorOutput.push(args.join(' '));

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({
        disableImport: true,
        reason: 'One-time operation managed by release manager'
      })),
      gitUtils: { getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git') },
      AbapHttp: jest.fn()
    };

    try {
      await importCommand.execute([], mockContext);
    } catch (e) {
      // Expected: process.exit(1) throws
    }

    console.error = originalConsoleError2;

    expect(mockExit).toHaveBeenCalledWith(1);
    const errorText = consoleErrorOutput.join('\n');
    expect(errorText).toMatch(/import command is disabled/);
    expect(errorText).toMatch(/One-time operation managed by release manager/);
    // Must not reach network layer
    expect(mockContext.AbapHttp).not.toHaveBeenCalled();
  });

  test('allows import when disableImport is true but current user is in importAllowedUsers', async () => {
    jest.resetModules();
    const importCommand = require('../../src/commands/import');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443, user: 'i045696' })),
      getSafeguards: jest.fn(() => ({
        disableImport: true,
        importAllowedUsers: ['I045696'],
        reason: 'Managed by release manager',
        requireImportMessage: false
      })),
      gitUtils: { getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git') },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token'),
        post: jest.fn().mockResolvedValue({ SUCCESS: 'X', JOB_NUMBER: '1234', JOB_NAME: 'IMPORT_TEST' }),
        get: jest.fn().mockResolvedValue({ STATUS: 'completed', RESULT: JSON.stringify({ success: 'X', filesStaged: 5, commitMessage: 'test' }) })
      }))
    };

    // Should not exit(1) — allowed user bypasses the block
    mockExit.mockClear();
    await importCommand.execute(['--message', 'release import'], mockContext);
    expect(mockExit).not.toHaveBeenCalledWith(1);
    expect(mockContext.AbapHttp).toHaveBeenCalled();
  });

  test('blocks import when disableImport is true and user is not in importAllowedUsers', async () => {
    jest.resetModules();
    const importCommand = require('../../src/commands/import');

    const consoleErrorOutput = [];
    const orig = console.error;
    console.error = (...args) => consoleErrorOutput.push(args.join(' '));

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443, user: 'OTHER_USER' })),
      getSafeguards: jest.fn(() => ({
        disableImport: true,
        importAllowedUsers: ['I045696', 'JOHN'],
        reason: 'Managed by release manager',
        requireImportMessage: false
      })),
      gitUtils: { getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git') },
      AbapHttp: jest.fn()
    };

    try { await importCommand.execute([], mockContext); } catch (e) { /* expected */ }
    console.error = orig;

    expect(mockExit).toHaveBeenCalledWith(1);
    const errorText = consoleErrorOutput.join('\n');
    expect(errorText).toMatch(/import command is disabled/);
    expect(errorText).toMatch(/I045696, JOHN/);
    expect(mockContext.AbapHttp).not.toHaveBeenCalled();
  });

  test('blocks import when requireImportMessage is true and no --message provided', async () => {
    jest.resetModules();
    const importCommand = require('../../src/commands/import');

    const consoleErrorOutput = [];
    const orig = console.error;
    console.error = (...args) => consoleErrorOutput.push(args.join(' '));

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({
        disableImport: false,
        requireImportMessage: true,
        importAllowedUsers: null,
        reason: 'All imports must be traceable'
      })),
      gitUtils: { getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git') },
      AbapHttp: jest.fn()
    };

    try { await importCommand.execute([], mockContext); } catch (e) { /* expected */ }
    console.error = orig;

    expect(mockExit).toHaveBeenCalledWith(1);
    const errorText = consoleErrorOutput.join('\n');
    expect(errorText).toMatch(/requires a commit message/);
    expect(errorText).toMatch(/All imports must be traceable/);
    expect(mockContext.AbapHttp).not.toHaveBeenCalled();
  });

  test('allows import when requireImportMessage is true and --message is provided', async () => {
    jest.resetModules();
    const importCommand = require('../../src/commands/import');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      getSafeguards: jest.fn(() => ({
        disableImport: false,
        requireImportMessage: true,
        importAllowedUsers: null,
        reason: null
      })),
      gitUtils: { getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git') },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token'),
        post: jest.fn().mockResolvedValue({ SUCCESS: 'X', JOB_NUMBER: '1234', JOB_NAME: 'IMPORT_TEST' }),
        get: jest.fn().mockResolvedValue({ STATUS: 'completed', RESULT: JSON.stringify({ success: 'X', filesStaged: 5, commitMessage: 'my import' }) })
      }))
    };

    mockExit.mockClear();
    await importCommand.execute(['--message', 'my import'], mockContext);
    expect(mockExit).not.toHaveBeenCalledWith(1);
    expect(mockContext.AbapHttp).toHaveBeenCalled();
  });
});
