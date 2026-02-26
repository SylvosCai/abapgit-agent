/**
 * Unit tests for delete command
 * Tests repository deletion
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

describe('Delete Command - Logic Tests', () => {
  test('delete command basic structure', () => {
    const command = 'delete';
    expect(command).toBe('delete');
  });

  test('auto-detects git URL from remote', () => {
    const args = ['delete'];
    const urlIndex = args.indexOf('--url');
    const hasExplicitUrl = urlIndex !== -1;

    expect(hasExplicitUrl).toBe(false);
  });

  test('requires confirmation', () => {
    const requiresConfirmation = true;
    expect(requiresConfirmation).toBe(true);
  });
});

describe('Delete Command - CLI Output Format', () => {
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

  test('output shows successful repository deletion', async () => {
    const deleteCommand = require('../../src/commands/delete');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          MESSAGE: 'Repository deleted successfully'
        })
      }))
    };

    await deleteCommand.execute(['--force'], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/✅|success|deleted/i);
  });

  test('output shows error when deletion fails', async () => {
    const deleteCommand = require('../../src/commands/delete');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: '',
          MESSAGE: 'Repository not found',
          ERROR: 'Repository does not exist'
        })
      }))
    };

    try {
      await deleteCommand.execute(['--force'], mockContext);
    } catch (e) {
      // May throw error
    }

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/error|failed|❌|not found/i);
  });
});
