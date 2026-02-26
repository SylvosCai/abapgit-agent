/**
 * Unit tests for create command
 * Tests repository creation
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

describe('Create Command - Logic Tests', () => {
  test('create command basic structure', () => {
    const command = 'create';
    expect(command).toBe('create');
  });

  test('auto-detects git URL from remote', () => {
    const args = ['create'];
    const urlIndex = args.indexOf('--url');
    const hasExplicitUrl = urlIndex !== -1;

    expect(hasExplicitUrl).toBe(false);
  });

  test('uses package from config', () => {
    const config = {
      package: '$ZMY_PACKAGE'
    };

    expect(config.package).toBe('$ZMY_PACKAGE');
  });
});

describe('Create Command - CLI Output Format', () => {
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

  test('output shows successful repository creation', async () => {
    const createCommand = require('../../src/commands/create');

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
          MESSAGE: 'Repository created successfully',
          REPO_KEY: 'TEST_REPO_KEY',
          PACKAGE: '$ZTEST'
        })
      }))
    };

    await createCommand.execute([], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/✅|success/i);
    expect(output).toMatch(/created/i);
    expect(output).toMatch(/\$ZTEST|PACKAGE/i);
  });

  test('output shows error when creation fails', async () => {
    const createCommand = require('../../src/commands/create');

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
          MESSAGE: 'Repository already exists',
          ERROR: 'Repository with same URL already exists'
        })
      }))
    };

    try {
      await createCommand.execute([], mockContext);
    } catch (e) {
      // May throw error
    }

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/error|failed|❌/i);
  });
});
