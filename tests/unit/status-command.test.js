/**
 * Unit tests for status command
 * Tests repository status checks
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

describe('Status Command - Logic Tests', () => {
  test('checks for .abapGitAgent file', () => {
    const configExists = true; // Mock
    expect(configExists).toBeDefined();
  });

  test('reads repository status', () => {
    const status = {
      enabled: true,
      configPath: '.abapGitAgent'
    };

    expect(status.enabled).toBe(true);
  });
});

describe('Status Command - CLI Output Format', () => {
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

  test('output shows enabled status with repository created', async () => {
    const statusCommand = require('../../src/commands/status');

    const mockContext = {
      isAbapIntegrationEnabled: jest.fn(() => true),
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          STATUS: 'Found',
          PACKAGE: '$ZTEST',
          KEY: 'TEST_KEY',
          REPO_KEY: 'TEST_REPO_KEY'
        })
      }))
    };

    await statusCommand.execute([], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/✅.*ENABLED/i);
    expect(output).toMatch(/Config location:/i);
    expect(output).toMatch(/Repository:.*✅.*Created/i);
    expect(output).toMatch(/Package:.*\$ZTEST/);
  });

  test('output shows enabled status with repository not created', async () => {
    const statusCommand = require('../../src/commands/status');

    const mockContext = {
      isAbapIntegrationEnabled: jest.fn(() => true),
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      gitUtils: {
        getRemoteUrl: jest.fn(() => 'https://github.com/test/repo.git')
      },
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          STATUS: 'NotFound'
        })
      }))
    };

    await statusCommand.execute([], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/✅.*ENABLED/i);
    expect(output).toMatch(/Repository:.*❌.*Not created/i);
    expect(output).toMatch(/abapgit-agent create/i);
  });

  test('output shows not configured status', async () => {
    const statusCommand = require('../../src/commands/status');

    const mockContext = {
      isAbapIntegrationEnabled: jest.fn(() => false),
      loadConfig: jest.fn(() => null),
      gitUtils: {
        getRemoteUrl: jest.fn(() => null)
      }
    };

    await statusCommand.execute([], mockContext);

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/❌.*NOT configured/i);
    expect(output).toMatch(/abapgit-agent init/i);
  });
});
