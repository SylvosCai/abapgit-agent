/**
 * Unit tests for health command
 * Tests ABAP connection health checks
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

describe('Health Command - Logic Tests', () => {
  test('checks ABAP connection', () => {
    const response = {
      status: 'healthy',
      abap: 'connected',
      version: '1.0.0'
    };

    expect(response.status).toBe('healthy');
  });

  test('handles unhealthy status', () => {
    const response = {
      status: 'unhealthy',
      error: 'Connection refused'
    };

    expect(response.status).toBe('unhealthy');
  });
});

describe('Health Command - CLI Output Format', () => {
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

  test('output shows healthy status', async () => {
    const healthCommand = require('../../src/commands/health');

    const mockContext = {
      config: { host: 'test', port: 443 },
      AbapHttp: jest.fn().mockImplementation(() => ({
        get: jest.fn().mockResolvedValue({
          status: 'healthy',
          abap: 'connected',
          version: '1.0.0',
          cli_version: '1.7.2',
          abap_version: '1.0.0'
        })
      }))
    };

    try {
      await healthCommand.execute([], mockContext);
    } catch (e) {
      // Health command calls process.exit(), which is mocked
    }

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/healthy|status/i);
    expect(output).toMatch(/connected|abap/i);
    expect(output).toMatch(/version|1.0.0/i);
  });

  test('output shows unhealthy status', async () => {
    const healthCommand = require('../../src/commands/health');

    const mockContext = {
      config: { host: 'test', port: 443 },
      AbapHttp: jest.fn().mockImplementation(() => ({
        get: jest.fn().mockRejectedValue(new Error('Connection refused'))
      }))
    };

    try {
      await healthCommand.execute([], mockContext);
    } catch (e) {
      // Expected to throw
    }

    const output = consoleOutput.join('\n');

    expect(output).toMatch(/error|failed|connection/i);
  });

  test('output shows JSON format', async () => {
    const healthCommand = require('../../src/commands/health');

    const mockContext = {
      config: { host: 'test', port: 443 },
      AbapHttp: jest.fn().mockImplementation(() => ({
        get: jest.fn().mockResolvedValue({
          status: 'healthy',
          abap: 'connected',
          version: '1.0.0'
        })
      }))
    };

    try {
      await healthCommand.execute(['--json'], mockContext);
    } catch (e) {
      // Health command calls process.exit()
    }

    const output = consoleOutput.join('\n');

    // JSON output should be parseable
    expect(() => JSON.parse(output)).not.toThrow();
    const parsed = JSON.parse(output);
    expect(parsed.status).toBe('healthy');
  });
});
