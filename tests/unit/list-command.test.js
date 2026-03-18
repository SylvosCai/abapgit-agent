/**
 * Unit tests for list command
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

// Mock process.exit
const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

// Mock AbapHttp
const mockFetchCsrfToken = jest.fn();
const mockPost = jest.fn();

jest.mock('../../src/utils/abap-http', () => ({
  AbapHttp: jest.fn().mockImplementation(() => ({
    fetchCsrfToken: mockFetchCsrfToken,
    post: mockPost
  }))
}));

// Mock logger
jest.mock('../../src/logger', () => ({
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
  debug: jest.fn()
}));

beforeEach(() => {
  jest.clearAllMocks();
  mockFetchCsrfToken.mockResolvedValue('test-csrf-token');
});

describe('List Command - CLI Output Format', () => {
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

  test('output matches spec format for grouped display', async () => {
    const listCommand = require('../../src/commands/list');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          PACKAGE: '$ZTEST',
          OBJECTS: [
            { TYPE: 'CLAS', NAME: 'ZCL_CLASS1' },
            { TYPE: 'CLAS', NAME: 'ZCL_CLASS2' },
            { TYPE: 'INTF', NAME: 'ZIF_INTERFACE1' },
            { TYPE: 'TABL', NAME: 'ZMY_TABLE' }
          ],
          BY_TYPE: [
            { TYPE: 'CLAS', COUNT: 2 },
            { TYPE: 'INTF', COUNT: 1 },
            { TYPE: 'TABL', COUNT: 1 }
          ],
          TOTAL: 4
        })
      }))
    };

    await listCommand.execute(['--package', '$ZTEST'], mockContext);

    const output = consoleOutput.join('\n');

    // Remove debug output
    const verified = verifiers.verifyListOutput(output, '$ZTEST');
    expect(verified).toBe(true);

    // Additional specific checks for grouped format
    expect(output).toMatch(/Objects in \$ZTEST/);
    expect(output).toMatch(/CLAS \(2\)/); // Grouped heading with count
    expect(output).toMatch(/INTF \(1\)/);
    expect(output).toMatch(/TABL \(1\)/);
    expect(output).toMatch(/ZCL_CLASS1/); // Object names indented
    expect(output).toMatch(/ZCL_CLASS2/);
    expect(output).toMatch(/ZIF_INTERFACE1/);
    expect(output).toMatch(/ZMY_TABLE/);
  });

  test('output handles empty package', async () => {
    const listCommand = require('../../src/commands/list');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          PACKAGE: '$ZTEST',
          OBJECTS: [],
          BY_TYPE: [],
          TOTAL: 0
        })
      }))
    };

    await listCommand.execute(['--package', '$ZTEST'], mockContext);

    const output = consoleOutput.join('\n');

    // Empty package just shows header, no "No objects found" message
    expect(output).toMatch(/Objects in \$ZTEST/);
  });

  test('output includes type filter when specified', async () => {
    const listCommand = require('../../src/commands/list');

    const mockContext = {
      loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
        post: jest.fn().mockResolvedValue({
          SUCCESS: 'X',
          PACKAGE: '$ZTEST',
          OBJECTS: [
            { TYPE: 'CLAS', NAME: 'ZCL_CLASS1' },
            { TYPE: 'CLAS', NAME: 'ZCL_CLASS2' }
          ],
          BY_TYPE: [
            { TYPE: 'CLAS', COUNT: 2 }
          ],
          TOTAL: 2
        })
      }))
    };

    await listCommand.execute(['--package', '$ZTEST', '--type', 'CLAS'], mockContext);

    const output = consoleOutput.join('\n');

    // Type filter shows "(CLAS only)" in header
    expect(output).toMatch(/Objects in \$ZTEST.*CLAS only/);
    expect(output).toMatch(/CLAS \(2\)/);
  });
});
