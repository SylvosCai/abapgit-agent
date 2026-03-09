/**
 * Unit tests for transport command
 * Tests subcommand dispatch, output formatting, and error handling
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

describe('Transport Command - Subcommand dispatch', () => {
  let transportCommand;
  let mockHttp;
  let mockContext;
  let consoleOutput;
  let consoleErrors;

  beforeEach(() => {
    jest.resetModules();
    jest.spyOn(process, 'exit').mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
    consoleOutput = [];
    consoleErrors = [];
    jest.spyOn(console, 'log').mockImplementation((...args) => consoleOutput.push(args.join(' ')));
    jest.spyOn(console, 'error').mockImplementation((...args) => consoleErrors.push(args.join(' ')));

    mockHttp = {
      get: jest.fn(),
      post: jest.fn(),
      fetchCsrfToken: jest.fn().mockResolvedValue('mock-csrf-token')
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com', user: 'DEV' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp),
      getTransportSettings: jest.fn().mockReturnValue({ allowCreate: true, allowRelease: true, reason: null })
    };

    transportCommand = require('../../src/commands/transport');
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('command has correct name and description', () => {
    expect(transportCommand.name).toBe('transport');
    expect(transportCommand.description).toMatch(/transport/i);
    expect(transportCommand.requiresAbapConfig).toBe(true);
  });

  test('default subcommand is list with scope=mine when no args', async () => {
    mockHttp.get.mockResolvedValue({
      success: true,
      action: 'LIST',
      scope: 'mine',
      transports: []
    });

    await transportCommand.execute([], mockContext);

    expect(mockHttp.get).toHaveBeenCalled();
    // Should include scope=mine in the request
    const callArg = mockHttp.get.mock.calls[0][0];
    expect(callArg).toMatch(/scope=mine/);
  });

  test('"list" subcommand calls GET with scope=mine by default', async () => {
    mockHttp.get.mockResolvedValue({
      success: true,
      action: 'LIST',
      scope: 'mine',
      transports: [
        { number: 'DEVK900001', description: 'Feature X', owner: 'DEVELOPER', date: '2026-03-09' }
      ]
    });

    await transportCommand.execute(['list'], mockContext);

    const callArg = mockHttp.get.mock.calls[0][0];
    expect(callArg).toMatch(/scope=mine/);
  });

  test('"list --scope task" calls GET with scope=task', async () => {
    mockHttp.get.mockResolvedValue({
      success: true,
      action: 'LIST',
      scope: 'task',
      transports: []
    });

    await transportCommand.execute(['list', '--scope', 'task'], mockContext);

    const callArg = mockHttp.get.mock.calls[0][0];
    expect(callArg).toMatch(/scope=task/);
  });

  test('"list --scope all" calls GET with scope=all', async () => {
    mockHttp.get.mockResolvedValue({
      success: true,
      action: 'LIST',
      scope: 'all',
      transports: []
    });

    await transportCommand.execute(['list', '--scope', 'all'], mockContext);

    const callArg = mockHttp.get.mock.calls[0][0];
    expect(callArg).toMatch(/scope=all/);
  });

  test('"create" subcommand calls POST with action=CREATE', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      action: 'CREATE',
      number: 'DEVK900099',
      message: 'Transport DEVK900099 created'
    });

    await transportCommand.execute(['create', '--description', 'Sprint 42'], mockContext);

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.stringContaining('/transport'),
      expect.objectContaining({ action: 'CREATE', description: 'Sprint 42' }),
      expect.anything()
    );
  });

  test('"check --number DEVK900001" calls POST with action=CHECK', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      action: 'CHECK',
      number: 'DEVK900001',
      passed: true,
      issues: []
    });

    await transportCommand.execute(['check', '--number', 'DEVK900001'], mockContext);

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.stringContaining('/transport'),
      expect.objectContaining({ action: 'CHECK', number: 'DEVK900001' }),
      expect.anything()
    );
  });

  test('"release --number DEVK900001" calls POST with action=RELEASE', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      action: 'RELEASE',
      number: 'DEVK900001',
      message: 'Released successfully'
    });

    await transportCommand.execute(['release', '--number', 'DEVK900001'], mockContext);

    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.stringContaining('/transport'),
      expect.objectContaining({ action: 'RELEASE', number: 'DEVK900001' }),
      expect.anything()
    );
  });
});

describe('Transport Command - Output formatting', () => {
  let transportCommand;
  let mockHttp;
  let mockContext;
  let consoleOutput;
  let consoleErrors;

  beforeEach(() => {
    jest.resetModules();
    jest.spyOn(process, 'exit').mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
    consoleOutput = [];
    consoleErrors = [];
    jest.spyOn(console, 'log').mockImplementation((...args) => consoleOutput.push(args.join(' ')));
    jest.spyOn(console, 'error').mockImplementation((...args) => consoleErrors.push(args.join(' ')));

    mockHttp = {
      get: jest.fn(),
      post: jest.fn(),
      fetchCsrfToken: jest.fn().mockResolvedValue('mock-csrf-token')
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com', user: 'DEV' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp),
      getTransportSettings: jest.fn().mockReturnValue({ allowCreate: true, allowRelease: true, reason: null })
    };

    transportCommand = require('../../src/commands/transport');
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('list output shows transport numbers', async () => {
    mockHttp.get.mockResolvedValue({
      success: true,
      action: 'LIST',
      scope: 'mine',
      transports: [
        { number: 'DEVK900001', description: 'Feature X', owner: 'DEVELOPER', date: '2026-03-09' },
        { number: 'DEVK900002', description: 'Bug fix', owner: 'DEVELOPER', date: '2026-03-07' }
      ]
    });

    await transportCommand.execute(['list'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toContain('DEVK900001');
    expect(output).toContain('DEVK900002');
    expect(output).toContain('Feature X');
  });

  test('list output shows transport count', async () => {
    mockHttp.get.mockResolvedValue({
      success: true,
      action: 'LIST',
      scope: 'mine',
      transports: [
        { number: 'DEVK900001', description: 'Feature X', owner: 'DEVELOPER', date: '2026-03-09' }
      ]
    });

    await transportCommand.execute(['list'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/1 transport/i);
  });

  test('list output shows "no transports found" when empty', async () => {
    mockHttp.get.mockResolvedValue({
      success: true,
      action: 'LIST',
      scope: 'mine',
      transports: []
    });

    await transportCommand.execute(['list'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/no open transport/i);
  });

  test('create output shows transport number on success', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      action: 'CREATE',
      number: 'DEVK900099',
      message: 'Transport DEVK900099 created'
    });

    await transportCommand.execute(['create', '--description', 'Sprint 42'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toContain('DEVK900099');
    expect(output).toMatch(/created/i);
  });

  test('check output shows "passed" when no issues', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      action: 'CHECK',
      number: 'DEVK900001',
      passed: true,
      issues: []
    });

    await transportCommand.execute(['check', '--number', 'DEVK900001'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toMatch(/passed|no issues/i);
  });

  test('check output shows issues when present', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      action: 'CHECK',
      number: 'DEVK900001',
      passed: false,
      issues: [
        { type: 'E', obj_type: 'CLAS', obj_name: 'ZCL_MY_CLASS', text: 'Syntax error' }
      ]
    });

    await transportCommand.execute(['check', '--number', 'DEVK900001'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toContain('ZCL_MY_CLASS');
    expect(output).toMatch(/syntax error/i);
  });

  test('release output shows success message', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      action: 'RELEASE',
      number: 'DEVK900001',
      message: 'Released successfully'
    });

    await transportCommand.execute(['release', '--number', 'DEVK900001'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toContain('DEVK900001');
    expect(output).toMatch(/released/i);
  });
});

describe('Transport Command - JSON output', () => {
  let transportCommand;
  let mockHttp;
  let mockContext;
  let consoleOutput;

  beforeEach(() => {
    jest.resetModules();
    jest.spyOn(process, 'exit').mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
    consoleOutput = [];
    jest.spyOn(console, 'log').mockImplementation((...args) => consoleOutput.push(args.join(' ')));
    jest.spyOn(console, 'error').mockImplementation(() => {});

    mockHttp = {
      get: jest.fn(),
      post: jest.fn(),
      fetchCsrfToken: jest.fn().mockResolvedValue('mock-csrf-token')
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com', user: 'DEV' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp),
      getTransportSettings: jest.fn().mockReturnValue({ allowCreate: true, allowRelease: true, reason: null })
    };

    transportCommand = require('../../src/commands/transport');
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('list --json outputs raw JSON', async () => {
    const apiResponse = {
      success: true,
      action: 'LIST',
      scope: 'mine',
      transports: [
        { number: 'DEVK900001', description: 'Feature X', owner: 'DEVELOPER', date: '2026-03-09' }
      ]
    };
    mockHttp.get.mockResolvedValue(apiResponse);

    await transportCommand.execute(['list', '--json'], mockContext);

    const output = consoleOutput.join('\n');
    const parsed = JSON.parse(output);
    expect(parsed.success).toBe(true);
    expect(parsed.transports).toHaveLength(1);
    expect(parsed.transports[0].number).toBe('DEVK900001');
  });

  test('create --json outputs raw JSON', async () => {
    const apiResponse = {
      success: true,
      action: 'CREATE',
      number: 'DEVK900099',
      message: 'Transport DEVK900099 created'
    };
    mockHttp.post.mockResolvedValue(apiResponse);

    await transportCommand.execute(['create', '--description', 'Sprint 42', '--json'], mockContext);

    const output = consoleOutput.join('\n');
    const parsed = JSON.parse(output);
    expect(parsed.success).toBe(true);
    expect(parsed.number).toBe('DEVK900099');
  });

  test('check --json outputs raw JSON', async () => {
    const apiResponse = {
      success: true,
      action: 'CHECK',
      number: 'DEVK900001',
      passed: true,
      issues: []
    };
    mockHttp.post.mockResolvedValue(apiResponse);

    await transportCommand.execute(['check', '--number', 'DEVK900001', '--json'], mockContext);

    const output = consoleOutput.join('\n');
    const parsed = JSON.parse(output);
    expect(parsed.passed).toBe(true);
    expect(parsed.issues).toHaveLength(0);
  });

  test('release --json outputs raw JSON', async () => {
    const apiResponse = {
      success: true,
      action: 'RELEASE',
      number: 'DEVK900001',
      message: 'Released successfully'
    };
    mockHttp.post.mockResolvedValue(apiResponse);

    await transportCommand.execute(['release', '--number', 'DEVK900001', '--json'], mockContext);

    const output = consoleOutput.join('\n');
    const parsed = JSON.parse(output);
    expect(parsed.action).toBe('RELEASE');
    expect(parsed.number).toBe('DEVK900001');
  });
});

describe('Transport Command - Error handling', () => {
  let transportCommand;
  let mockHttp;
  let mockContext;
  let consoleErrors;

  beforeEach(() => {
    jest.resetModules();
    jest.spyOn(process, 'exit').mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
    consoleErrors = [];
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation((...args) => consoleErrors.push(args.join(' ')));

    mockHttp = {
      get: jest.fn(),
      post: jest.fn(),
      fetchCsrfToken: jest.fn().mockResolvedValue('mock-csrf-token')
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com', user: 'DEV' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp),
      getTransportSettings: jest.fn().mockReturnValue({ allowCreate: true, allowRelease: true, reason: null })
    };

    transportCommand = require('../../src/commands/transport');
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('unknown subcommand shows error', async () => {
    await expect(
      transportCommand.execute(['xyz'], mockContext)
    ).rejects.toThrow(/process.exit/);

    const errors = consoleErrors.join('\n');
    expect(errors).toMatch(/unknown subcommand/i);
    expect(errors).toMatch(/xyz/);
  });

  test('invalid --scope value shows error', async () => {
    await expect(
      transportCommand.execute(['list', '--scope', 'invalid'], mockContext)
    ).rejects.toThrow(/process.exit/);

    const errors = consoleErrors.join('\n');
    expect(errors).toMatch(/invalid scope/i);
  });

  test('"check" without --number shows error', async () => {
    await expect(
      transportCommand.execute(['check'], mockContext)
    ).rejects.toThrow(/process.exit/);

    const errors = consoleErrors.join('\n');
    expect(errors).toMatch(/--number.*required/i);
  });

  test('"release" without --number shows error', async () => {
    await expect(
      transportCommand.execute(['release'], mockContext)
    ).rejects.toThrow(/process.exit/);

    const errors = consoleErrors.join('\n');
    expect(errors).toMatch(/--number.*required/i);
  });

  test('ABAP connection failure shows error', async () => {
    mockHttp.get.mockRejectedValue(new Error('Connection refused'));

    await expect(
      transportCommand.execute(['list'], mockContext)
    ).rejects.toThrow(/process.exit/);

    const errors = consoleErrors.join('\n');
    expect(errors).toMatch(/error/i);
  });

  test('create failure shows error', async () => {
    mockHttp.post.mockRejectedValue(new Error('Could not create transport'));

    await expect(
      transportCommand.execute(['create', '--description', 'Test'], mockContext)
    ).rejects.toThrow(/process.exit/);

    const errors = consoleErrors.join('\n');
    expect(errors).toMatch(/error/i);
  });

  test('release failure shows error message', async () => {
    mockHttp.post.mockResolvedValue({
      success: false,
      action: 'RELEASE',
      number: 'DEVK900001',
      error: 'Transport has open tasks'
    });

    await transportCommand.execute(['release', '--number', 'DEVK900001'], mockContext);

    const errors = consoleErrors.join('\n');
    expect(errors).toMatch(/open tasks|error/i);
  });
});

describe('Transport Command - ABAP uppercase key handling', () => {
  let transportCommand;
  let mockHttp;
  let mockContext;
  let consoleOutput;

  beforeEach(() => {
    jest.resetModules();
    jest.spyOn(process, 'exit').mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
    consoleOutput = [];
    jest.spyOn(console, 'log').mockImplementation((...args) => consoleOutput.push(args.join(' ')));
    jest.spyOn(console, 'error').mockImplementation(() => {});

    mockHttp = {
      get: jest.fn(),
      post: jest.fn(),
      fetchCsrfToken: jest.fn().mockResolvedValue('mock-csrf-token')
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com', user: 'DEV' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp),
      getTransportSettings: jest.fn().mockReturnValue({ allowCreate: true, allowRelease: true, reason: null })
    };

    transportCommand = require('../../src/commands/transport');
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('handles ABAP uppercase keys in list response', async () => {
    mockHttp.get.mockResolvedValue({
      SUCCESS: true,
      ACTION: 'LIST',
      SCOPE: 'mine',
      TRANSPORTS: [
        { NUMBER: 'DEVK900001', DESCRIPTION: 'Feature X', OWNER: 'DEVELOPER', DATE: '2026-03-09' }
      ]
    });

    await transportCommand.execute(['list'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toContain('DEVK900001');
    expect(output).toContain('Feature X');
  });

  test('handles ABAP uppercase keys in create response', async () => {
    mockHttp.post.mockResolvedValue({
      SUCCESS: true,
      ACTION: 'CREATE',
      NUMBER: 'DEVK900099',
      MESSAGE: 'Transport created'
    });

    await transportCommand.execute(['create', '--description', 'Sprint 42'], mockContext);

    const output = consoleOutput.join('\n');
    expect(output).toContain('DEVK900099');
  });
});

describe('Transport Command - Project config enforcement', () => {
  let transportCommand;
  let mockHttp;
  let mockContext;
  let consoleErrors;

  beforeEach(() => {
    jest.resetModules();
    jest.spyOn(process, 'exit').mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
    consoleErrors = [];
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation((...args) => consoleErrors.push(args.join(' ')));

    mockHttp = {
      get: jest.fn(),
      post: jest.fn(),
      fetchCsrfToken: jest.fn().mockResolvedValue('mock-csrf-token')
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com', user: 'DEV' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp),
      getTransportSettings: jest.fn().mockReturnValue({ allowCreate: true, allowRelease: true, reason: null })
    };

    transportCommand = require('../../src/commands/transport');
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('allowCreate=false blocks create and exits with error', async () => {
    mockContext.getTransportSettings.mockReturnValue({ allowCreate: false, allowRelease: true, reason: null });

    await expect(
      transportCommand.execute(['create', '--description', 'Test'], mockContext)
    ).rejects.toThrow(/transport create disabled/);

    expect(mockHttp.post).not.toHaveBeenCalled();
    const errors = consoleErrors.join('\n');
    expect(errors).toMatch(/transport create is disabled/i);
  });

  test('allowRelease=false blocks release and exits with error', async () => {
    mockContext.getTransportSettings.mockReturnValue({ allowCreate: true, allowRelease: false, reason: null });

    await expect(
      transportCommand.execute(['release', '--number', 'DEVK900001'], mockContext)
    ).rejects.toThrow(/transport release disabled/);

    expect(mockHttp.post).not.toHaveBeenCalled();
    const errors = consoleErrors.join('\n');
    expect(errors).toMatch(/transport release is disabled/i);
  });

  test('reason is shown in error when provided', async () => {
    mockContext.getTransportSettings.mockReturnValue({
      allowCreate: false,
      allowRelease: true,
      reason: 'Only CI/CD pipeline may create transports'
    });

    await expect(
      transportCommand.execute(['create', '--description', 'Test'], mockContext)
    ).rejects.toThrow(/transport create disabled/);

    const errors = consoleErrors.join('\n');
    expect(errors).toContain('Only CI/CD pipeline may create transports');
  });

  test('allowCreate=true (default) allows create to proceed', async () => {
    mockContext.getTransportSettings.mockReturnValue({ allowCreate: true, allowRelease: true, reason: null });
    mockHttp.post.mockResolvedValue({ success: true, number: 'DEVK900099' });

    await transportCommand.execute(['create', '--description', 'Test'], mockContext);

    expect(mockHttp.post).toHaveBeenCalled();
  });

  test('allowRelease=true (default) allows release to proceed', async () => {
    mockContext.getTransportSettings.mockReturnValue({ allowCreate: true, allowRelease: true, reason: null });
    mockHttp.post.mockResolvedValue({ success: true, number: 'DEVK900001' });

    await transportCommand.execute(['release', '--number', 'DEVK900001'], mockContext);

    expect(mockHttp.post).toHaveBeenCalled();
  });
});
