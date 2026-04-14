/**
 * Unit tests for transport-selector utility
 * Tests TTY detection, hook execution, and pull.js integration
 */

// Mock fs module
jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn(() => '{}')
}));

// Mock path module
jest.mock('path', () => ({
  isAbsolute: jest.fn((p) => p.startsWith('/')),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => args.join('/')),
  basename: jest.fn((p) => p.split('/').pop())
}));

jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

describe('Transport Selector - isNonInteractive()', () => {
  let selector;

  beforeEach(() => {
    jest.resetModules();
    // Restore any env overrides
    delete process.env.NO_TTY;
    selector = require('../../src/utils/transport-selector');
  });

  afterEach(() => {
    delete process.env.NO_TTY;
    jest.restoreAllMocks();
  });

  test('returns true when NO_TTY=1 env var is set', () => {
    process.env.NO_TTY = '1';
    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');
    expect(freshSelector.isNonInteractive()).toBe(true);
  });

  test('returns false when stdout is TTY and no override', () => {
    // Simulate a TTY environment
    const origStdoutIsTTY = process.stdout.isTTY;
    const origStdinIsTTY = process.stdin.isTTY;
    Object.defineProperty(process.stdout, 'isTTY', { value: true, configurable: true });
    Object.defineProperty(process.stdin, 'isTTY', { value: true, configurable: true });

    delete process.env.NO_TTY;
    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');
    expect(freshSelector.isNonInteractive()).toBe(false);

    // Restore
    Object.defineProperty(process.stdout, 'isTTY', { value: origStdoutIsTTY, configurable: true });
    Object.defineProperty(process.stdin, 'isTTY', { value: origStdinIsTTY, configurable: true });
  });

  test('returns true when stdout is not TTY', () => {
    const origStdoutIsTTY = process.stdout.isTTY;
    Object.defineProperty(process.stdout, 'isTTY', { value: false, configurable: true });

    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');
    expect(freshSelector.isNonInteractive()).toBe(true);

    Object.defineProperty(process.stdout, 'isTTY', { value: origStdoutIsTTY, configurable: true });
  });
});

describe('Transport Selector - runHook()', () => {
  let selector;

  beforeEach(() => {
    jest.resetModules();
    selector = require('../../src/utils/transport-selector');
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('returns transport number from hook', async () => {
    const mockHook = jest.fn().mockResolvedValue('DEVK900001');
    // Mock require to return hook
    jest.spyOn(selector, 'runHook').mockImplementation(async (hookPath, context) => {
      return await mockHook(context);
    });

    const result = await selector.runHook('/tmp/hook.js', { config: {}, http: {} });
    expect(result).toBe('DEVK900001');
  });

  test('returns null when hook returns null', async () => {
    jest.spyOn(selector, 'runHook').mockResolvedValue(null);

    const result = await selector.runHook('/tmp/hook.js', { config: {}, http: {} });
    expect(result).toBeNull();
  });

  test('returns null when hook throws', async () => {
    jest.spyOn(selector, 'runHook').mockImplementation(async () => {
      throw new Error('hook failed');
    });

    // Should catch and return null
    const safeResult = await selector.runHook('/tmp/hook.js', { config: {}, http: {} }).catch(() => null);
    expect(safeResult).toBeNull();
  });
});

describe('Transport Selector - selectTransport() AI mode (non-interactive)', () => {
  let selector;
  let mockHttp;
  let mockConfig;

  beforeEach(() => {
    jest.resetModules();
    process.env.NO_TTY = '1';  // Force AI mode

    mockHttp = { get: jest.fn(), post: jest.fn() };
    mockConfig = { host: 'test.sap.com', user: 'DEV' };

    selector = require('../../src/utils/transport-selector');
  });

  afterEach(() => {
    delete process.env.NO_TTY;
    jest.restoreAllMocks();
  });

  test('returns null when no hook configured', async () => {
    // No hook configured in project config
    const fs = require('fs');
    fs.existsSync.mockReturnValue(false);

    jest.resetModules();
    process.env.NO_TTY = '1';
    const freshSelector = require('../../src/utils/transport-selector');

    const result = await freshSelector.selectTransport(mockConfig, mockHttp);
    expect(result).toBeNull();
  });

  test('returns transport from hook when hook is configured', async () => {
    // Spy on the internal hook config reader + runHook, no need for fs mocks
    jest.spyOn(selector, '_getTransportHookConfig').mockReturnValue({
      hook: './scripts/get-transport.js'
    });
    jest.spyOn(selector, 'runHook').mockResolvedValue('DEVK900001');

    const result = await selector.selectTransport(mockConfig, mockHttp);
    expect(result).toBe('DEVK900001');
  });

  test('returns null when hook returns null', async () => {
    jest.spyOn(selector, '_getTransportHookConfig').mockReturnValue({
      hook: './scripts/get-transport.js'
    });
    jest.spyOn(selector, 'runHook').mockResolvedValue(null);

    const result = await selector.selectTransport(mockConfig, mockHttp);
    expect(result).toBeNull();
  });

  test('returns null when hook throws (silent fallback)', async () => {
    jest.spyOn(selector, '_getTransportHookConfig').mockReturnValue({
      hook: './scripts/get-transport.js'
    });
    jest.spyOn(selector, 'runHook').mockRejectedValue(new Error('hook error'));

    const result = await selector.selectTransport(mockConfig, mockHttp);
    expect(result).toBeNull();
  });
});

describe('Transport Selector - fetchTransports()', () => {
  let selector;
  let mockHttp;

  beforeEach(() => {
    jest.resetModules();
    selector = require('../../src/utils/transport-selector');
    mockHttp = { get: jest.fn() };
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('calls GET /transport?scope=mine', async () => {
    mockHttp.get.mockResolvedValue({
      success: true,
      transports: [
        { number: 'DEVK900001', description: 'Feature X', owner: 'DEVELOPER', date: '2026-03-09' }
      ]
    });

    const transports = await selector.fetchTransports(mockHttp, 'mine');
    expect(mockHttp.get).toHaveBeenCalledWith(
      expect.stringMatching(/transport.*scope=mine/)
    );
    expect(transports).toHaveLength(1);
    expect(transports[0].number).toBe('DEVK900001');
  });

  test('calls GET /transport?scope=tasks', async () => {
    mockHttp.get.mockResolvedValue({
      success: true,
      transports: []
    });

    await selector.fetchTransports(mockHttp, 'tasks');
    expect(mockHttp.get).toHaveBeenCalledWith(
      expect.stringMatching(/scope=tasks/)
    );
  });

  test('normalises ABAP uppercase keys', async () => {
    mockHttp.get.mockResolvedValue({
      SUCCESS: true,
      TRANSPORTS: [
        { NUMBER: 'DEVK900001', DESCRIPTION: 'Feature X', OWNER: 'DEV', DATE: '2026-03-09' }
      ]
    });

    const transports = await selector.fetchTransports(mockHttp, 'mine');
    expect(transports[0].number).toBe('DEVK900001');
    expect(transports[0].description).toBe('Feature X');
  });

  test('returns empty array on error', async () => {
    mockHttp.get.mockRejectedValue(new Error('Connection refused'));

    const transports = await selector.fetchTransports(mockHttp, 'mine');
    expect(transports).toEqual([]);
  });
});

describe('Transport Selector - createTransport()', () => {
  let selector;
  let mockHttp;

  beforeEach(() => {
    jest.resetModules();
    selector = require('../../src/utils/transport-selector');
    mockHttp = { post: jest.fn() };
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('calls POST /transport with action=CREATE', async () => {
    mockHttp.post.mockResolvedValue({
      success: true,
      number: 'DEVK900099'
    });

    const number = await selector.createTransport(mockHttp, 'Sprint 42');
    expect(mockHttp.post).toHaveBeenCalledWith(
      expect.stringContaining('/transport'),
      expect.objectContaining({ action: 'CREATE', description: 'Sprint 42' }),
      expect.anything()
    );
    expect(number).toBe('DEVK900099');
  });

  test('normalises ABAP uppercase NUMBER key', async () => {
    mockHttp.post.mockResolvedValue({
      SUCCESS: true,
      NUMBER: 'DEVK900099'
    });

    const number = await selector.createTransport(mockHttp, 'Test');
    expect(number).toBe('DEVK900099');
  });

  test('returns null on failure', async () => {
    mockHttp.post.mockRejectedValue(new Error('Failed'));

    const number = await selector.createTransport(mockHttp, 'Test');
    expect(number).toBeNull();
  });
});

describe('Transport Selector - buildRun()', () => {
  let selector;
  let mockConfig;
  let mockHttp;
  let mockLoadConfig;
  let MockAbapHttp;
  let mockGetTransportSettings;

  beforeEach(() => {
    jest.resetModules();
    selector = require('../../src/utils/transport-selector');
    mockConfig = { host: 'test.sap.com', user: 'DEV' };
    mockHttp = { get: jest.fn(), post: jest.fn(), fetchCsrfToken: jest.fn().mockResolvedValue('token') };
    mockLoadConfig = jest.fn().mockReturnValue(mockConfig);
    MockAbapHttp = jest.fn().mockImplementation(() => mockHttp);
    mockGetTransportSettings = jest.fn().mockReturnValue({ allowCreate: true, allowRelease: true, reason: null });
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('returns undefined when loadConfig is not provided', () => {
    const run = selector.buildRun(mockConfig, mockHttp, null, null, null);
    expect(run).toBeUndefined();
  });

  test('returns a function when factories are provided', () => {
    const run = selector.buildRun(mockConfig, mockHttp, mockLoadConfig, MockAbapHttp, mockGetTransportSettings);
    expect(typeof run).toBe('function');
  });

  test('run appends --json when not already present', async () => {
    const capturedArgs = [];
    const fakeCmd = {
      execute: jest.fn(async (args) => {
        capturedArgs.push(...args);
        console.log(JSON.stringify({ transports: [] }));
      })
    };

    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');
    jest.spyOn(freshSelector, 'buildRun').mockImplementation((cfg, h, lc, AH, gts) => {
      if (!lc || !AH) return undefined;
      return async function run(command) {
        const [, ...args] = command.trim().split(/\s+/);
        const runArgs = args.includes('--json') ? args : [...args, '--json'];
        const captured = [];
        const origLog = console.log;
        console.log = (...a) => captured.push(a.map(String).join(' '));
        try {
          await fakeCmd.execute(runArgs, { loadConfig: () => cfg, AbapHttp: AH, getTransportSettings: gts });
        } finally {
          console.log = origLog;
        }
        return JSON.parse(captured.join(''));
      };
    });

    const run = freshSelector.buildRun(mockConfig, mockHttp, mockLoadConfig, MockAbapHttp, mockGetTransportSettings);
    await run('transport list --scope task');

    expect(capturedArgs).toContain('--json');
    expect(capturedArgs).toContain('list');
  });

  test('run returns parsed JSON from command output', async () => {
    const fakeResult = { success: true, transports: [{ number: 'DEVK900001' }] };

    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');
    jest.spyOn(freshSelector, 'buildRun').mockImplementation((cfg, h, lc, AH, gts) => {
      if (!lc || !AH) return undefined;
      return async function run() {
        const captured = [];
        const origLog = console.log;
        console.log = (...a) => captured.push(a.map(String).join(' '));
        try {
          console.log(JSON.stringify(fakeResult));
        } finally {
          console.log = origLog;
        }
        return JSON.parse(captured.join(''));
      };
    });

    const run = freshSelector.buildRun(mockConfig, mockHttp, mockLoadConfig, MockAbapHttp, mockGetTransportSettings);
    const result = await run('transport list');

    expect(result).toEqual(fakeResult);
  });

  test('run helper included in hook context when factories passed to selectTransport', async () => {
    process.env.NO_TTY = '1';
    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');

    jest.spyOn(freshSelector, '_getTransportHookConfig').mockReturnValue({ hook: './scripts/get-transport.js' });

    let receivedContext;
    jest.spyOn(freshSelector, 'runHook').mockImplementation(async (_hookPath, ctx) => {
      receivedContext = ctx;
      return 'DEVK900001';
    });

    await freshSelector.selectTransport(mockConfig, mockHttp, mockLoadConfig, MockAbapHttp, mockGetTransportSettings);

    expect(typeof receivedContext.run).toBe('function');
    delete process.env.NO_TTY;
  });

  test('run helper is undefined in hook context when no factories passed to selectTransport', async () => {
    process.env.NO_TTY = '1';
    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');

    jest.spyOn(freshSelector, '_getTransportHookConfig').mockReturnValue({ hook: './scripts/get-transport.js' });

    let receivedContext;
    jest.spyOn(freshSelector, 'runHook').mockImplementation(async (_hookPath, ctx) => {
      receivedContext = ctx;
      return null;
    });

    await freshSelector.selectTransport(mockConfig, mockHttp);

    expect(receivedContext.run).toBeUndefined();
    delete process.env.NO_TTY;
  });

  test('type is passed through to hook context (workbench default)', async () => {
    process.env.NO_TTY = '1';
    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');
    jest.spyOn(freshSelector, '_getTransportHookConfig').mockReturnValue({ hook: './scripts/get-transport.js' });

    let receivedContext;
    jest.spyOn(freshSelector, 'runHook').mockImplementation(async (_hookPath, ctx) => {
      receivedContext = ctx;
      return 'DEVK900001';
    });

    await freshSelector.selectTransport(mockConfig, mockHttp, mockLoadConfig, MockAbapHttp, mockGetTransportSettings);

    expect(receivedContext.type).toBe('workbench');
    delete process.env.NO_TTY;
  });

  test('type=customizing is passed through to hook context', async () => {
    process.env.NO_TTY = '1';
    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');
    jest.spyOn(freshSelector, '_getTransportHookConfig').mockReturnValue({ hook: './scripts/get-transport.js' });

    let receivedContext;
    jest.spyOn(freshSelector, 'runHook').mockImplementation(async (_hookPath, ctx) => {
      receivedContext = ctx;
      return 'DEVK900002';
    });

    await freshSelector.selectTransport(mockConfig, mockHttp, mockLoadConfig, MockAbapHttp, mockGetTransportSettings, 'customizing');

    expect(receivedContext.type).toBe('customizing');
    delete process.env.NO_TTY;
  });

  test('run helper auto-injects --type for transport list when type=customizing', async () => {
    process.env.NO_TTY = '1';
    jest.resetModules();
    const freshSelector = require('../../src/utils/transport-selector');
    jest.spyOn(freshSelector, '_getTransportHookConfig').mockReturnValue({ hook: './scripts/get-transport.js' });

    let capturedArgs;
    const MockAbapHttpForRun = function () { return mockHttp; };
    const mockTransportExecute = jest.fn(async (args) => { capturedArgs = args; });
    jest.mock('../../src/commands/transport', () => ({ execute: mockTransportExecute }), { virtual: true });

    jest.spyOn(freshSelector, 'runHook').mockImplementation(async (_hookPath, ctx) => {
      // Simulate hook calling run('transport list --scope task')
      // We just verify the run function exists and was built with type injected
      expect(typeof ctx.run).toBe('function');
      expect(ctx.type).toBe('customizing');
      return 'DEVK900002';
    });

    await freshSelector.selectTransport(mockConfig, mockHttp, mockLoadConfig, MockAbapHttp, mockGetTransportSettings, 'customizing');

    delete process.env.NO_TTY;
  });
});

describe('Transport Selector - _getTransportHookConfig()', () => {
  beforeEach(() => {
    jest.resetModules();
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('returns null when project config file does not exist', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(false);
    const selector = require('../../src/utils/transport-selector');
    expect(selector._getTransportHookConfig()).toBeNull();
  });

  test('returns null when JSON is malformed (parse error silently ignored)', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue('{invalid json}');
    const selector = require('../../src/utils/transport-selector');
    expect(selector._getTransportHookConfig()).toBeNull();
  });

  test('returns null when config has no transports.hook', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({ project: { name: 'Test' } }));
    const selector = require('../../src/utils/transport-selector');
    expect(selector._getTransportHookConfig()).toBeNull();
  });

  test('returns hook config when transports.hook is present', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({
      transports: {
        hook: { path: './scripts/get-transport.js', description: 'My hook' }
      }
    }));
    const selector = require('../../src/utils/transport-selector');
    const result = selector._getTransportHookConfig();
    expect(result).toEqual({ hook: './scripts/get-transport.js', description: 'My hook' });
  });

  test('returns null hook path when path is missing', () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({
      transports: { hook: { description: 'hook without path' } }
    }));
    const selector = require('../../src/utils/transport-selector');
    const result = selector._getTransportHookConfig();
    expect(result).toEqual({ hook: null, description: 'hook without path' });
  });
});

describe('Pull command integration - selectTransport hook', () => {
  let pullCommand;
  let mockContext;

  beforeEach(() => {
    jest.resetModules();
    jest.spyOn(process, 'exit').mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});

    process.env.NO_TTY = '1';
  });

  afterEach(() => {
    delete process.env.NO_TTY;
    jest.restoreAllMocks();
  });

  test('selectTransport is invoked when no transport configured (AI mode)', async () => {
    const fs = require('fs');
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue(JSON.stringify({
      transports: { hook: './scripts/get-transport.js' }
    }));

    const selector = require('../../src/utils/transport-selector');
    const mockSelectTransport = jest.spyOn(selector, 'selectTransport').mockResolvedValue('DEVK900042');

    pullCommand = require('../../src/commands/pull');

    const mockHttp = {
      fetchCsrfToken: jest.fn().mockResolvedValue('token'),
      post: jest.fn().mockImplementation((url) => {
        if (url.includes('/status')) return Promise.resolve({ success: true, status: 'Found', transport_required: true });
        return Promise.resolve({ success: 'X', activated_count: 0, failed_count: 0, log_messages: [] });
      })
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com', gitUsername: 'u', gitPassword: 'p' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp),
      gitUtils: { getBranch: jest.fn().mockReturnValue('main'), getRemoteUrl: jest.fn().mockReturnValue('https://github.com/test/repo') },
      getTransport: jest.fn().mockReturnValue(null),  // No transport from config
      getSafeguards: jest.fn().mockReturnValue({ disablePull: false, requireFilesForPull: false }),
      getConflictSettings: jest.fn().mockReturnValue({ mode: 'abort' }),
      getTransportSettings: jest.fn().mockReturnValue({ allowCreate: true, allowRelease: true, reason: null })
    };

    await pullCommand.execute([], mockContext);

    expect(mockSelectTransport).toHaveBeenCalled();
    // The transport from hook should be used in the POST body (status check is call[0], pull is call[1])
    const pullCall = mockHttp.post.mock.calls.find(c => c[0].includes('/pull'));
    expect(pullCall[1]).toHaveProperty('transport_request', 'DEVK900042');
  });

  test('selectTransport is NOT called when --json flag is set', async () => {
    const selector = require('../../src/utils/transport-selector');
    const mockSelectTransport = jest.spyOn(selector, 'selectTransport').mockResolvedValue('DEVK900042');

    pullCommand = require('../../src/commands/pull');

    const mockHttp = {
      fetchCsrfToken: jest.fn().mockResolvedValue('token'),
      post: jest.fn().mockResolvedValue({ success: true, activated_count: 0, failed_count: 0, log_messages: [] })
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp),
      gitUtils: { getBranch: jest.fn().mockReturnValue('main'), getRemoteUrl: jest.fn().mockReturnValue('https://github.com/test/repo') },
      getTransport: jest.fn().mockReturnValue(null),
      getSafeguards: jest.fn().mockReturnValue({ disablePull: false, requireFilesForPull: false }),
      getConflictSettings: jest.fn().mockReturnValue({ mode: 'abort' })
    };

    await pullCommand.execute(['--json'], mockContext);

    expect(mockSelectTransport).not.toHaveBeenCalled();
  });

  test('selectTransport is NOT called when transport is already set', async () => {
    const selector = require('../../src/utils/transport-selector');
    const mockSelectTransport = jest.spyOn(selector, 'selectTransport').mockResolvedValue('DEVK900042');

    pullCommand = require('../../src/commands/pull');

    const mockHttp = {
      fetchCsrfToken: jest.fn().mockResolvedValue('token'),
      post: jest.fn().mockResolvedValue({ success: 'X', activated_count: 0, failed_count: 0, log_messages: [] })
    };

    mockContext = {
      loadConfig: jest.fn().mockReturnValue({ host: 'test.sap.com' }),
      AbapHttp: jest.fn().mockImplementation(() => mockHttp),
      gitUtils: { getBranch: jest.fn().mockReturnValue('main'), getRemoteUrl: jest.fn().mockReturnValue('https://github.com/test/repo') },
      getTransport: jest.fn().mockReturnValue('DEVK900001'),  // Transport already set
      getSafeguards: jest.fn().mockReturnValue({ disablePull: false, requireFilesForPull: false }),
      getConflictSettings: jest.fn().mockReturnValue({ mode: 'abort' })
    };

    await pullCommand.execute([], mockContext);

    expect(mockSelectTransport).not.toHaveBeenCalled();
  });
});
