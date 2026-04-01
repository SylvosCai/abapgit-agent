'use strict';

/**
 * Unit tests for debug command
 */

jest.mock('fs', () => ({
  existsSync: jest.fn(() => false),
  readFileSync: jest.fn(() => '{}'),
  writeFileSync: jest.fn(),
  unlinkSync: jest.fn()
}));

jest.mock('path', () => ({
  join:     jest.fn((...args) => args.join('/')),
  basename: jest.fn((p) => (p || '').split('/').pop())
}));

jest.mock('os', () => ({
  tmpdir: jest.fn(() => '/tmp')
}));

// Mock debug-repl to avoid interactive readline in tests
jest.mock('../../src/utils/debug-repl', () => ({
  startRepl: jest.fn().mockResolvedValue(undefined),
  renderState: jest.fn()
}));

// Mock debug-state
jest.mock('../../src/utils/debug-state', () => ({
  saveActiveSession: jest.fn(),
  loadActiveSession: jest.fn(() => null),
  clearActiveSession: jest.fn(),
  saveBreakpointState: jest.fn(),
  loadBreakpointState: jest.fn(() => []),
  getDaemonSocketPath: jest.fn(() => null)
}));

const debugCommand = require('../../src/commands/debug');
const debugState   = require('../../src/utils/debug-state');

// ─── helpers ─────────────────────────────────────────────────────────────────

function makeAdtHttp({ postResp, getResp, deleteResp } = {}) {
  return jest.fn().mockImplementation(() => ({
    fetchCsrfToken: jest.fn().mockResolvedValue('csrf123'),
    post: jest.fn().mockImplementation(() => {
      const resp = postResp || {
        body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP001" adtcore:uri="/sap/bc/adt/oo/classes/zcl_foo/source/main#start=42" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
        headers: {}, statusCode: 200
      };
      return Promise.resolve(resp);
    }),
    get: jest.fn().mockResolvedValue(getResp || { body: '<feed/>', headers: {}, statusCode: 200 }),
    delete: jest.fn().mockResolvedValue({ body: '', headers: {}, statusCode: 200 })
  }));
}

function makeContext(AdtHttpClass, loadConfigOverride) {
  return {
    loadConfig: loadConfigOverride || jest.fn(() => ({ host: 'test.sap.com', sapport: 443, user: 'U', password: 'P', client: '100' })),
    AdtHttp: AdtHttpClass,
    AdtHttpClass
  };
}

function captureOutput() {
  const lines = [];
  const origLog    = console.log;
  const origError  = console.error;
  const origStderr = process.stderr.write.bind(process.stderr);
  console.log   = (...a) => lines.push(a.join(' '));
  console.error = (...a) => lines.push(a.join(' '));
  process.stderr.write = (s) => { lines.push(String(s)); return true; };
  return {
    restore() {
      console.log = origLog;
      console.error = origError;
      process.stderr.write = origStderr;
    },
    get output() { return lines.join('\n'); }
  };
}

// ─── Module metadata ──────────────────────────────────────────────────────────

describe('Debug Command - module metadata', () => {
  test('has correct name', () => {
    expect(debugCommand.name).toBe('debug');
  });

  test('requiresAbapConfig is true', () => {
    expect(debugCommand.requiresAbapConfig).toBe(true);
  });

  test('requiresVersionCheck is false', () => {
    expect(debugCommand.requiresVersionCheck).toBe(false);
  });
});

// ─── No subcommand ────────────────────────────────────────────────────────────

describe('Debug Command - no subcommand', () => {
  let cap;
  beforeEach(() => { cap = captureOutput(); });
  afterEach(() => { cap.restore(); });

  test('prints usage when no subcommand given', async () => {
    const ctx = makeContext(makeAdtHttp());
    await debugCommand.execute([], ctx);
    expect(cap.output).toMatch(/Usage/i);
    expect(cap.output).toMatch(/set/);
    expect(cap.output).toMatch(/attach/);
  });

  test('prints usage for --help', async () => {
    const ctx = makeContext(makeAdtHttp());
    await debugCommand.execute(['--help'], ctx);
    expect(cap.output).toMatch(/Usage/i);
  });
});

// ─── debug set ────────────────────────────────────────────────────────────────

describe('Debug Command - set', () => {
  let cap;
  beforeEach(() => { cap = captureOutput(); jest.clearAllMocks(); });
  afterEach(() => { cap.restore(); });

  test('sends POST with correct XML and prints breakpoint ID', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP042" adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_class/source/main#start=42" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(),
      delete: jest.fn()
    }));

    await debugCommand.execute(['set', '--object', 'ZCL_MY_CLASS', '--line', '42'], makeContext(AdtHttpClass));

    expect(capturedBody).toContain('zcl_my_class');
    expect(capturedBody).toContain('#start=42');
    expect(cap.output).toMatch(/ZCL_MY_CLASS:42/);
  });

  test('returns JSON with --json flag', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({
        body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP999" adtcore:uri="/sap/bc/adt/programs/programs/ztest#start=10" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
        headers: {}, statusCode: 200
      }),
      get: jest.fn(),
      delete: jest.fn()
    }));

    await debugCommand.execute(['set', '--object', 'ZTEST', '--line', '10', '--json'], makeContext(AdtHttpClass));

    const parsed = JSON.parse(cap.output);
    expect(parsed.id).toBe('BP999');
    expect(parsed.object).toBe('ZTEST');
    expect(parsed.line).toBe(10);
  });

  test('errors when --object is missing', async () => {
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    const ctx = makeContext(makeAdtHttp());
    await expect(debugCommand.execute(['set', '--line', '10'], ctx)).rejects.toThrow('exit');
    expect(cap.output).toMatch(/--object is required/i);
    mockExit.mockRestore();
  });

  test('errors when --line is missing', async () => {
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    const ctx = makeContext(makeAdtHttp());
    await expect(debugCommand.execute(['set', '--object', 'ZCL_FOO'], ctx)).rejects.toThrow('exit');
    expect(cap.output).toMatch(/--line is required/i);
    mockExit.mockRestore();
  });

  test('uses /oo/classes/ URI for ZCL_ objects', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP1" adtcore:uri="/sap/bc/adt/oo/classes/zcl_test/source/main#start=5" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['set', '--object', 'ZCL_TEST', '--line', '5'], makeContext(AdtHttpClass));
    expect(capturedBody).toContain('/sap/bc/adt/oo/classes/zcl_test/source/main');
  });

  test('uses /programs/programs/ URI for non-class objects', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP2" adtcore:uri="/sap/bc/adt/programs/programs/ztest_program#start=5" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['set', '--object', 'ZTEST_PROGRAM', '--line', '5'], makeContext(AdtHttpClass));
    expect(capturedBody).toContain('/sap/bc/adt/programs/programs/ztest_program');
  });

  test('--include testclasses routes to /includes/testclasses URI', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP10" adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_class/includes/testclasses#start=12" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(
      ['set', '--objects', 'ZCL_MY_CLASS:12', '--include', 'testclasses'],
      makeContext(AdtHttpClass)
    );
    expect(capturedBody).toContain('/sap/bc/adt/oo/classes/zcl_my_class/includes/testclasses');
    expect(capturedBody).toContain('#start=12');
    expect(cap.output).toMatch(/ZCL_MY_CLASS:12/);
  });

  test('--include locals_imp routes to /includes/implementations URI', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP11" adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_class/includes/implementations#start=5" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(
      ['set', '--objects', 'ZCL_MY_CLASS:5', '--include', 'locals_imp'],
      makeContext(AdtHttpClass)
    );
    expect(capturedBody).toContain('/sap/bc/adt/oo/classes/zcl_my_class/includes/implementations');
    expect(capturedBody).toContain('#start=5');
    expect(cap.output).toMatch(/ZCL_MY_CLASS:5/);
  });

  test('--include locals_def routes to /includes/definitions URI', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP12" adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_class/includes/definitions#start=3" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(
      ['set', '--objects', 'ZCL_MY_CLASS:3', '--include', 'locals_def'],
      makeContext(AdtHttpClass)
    );
    expect(capturedBody).toContain('/sap/bc/adt/oo/classes/zcl_my_class/includes/definitions');
    expect(capturedBody).toContain('#start=3');
    expect(cap.output).toMatch(/ZCL_MY_CLASS:3/);
  });

  test('rejects unknown --include value', async () => {
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    await expect(
      debugCommand.execute(['set', '--objects', 'ZCL_MY_CLASS:5', '--include', 'implementations'], makeContext(makeAdtHttp()))
    ).rejects.toThrow('exit');
    expect(cap.output).toMatch(/--include must be one of/i);
    mockExit.mockRestore();
  });

  test('--include testclasses returns JSON with correct include info', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({
        body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP20" adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_class/includes/testclasses#start=18" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
        headers: {}, statusCode: 200
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(
      ['set', '--objects', 'ZCL_MY_CLASS:18', '--include', 'testclasses', '--json'],
      makeContext(AdtHttpClass)
    );
    const parsed = JSON.parse(cap.output);
    expect(parsed.id).toBe('BP20');
    expect(parsed.object).toBe('ZCL_MY_CLASS');
    expect(parsed.line).toBe(18);
  });
});

// ─── debug list ───────────────────────────────────────────────────────────────

describe('Debug Command - list', () => {
  let cap;
  beforeEach(() => { cap = captureOutput(); jest.clearAllMocks(); });
  afterEach(() => { cap.restore(); });

  // POST response body that ADT returns when breakpoints are valid
  const bpPostResponse = `<?xml version="1.0"?><dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP001" adtcore:uri="/sap/bc/adt/oo/classes/zcl_foo/source/main#start=42" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>`;

  // Local state to be returned by loadBreakpointState mock
  const localState = [{ id: 'bp-old', object: 'ZCL_FOO', uri: '/sap/bc/adt/oo/classes/zcl_foo/source/main', line: 42 }];

  test('re-POSTs to verify breakpoints and renders table', async () => {
    const debugState = require('../../src/utils/debug-state');
    debugState.loadBreakpointState.mockReturnValue(localState);

    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({ body: bpPostResponse, headers: {}, statusCode: 200 }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['list'], makeContext(AdtHttpClass));
    expect(cap.output).toMatch(/Breakpoints/i);
    expect(cap.output).toMatch(/ZCL_FOO/);
    expect(cap.output).toMatch(/42/);
  });

  test('shows "No breakpoints" when local state is empty', async () => {
    const debugState = require('../../src/utils/debug-state');
    debugState.loadBreakpointState.mockReturnValue([]);

    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      get: jest.fn().mockResolvedValue({ body: '<feed/>', headers: {}, statusCode: 200 }),
      post: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['list'], makeContext(AdtHttpClass));
    expect(cap.output).toMatch(/No breakpoints/i);
  });

  test('warns and removes stale breakpoints rejected by server', async () => {
    const debugState = require('../../src/utils/debug-state');
    debugState.loadBreakpointState.mockReturnValue(localState);

    // Server rejects the breakpoint (invalid position)
    const staleResponse = `<?xml version="1.0"?><dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint errorMessage="Cannot create a breakpoint at this position" adtcore:uri="/sap/bc/adt/oo/classes/zcl_foo/source/main#start=42" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>`;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({ body: staleResponse, headers: {}, statusCode: 200 }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['list'], makeContext(AdtHttpClass));
    expect(cap.output).toMatch(/Warning/i);
    expect(cap.output).toMatch(/no longer valid|not registered|Cannot create/i);
  });

  test('returns JSON with --json flag', async () => {
    const debugState = require('../../src/utils/debug-state');
    debugState.loadBreakpointState.mockReturnValue(localState);

    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({ body: bpPostResponse, headers: {}, statusCode: 200 }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['list', '--json'], makeContext(AdtHttpClass));
    const parsed = JSON.parse(cap.output);
    expect(parsed).toHaveProperty('breakpoints');
    expect(Array.isArray(parsed.breakpoints)).toBe(true);
  });
});

// ─── debug delete ─────────────────────────────────────────────────────────────

describe('Debug Command - delete', () => {
  let cap;
  beforeEach(() => { cap = captureOutput(); jest.clearAllMocks(); });
  afterEach(() => { cap.restore(); });

  test('calls DELETE with correct URL for --id', async () => {
    let deletedUrl;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      delete: jest.fn().mockImplementation((url) => { deletedUrl = url; return Promise.resolve({ body: '', headers: {}, statusCode: 200 }); }),
      get: jest.fn(), post: jest.fn()
    }));

    await debugCommand.execute(['delete', '--id', 'BP001'], makeContext(AdtHttpClass));
    expect(deletedUrl).toContain('BP001');
    expect(cap.output).toMatch(/BP001/);
    expect(cap.output).toMatch(/deleted/i);
  });

  test('calls POST with empty list for --all (synchronize model)', async () => {
    let capturedUrl;
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((url, body) => { capturedUrl = url; capturedBody = body; return Promise.resolve({ body: '', headers: {}, statusCode: 200 }); }),
      get: jest.fn(),
      delete: jest.fn().mockResolvedValue({ body: '', headers: {}, statusCode: 200 })
    }));

    await debugCommand.execute(['delete', '--all'], makeContext(AdtHttpClass));
    expect(capturedUrl).toContain('/sap/bc/adt/debugger/breakpoints');
    expect(cap.output).toMatch(/All breakpoints deleted/i);
  });

  test('returns JSON with --json flag', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({ body: '', headers: {}, statusCode: 200 }),
      delete: jest.fn().mockResolvedValue({ body: '', headers: {}, statusCode: 200 }),
      get: jest.fn()
    }));

    await debugCommand.execute(['delete', '--all', '--json'], makeContext(AdtHttpClass));
    const parsed = JSON.parse(cap.output);
    expect(parsed.deleted).toBe('all');
  });

  test('errors when neither --id nor --all given', async () => {
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    await expect(debugCommand.execute(['delete'], makeContext(makeAdtHttp()))).rejects.toThrow('exit');
    expect(cap.output).toMatch(/--id.*--all/i);
    mockExit.mockRestore();
  });
});

// ─── debug step (AI mode) ─────────────────────────────────────────────────────

describe('Debug Command - step (AI mode)', () => {
  let cap;
  beforeEach(() => {
    cap = captureOutput();
    jest.clearAllMocks();
    // Simulate active session available
    debugState.loadActiveSession.mockReturnValue({ sessionId: 'sess-001', position: {} });
  });
  afterEach(() => { cap.restore(); });

  const stackXml = `<feed>
    <adtdbg:stackFrame>
      <adtdbg:programName>ZCL_MY_CLASS</adtdbg:programName>
      <adtdbg:methodName>EXECUTE</adtdbg:methodName>
      <adtdbg:includeName>ZCL_MY_CLASS===CP</adtdbg:includeName>
      <adtdbg:line>43</adtdbg:line>
      <adtdbg:stackDepth>1</adtdbg:stackDepth>
    </adtdbg:stackFrame>
  </feed>`;

  test('step --type over --json returns JSON position', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({ body: stackXml, headers: {}, statusCode: 200 }),
      get: jest.fn(),
      delete: jest.fn()
    }));

    await debugCommand.execute(['step', '--type', 'over', '--json'], makeContext(AdtHttpClass));

    const parsed = JSON.parse(cap.output);
    expect(parsed).toHaveProperty('position');
    expect(parsed).toHaveProperty('source');
  });

  test('step default type is stepOver', async () => {
    const capturedUrls = [];
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((url) => { capturedUrls.push(url); return Promise.resolve({ body: stackXml, headers: {}, statusCode: 200 }); }),
      get: jest.fn(),
      delete: jest.fn()
    }));

    await debugCommand.execute(['step', '--json'], makeContext(AdtHttpClass));
    // First POST is the step action (?method=stepOver)
    expect(capturedUrls[0]).toContain('stepOver');
  });

  test('step errors with no active session and no --session', async () => {
    debugState.loadActiveSession.mockReturnValue(null);
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    await expect(debugCommand.execute(['step', '--json'], makeContext(makeAdtHttp()))).rejects.toThrow('exit');
    expect(cap.output).toMatch(/No active debug session/i);
    mockExit.mockRestore();
  });

  test('step errors when --session is passed (not valid for step/vars/stack)', async () => {
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    await expect(debugCommand.execute(['step', '--session', 'explicit-sess', '--json'], makeContext(makeAdtHttp()))).rejects.toThrow('exit');
    expect(cap.output).toMatch(/--session is not valid for this command/i);
    mockExit.mockRestore();
  });
});

// ─── debug vars (AI mode) ─────────────────────────────────────────────────────

describe('Debug Command - vars (AI mode)', () => {
  let cap;
  beforeEach(() => {
    cap = captureOutput();
    jest.clearAllMocks();
    debugState.loadActiveSession.mockReturnValue({ sessionId: 'sess-001', position: {} });
  });
  afterEach(() => { cap.restore(); });

  const varsXml = `<feed>
    <adtdbg:variable name="IV_COUNT" type="I" value="5"/>
    <adtdbg:variable name="LV_RESULT" type="STRING" value="init"/>
  </feed>`;

  test('--json returns variable list', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({ body: varsXml, headers: {}, statusCode: 200 }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['vars', '--json'], makeContext(AdtHttpClass));
    const parsed = JSON.parse(cap.output);
    expect(parsed).toHaveProperty('variables');
    expect(Array.isArray(parsed.variables)).toBe(true);
  });

  test('table rendering shows variable names', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({ body: varsXml, headers: {}, statusCode: 200 }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['vars'], makeContext(AdtHttpClass));
    expect(cap.output).toMatch(/Variables/i);
  });
});

// ─── debug stack (AI mode) ────────────────────────────────────────────────────

describe('Debug Command - stack (AI mode)', () => {
  let cap;
  beforeEach(() => {
    cap = captureOutput();
    jest.clearAllMocks();
    debugState.loadActiveSession.mockReturnValue({ sessionId: 'sess-001', position: {} });
  });
  afterEach(() => { cap.restore(); });

  const stackXml = `<feed>
    <adtdbg:stackFrame>
      <adtdbg:programName>ZCL_MY_CLASS</adtdbg:programName>
      <adtdbg:methodName>EXECUTE</adtdbg:methodName>
      <adtdbg:includeName>ZCL_MY_CLASS===CP</adtdbg:includeName>
      <adtdbg:line>43</adtdbg:line>
      <adtdbg:stackDepth>1</adtdbg:stackDepth>
    </adtdbg:stackFrame>
  </feed>`;

  test('--json returns frames array', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({ body: stackXml, headers: {}, statusCode: 200 }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['stack', '--json'], makeContext(AdtHttpClass));
    const parsed = JSON.parse(cap.output);
    expect(parsed).toHaveProperty('frames');
    expect(Array.isArray(parsed.frames)).toBe(true);
  });

  test('table rendering shows Call Stack header', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockResolvedValue({ body: stackXml, headers: {}, statusCode: 200 }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['stack'], makeContext(AdtHttpClass));
    expect(cap.output).toMatch(/Call Stack/i);
  });
});

// ─── debug terminate ──────────────────────────────────────────────────────────

describe('Debug Command - terminate', () => {
  let cap;
  beforeEach(() => {
    cap = captureOutput();
    jest.clearAllMocks();
    debugState.loadActiveSession.mockReturnValue({ sessionId: 'sess-001', position: {} });
  });
  afterEach(() => { cap.restore(); });

  test('calls DELETE and clears active session', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      delete: jest.fn().mockResolvedValue({ body: '', headers: {}, statusCode: 200 }),
      get: jest.fn(),
      post: jest.fn().mockResolvedValue({ body: '', headers: {}, statusCode: 200 })
    }));

    await debugCommand.execute(['terminate'], makeContext(AdtHttpClass));
    expect(debugState.clearActiveSession).toHaveBeenCalled();
    expect(cap.output).toMatch(/terminated/i);
  });

  test('--json returns { terminated: true }', async () => {
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      delete: jest.fn().mockResolvedValue({ body: '', headers: {}, statusCode: 200 }),
      get: jest.fn(),
      post: jest.fn().mockResolvedValue({ body: '', headers: {}, statusCode: 200 })
    }));

    await debugCommand.execute(['terminate', '--json'], makeContext(AdtHttpClass));
    const parsed = JSON.parse(cap.output);
    expect(parsed.terminated).toBe(true);
  });
});

// ─── Unknown subcommand ───────────────────────────────────────────────────────

describe('Debug Command - unknown subcommand', () => {
  let cap;
  beforeEach(() => { cap = captureOutput(); jest.clearAllMocks(); });
  afterEach(() => { cap.restore(); });

  test('shows error and usage for unknown subcommand', async () => {
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    await expect(debugCommand.execute(['bogus'], makeContext(makeAdtHttp()))).rejects.toThrow('exit');
    expect(cap.output).toMatch(/Unknown debug subcommand/i);
    mockExit.mockRestore();
  });
});

// ─── parseVariables (unit tests for the parser) ───────────────────────────────

const { parseVariables, extractFriendlyName } = require('../../src/utils/debug-session');

describe('extractFriendlyName - opaque ID parsing', () => {
  test('extracts field name from object reference child ID', () => {
    const id = '{O:73*\\CLASS=ZCL_ABGAGT_CMD_FACTORY}-MT_COMMAND_MAP\\TYPE=%_T00004S00000130O00';
    expect(extractFriendlyName(id)).toBe('MT_COMMAND_MAP');
  });

  test('extracts [N] from table row IDs', () => {
    const rowId = '{O:73*\\CLASS=ZCL_ABGAGT_CMD_FACTORY}-MT_COMMAND_MAP[1]';
    expect(extractFriendlyName(rowId)).toBe('[1]');
  });

  test('extracts FIELDNAME from structure field within table row ID', () => {
    const fieldId = '{O:73*\\CLASS=ZCL_ABGAGT_CMD_FACTORY}-MT_COMMAND_MAP[1]-COMMANDSTRING';
    expect(extractFriendlyName(fieldId)).toBe('COMMANDSTRING');
  });

  test('extracts FIELDNAME even with \\TYPE= suffix on row field', () => {
    const fieldId = '{O:73*\\CLASS=ZCL_ABGAGT_CMD_FACTORY}-MT_COMMAND_MAP[1]-CLASS_NAME\\TYPE=STRING';
    expect(extractFriendlyName(fieldId)).toBe('CLASS_NAME');
  });

  test('extracts IF_FOO~FIELDNAME for interface-redefined attributes (~ separator)', () => {
    const id1 = '{O:4*\\CLASS=CL_HTTP_REQUEST}-IF_HTTP_ENTITY~FORMFIELD_ENCODING';
    expect(extractFriendlyName(id1)).toBe('IF_HTTP_ENTITY~FORMFIELD_ENCODING');
    const id2 = '{O:4*\\CLASS=CL_HTTP_REQUEST}-IF_HTTP_REQUEST~CO_ENCODING_URL';
    expect(extractFriendlyName(id2)).toBe('IF_HTTP_REQUEST~CO_ENCODING_URL');
    const id3 = '{O:4*\\CLASS=CL_HTTP_REQUEST}-IF_HTTP_RESPONSE~CO_FORMFIELD_ENCODING_ENCODED';
    expect(extractFriendlyName(id3)).toBe('IF_HTTP_RESPONSE~CO_FORMFIELD_ENCODING_ENCODED');
  });

  test('extracts FIELDNAME from VARNAME->FIELDNAME (dataref child)', () => {
    expect(extractFriendlyName('LR_REQUEST->PACKAGE')).toBe('PACKAGE');
    expect(extractFriendlyName('LR_REQUEST->FILES')).toBe('FILES');
    expect(extractFriendlyName('LR_REQUEST->COVERAGE')).toBe('COVERAGE');
  });

  test('extracts [N] from VARNAME->FIELD[N] (table row within dereferenced field)', () => {
    expect(extractFriendlyName('LR_REQUEST->FILES[1]')).toBe('[1]');
    expect(extractFriendlyName('LR_REQUEST->FILES[14]')).toBe('[14]');
    expect(extractFriendlyName('LO_FACTORY->MT_COMMAND_MAP[3]')).toBe('[3]');
  });

  test('returns null for ->* (dereference marker, not a field name)', () => {
    expect(extractFriendlyName('LR_REQUEST->*')).toBeNull();
  });

  test('returns null for already-friendly names', () => {
    expect(extractFriendlyName('IV_COUNT')).toBeNull();
    expect(extractFriendlyName('LT_PARTS')).toBeNull();
  });

  test('returns null for empty or null input', () => {
    expect(extractFriendlyName('')).toBeNull();
    expect(extractFriendlyName(null)).toBeNull();
  });
});

describe('parseVariables - opaque ID friendly-name extraction', () => {
  test('extracts MT_COMMAND_MAP from opaque object-reference child ID', () => {
    const opaqueName = '{O:73*\\CLASS=ZCL_ABGAGT_CMD_FACTORY}-MT_COMMAND_MAP\\TYPE=%_T00004S00000130';
    const xml =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>${opaqueName}</ID>` +
      `<NAME>${opaqueName}</NAME>` +
      `<DECLARED_TYPE_NAME>SOME_TABLE_TYPE</DECLARED_TYPE_NAME>` +
      `<VALUE></VALUE><META_TYPE>table</META_TYPE><TABLE_LINES>14</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;
    const vars = parseVariables(xml);
    expect(vars).toHaveLength(1);
    expect(vars[0].name).toBe('MT_COMMAND_MAP');
    expect(vars[0].id).toContain('{O:73');  // opaque ID preserved
    expect(vars[0].metaType).toBe('table');
    expect(vars[0].tableLines).toBe(14);
  });

  test('extracts [1] row name from table row opaque ID', () => {
    const rowId = '{O:73*\\CLASS=ZCL_ABGAGT_CMD_FACTORY}-MT_COMMAND_MAP[1]';
    const xml =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>${rowId}</ID><NAME>${rowId}</NAME>` +
      `<DECLARED_TYPE_NAME>Type TY_COMMAND_MAP in ZCL_ABGAGT_CMD_FACTORY</DECLARED_TYPE_NAME>` +
      `<VALUE>Structure: deep</VALUE><META_TYPE>structure</META_TYPE><TABLE_LINES>0</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;
    const vars = parseVariables(xml);
    expect(vars[0].name).toBe('[1]');
    // "Type X in Y" → "X"
    expect(vars[0].type).toBe('TY_COMMAND_MAP');
  });

  test('extracts COMMANDSTRING from structure field within table row', () => {
    const fieldId = '{O:73*\\CLASS=ZCL_ABGAGT_CMD_FACTORY}-MT_COMMAND_MAP[1]-COMMANDSTRING';
    const xml =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>${fieldId}</ID><NAME>${fieldId}</NAME>` +
      `<DECLARED_TYPE_NAME>STRING</DECLARED_TYPE_NAME>` +
      `<VALUE>PULL</VALUE><META_TYPE>simple</META_TYPE><TABLE_LINES>0</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;
    const vars = parseVariables(xml);
    expect(vars[0].name).toBe('COMMANDSTRING');
    expect(vars[0].value).toBe('PULL');
    expect(vars[0].type).toBe('STRING');
  });

  test('cleans \\TYPE= prefix — strips generated IDs, keeps named types', () => {
    // Generated ID (starts with %): strip to empty
    const opaqueGenerated = '{O:73*\\CLASS=ZCL_FOO}-MY_FIELD\\TYPE=%_T0001';
    const xmlGen =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>${opaqueGenerated}</ID><NAME>${opaqueGenerated}</NAME>` +
      `<DECLARED_TYPE_NAME>\\TYPE=%_T00004S00000130</DECLARED_TYPE_NAME>` +
      `<VALUE></VALUE><META_TYPE>table</META_TYPE><TABLE_LINES>5</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;
    const varsGen = parseVariables(xmlGen);
    expect(varsGen[0].name).toBe('MY_FIELD');
    expect(varsGen[0].type).toBe('');  // % generated ID stripped to empty

    // Named type: keep it
    const xmlNamed =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>LR_REQUEST</ID><NAME>LR_REQUEST</NAME>` +
      `<DECLARED_TYPE_NAME>\\TYPE=TY_UNIT_PARAMS</DECLARED_TYPE_NAME>` +
      `<VALUE>assigned</VALUE><META_TYPE>dataref</META_TYPE><TABLE_LINES>0</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;
    const varsNamed = parseVariables(xmlNamed);
    expect(varsNamed[0].name).toBe('LR_REQUEST');
    expect(varsNamed[0].type).toBe('TY_UNIT_PARAMS');  // named type preserved
  });
});


describe('parseVariables - HTML entity decoding', () => {
  test('decodes &lt; and &gt; in variable names (field symbols)', () => {
    const xml =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>&lt;LS_REQUEST&gt;</ID>` +
      `<NAME>&lt;LS_REQUEST&gt;</NAME>` +
      `<DECLARED_TYPE_NAME>TY_UNIT_PARAMS</DECLARED_TYPE_NAME>` +
      `<VALUE>assigned</VALUE>` +
      `<META_TYPE>structure</META_TYPE>` +
      `<TABLE_LINES>0</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;
    const vars = parseVariables(xml);
    expect(vars).toHaveLength(1);
    expect(vars[0].name).toBe('<LS_REQUEST>');
    expect(vars[0].type).toBe('TY_UNIT_PARAMS');
  });

  test('decodes &amp; in values', () => {
    const xml =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>LV_STR</ID>` +
      `<NAME>LV_STR</NAME>` +
      `<DECLARED_TYPE_NAME>STRING</DECLARED_TYPE_NAME>` +
      `<VALUE>a &amp; b</VALUE>` +
      `<META_TYPE>simple</META_TYPE>` +
      `<TABLE_LINES>0</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;
    const vars = parseVariables(xml);
    expect(vars[0].value).toBe('a & b');
  });

  test('passes through strings with no entities unchanged', () => {
    const xml =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>IV_COUNT</ID>` +
      `<NAME>IV_COUNT</NAME>` +
      `<DECLARED_TYPE_NAME>I</DECLARED_TYPE_NAME>` +
      `<VALUE>5</VALUE>` +
      `<META_TYPE>simple</META_TYPE>` +
      `<TABLE_LINES>0</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;
    const vars = parseVariables(xml);
    expect(vars[0].name).toBe('IV_COUNT');
    expect(vars[0].value).toBe('5');
  });
});

// ─── debug vars --expand with dataref ────────────────────────────────────────

describe('Debug Command - vars --expand with dataref variable', () => {
  let cap;
  beforeEach(() => {
    cap = captureOutput();
    jest.clearAllMocks();
    debugState.loadActiveSession.mockReturnValue({ sessionId: 'sess-001', position: {} });
  });
  afterEach(() => { cap.restore(); });

  test('--expand LR_REQUEST dereferences dataref and shows structure fields', async () => {
    // First POST (getChildVariables) → hierarchy listing LR_REQUEST
    // Second POST (getVariables for leaf IDs) → LR_REQUEST with meta=dataref
    // Third POST (getVariables for deref ID) → the pointed-to structure (meta=structure)
    // Fourth POST (getChildVariables for structure) → child IDs
    // Fifth POST (getVariables for child IDs) → the actual fields
    const allVarsXml =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>LR_REQUEST</ID><NAME>LR_REQUEST</NAME>` +
      `<DECLARED_TYPE_NAME>REF TO TY_UNIT_PARAMS</DECLARED_TYPE_NAME>` +
      `<VALUE>initial</VALUE><META_TYPE>dataref</META_TYPE><TABLE_LINES>0</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;

    const derefVarsXml =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>LR_REQUEST->*</ID><NAME>LR_REQUEST->*</NAME>` +
      `<DECLARED_TYPE_NAME>TY_UNIT_PARAMS</DECLARED_TYPE_NAME>` +
      `<VALUE></VALUE><META_TYPE>structure</META_TYPE><TABLE_LINES>0</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;

    const childHierXml =
      `<STPDA_ADT_VARIABLE_HIERARCHY>` +
      `<PARENT_ID>LR_REQUEST->*</PARENT_ID><CHILD_ID>LR_REQUEST->*-OBJECT</CHILD_ID>` +
      `</STPDA_ADT_VARIABLE_HIERARCHY>`;

    const childVarsXml =
      `<STPDA_ADT_VARIABLE>` +
      `<ID>LR_REQUEST->*-OBJECT</ID><NAME>OBJECT</NAME>` +
      `<DECLARED_TYPE_NAME>STRING</DECLARED_TYPE_NAME>` +
      `<VALUE>ZCL_FOO</VALUE><META_TYPE>simple</META_TYPE><TABLE_LINES>0</TABLE_LINES>` +
      `</STPDA_ADT_VARIABLE>`;

    let callCount = 0;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      get: jest.fn(),
      delete: jest.fn(),
      post: jest.fn().mockImplementation(() => {
        callCount++;
        // 1: getChildVariables (hierarchy for @LOCALS etc)
        // 2: getVariables (leaf IDs → returns LR_REQUEST with dataref)
        if (callCount <= 2) return Promise.resolve({ body: callCount === 1 ? childHierXml + `<STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>@LOCALS</PARENT_ID><CHILD_ID>LR_REQUEST</CHILD_ID></STPDA_ADT_VARIABLE_HIERARCHY>` : allVarsXml, headers: {}, statusCode: 200 });
        // 3: getVariables(LR_REQUEST->*) → the structure
        if (callCount === 3) return Promise.resolve({ body: derefVarsXml, headers: {}, statusCode: 200 });
        // 4: getChildVariables(LR_REQUEST->*) → child hierarchy
        if (callCount === 4) return Promise.resolve({ body: childHierXml, headers: {}, statusCode: 200 });
        // 5: getVariables(OBJECT) → the field values
        return Promise.resolve({ body: childVarsXml, headers: {}, statusCode: 200 });
      })
    }));

    await debugCommand.execute(['vars', '--expand', 'LR_REQUEST', '--json'], makeContext(AdtHttpClass));
    const parsed = JSON.parse(cap.output);
    expect(parsed).toHaveProperty('children');
    expect(parsed.children.length).toBeGreaterThan(0);
    expect(parsed.children[0].name).toBe('OBJECT');
  });
});

// ─── debug set — --files and --objects ───────────────────────────────────────

describe('Debug Command - set --files', () => {
  let cap;
  beforeEach(() => { cap = captureOutput(); jest.clearAllMocks(); });
  afterEach(() => { cap.restore(); });

  function makeBpResponse(objectPath, line) {
    const lower = objectPath.toLowerCase();
    return {
      body: `<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP001" adtcore:uri="${lower}#start=${line}" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>`,
      headers: {}, statusCode: 200
    };
  }

  test('derives object name from .clas.abap filename', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve(makeBpResponse('/sap/bc/adt/oo/classes/zcl_my_class/source/main', 42));
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['set', '--files', 'abap/zcl_my_class.clas.abap:42'], makeContext(AdtHttpClass));

    expect(capturedBody).toContain('zcl_my_class');
    expect(capturedBody).toContain('#start=42');
    expect(cap.output).toMatch(/ZCL_MY_CLASS:42/);
  });

  test('handles src/ prefix in path', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve(makeBpResponse('/sap/bc/adt/oo/classes/zcl_util/source/main', 10));
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['set', '--files', 'src/zcl_util.clas.abap:10'], makeContext(AdtHttpClass));
    expect(capturedBody).toContain('zcl_util');
    expect(capturedBody).toContain('#start=10');
  });

  test('accepts comma-separated list and adds multiple breakpoints in one POST', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: `<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger">` +
                `<breakpoint id="BP001" adtcore:uri="/sap/bc/adt/oo/classes/zcl_foo/source/main#start=5" xmlns:adtcore="http://www.sap.com/adt/core"/>` +
                `<breakpoint id="BP002" adtcore:uri="/sap/bc/adt/oo/classes/zcl_bar/source/main#start=20" xmlns:adtcore="http://www.sap.com/adt/core"/>` +
                `</dbg:breakpoints>`,
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(
      ['set', '--files', 'src/zcl_foo.clas.abap:5,src/zcl_bar.clas.abap:20'],
      makeContext(AdtHttpClass)
    );
    expect(capturedBody).toContain('zcl_foo');
    expect(capturedBody).toContain('#start=5');
    expect(capturedBody).toContain('zcl_bar');
    expect(capturedBody).toContain('#start=20');
    expect(cap.output).toMatch(/ZCL_FOO:5/);
    expect(cap.output).toMatch(/ZCL_BAR:20/);
  });

  test('errors when file token has no line number', async () => {
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    await expect(
      debugCommand.execute(['set', '--files', 'src/zcl_foo.clas.abap'], makeContext(makeAdtHttp()))
    ).rejects.toThrow('exit');
    expect(cap.output).toMatch(/must include a line number/i);
    mockExit.mockRestore();
  });
});

describe('Debug Command - set --objects', () => {
  let cap;
  beforeEach(() => { cap = captureOutput(); jest.clearAllMocks(); });
  afterEach(() => { cap.restore(); });

  test('sets breakpoint by object name:line', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: `<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP001" adtcore:uri="/sap/bc/adt/oo/classes/zcl_util/source/main#start=99" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>`,
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['set', '--objects', 'ZCL_UTIL:99'], makeContext(AdtHttpClass));
    expect(capturedBody).toContain('zcl_util');
    expect(capturedBody).toContain('#start=99');
    expect(cap.output).toMatch(/ZCL_UTIL:99/);
  });

  test('accepts comma-separated list', async () => {
    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: `<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger">` +
                `<breakpoint id="BP001" adtcore:uri="/sap/bc/adt/oo/classes/zcl_a/source/main#start=1" xmlns:adtcore="http://www.sap.com/adt/core"/>` +
                `<breakpoint id="BP002" adtcore:uri="/sap/bc/adt/oo/classes/zcl_b/source/main#start=2" xmlns:adtcore="http://www.sap.com/adt/core"/>` +
                `</dbg:breakpoints>`,
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['set', '--objects', 'ZCL_A:1,ZCL_B:2'], makeContext(AdtHttpClass));
    expect(capturedBody).toContain('zcl_a');
    expect(capturedBody).toContain('#start=1');
    expect(capturedBody).toContain('zcl_b');
    expect(capturedBody).toContain('#start=2');
  });

  test('errors when object token has no line number', async () => {
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    await expect(
      debugCommand.execute(['set', '--objects', 'ZCL_FOO'], makeContext(makeAdtHttp()))
    ).rejects.toThrow('exit');
    expect(cap.output).toMatch(/must include a line number/i);
    mockExit.mockRestore();
  });

  test('errors when no input form is given', async () => {
    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit'); });
    await expect(
      debugCommand.execute(['set'], makeContext(makeAdtHttp()))
    ).rejects.toThrow('exit');
    expect(cap.output).toMatch(/--files.*--objects.*--object/i);
    mockExit.mockRestore();
  });
});

// ─── debug set — deduplication ────────────────────────────────────────────────

describe('Debug Command - set deduplication', () => {
  let cap;
  beforeEach(() => { cap = captureOutput(); jest.clearAllMocks(); });
  afterEach(() => { cap.restore(); });

  test('skips POST when breakpoint already exists at same object:line', async () => {
    const ds = require('../../src/utils/debug-state');
    ds.loadBreakpointState.mockReturnValue([
      { id: 'bp-existing', object: 'ZCL_MY_CLASS', uri: '/sap/bc/adt/oo/classes/zcl_my_class/source/main', line: 42 }
    ]);

    const postFn = jest.fn();
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: postFn,
      get: jest.fn(), delete: jest.fn()
    }));

    await debugCommand.execute(['set', '--object', 'ZCL_MY_CLASS', '--line', '42'], makeContext(AdtHttpClass));

    expect(postFn).not.toHaveBeenCalled();
    expect(cap.output).toMatch(/Already set/i);
  });

  test('only POSTs new breakpoints when one of several already exists', async () => {
    const ds = require('../../src/utils/debug-state');
    ds.loadBreakpointState.mockReturnValue([
      { id: 'bp-existing', object: 'ZCL_FOO', uri: '/sap/bc/adt/oo/classes/zcl_foo/source/main', line: 5 }
    ]);

    let capturedBody;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((_url, body) => {
        capturedBody = body;
        return Promise.resolve({
          body: `<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger">` +
                `<breakpoint id="BP001" adtcore:uri="/sap/bc/adt/oo/classes/zcl_foo/source/main#start=5" xmlns:adtcore="http://www.sap.com/adt/core"/>` +
                `<breakpoint id="BP002" adtcore:uri="/sap/bc/adt/oo/classes/zcl_bar/source/main#start=20" xmlns:adtcore="http://www.sap.com/adt/core"/>` +
                `</dbg:breakpoints>`,
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    // ZCL_FOO:5 already exists, ZCL_BAR:20 is new
    await debugCommand.execute(
      ['set', '--objects', 'ZCL_FOO:5,ZCL_BAR:20'],
      makeContext(AdtHttpClass)
    );

    // POST should include the existing ZCL_FOO:5 (synchronize model keeps the full list)
    // but the new ZCL_BAR:20 must be in the body too
    expect(capturedBody).toContain('zcl_bar');
    expect(capturedBody).toContain('#start=20');
    expect(cap.output).toMatch(/Already set.*ZCL_FOO/i);
    expect(cap.output).toMatch(/ZCL_BAR:20/);
  });
});

// ─── debug attach — takeover detection ───────────────────────────────────────

describe('Debug Command - attach takeover detection', () => {
  let cap;
  beforeEach(() => { cap = captureOutput(); jest.clearAllMocks(); });
  afterEach(() => { cap.restore(); });

  function makeAttachAdtHttp({ listenerBody = '' } = {}) {
    // breakpoints sync POST response
    const bpResp = {
      body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP1" adtcore:uri="/sap/bc/adt/oo/classes/zcl_util/source/main#start=25" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
      headers: {}, statusCode: 200
    };
    return jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((url) => {
        if (url.includes('/debugger/listeners')) {
          return Promise.resolve({ body: listenerBody, headers: {}, statusCode: 200 });
        }
        return Promise.resolve(bpResp);
      }),
      get: jest.fn(), delete: jest.fn()
    }));
  }

  test('detects saved session from another attach and exits cleanly', async () => {
    const ds = require('../../src/utils/debug-state');
    ds.loadBreakpointState.mockReturnValue([
      { id: 'bp1', object: 'ZCL_UTIL', uri: '/sap/bc/adt/oo/classes/zcl_util/source/main', line: 25 }
    ]);

    // First loadActiveSession call (takeover check): another session is active
    // Second call (session-ended check): session is gone → triggers exit
    ds.loadActiveSession
      .mockReturnValueOnce({ sessionId: 'other-sess', savedAt: Date.now() + 60000, position: { class: 'ZCL_UTIL', line: 25 } })
      .mockReturnValueOnce(null);

    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit:0'); });

    await expect(
      debugCommand.execute(['attach'], makeContext(makeAttachAdtHttp()))
    ).rejects.toThrow('exit:0');

    expect(cap.output).toMatch(/Another session attached/i);
    expect(cap.output).toMatch(/ZCL_UTIL:25/);
    expect(cap.output).toMatch(/Session ended/i);
    expect(mockExit).toHaveBeenCalledWith(0);

    mockExit.mockRestore();
  });

  test('prints "Waiting for it to finish" message and keeps polling until session clears', async () => {
    const ds = require('../../src/utils/debug-state');
    ds.loadBreakpointState.mockReturnValue([
      { id: 'bp1', object: 'ZCL_UTIL', uri: '/sap/bc/adt/oo/classes/zcl_util/source/main', line: 25 }
    ]);

    // Active session present on first check, gone on second
    ds.loadActiveSession
      .mockReturnValueOnce({ sessionId: 'sess-1', savedAt: Date.now() + 60000, position: {} })
      .mockReturnValueOnce(null);

    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit:0'); });

    await expect(
      debugCommand.execute(['attach'], makeContext(makeAttachAdtHttp()))
    ).rejects.toThrow('exit:0');

    expect(cap.output).toMatch(/Waiting for it to finish/i);
    mockExit.mockRestore();
  });

  test('does NOT exit immediately when takeover detected — loads session again to check for end', async () => {
    const ds = require('../../src/utils/debug-state');
    ds.loadBreakpointState.mockReturnValue([
      { id: 'bp1', object: 'ZCL_UTIL', uri: '/sap/bc/adt/oo/classes/zcl_util/source/main', line: 25 }
    ]);

    // Both calls return active session → no exit yet; then null on third call
    ds.loadActiveSession
      .mockReturnValueOnce({ sessionId: 'sess-1', savedAt: Date.now() + 60000, position: {} })
      .mockReturnValueOnce({ sessionId: 'sess-1', savedAt: Date.now() + 60000, position: {} })
      .mockReturnValueOnce(null);

    const mockExit = jest.spyOn(process, 'exit').mockImplementation(() => { throw new Error('exit:0'); });

    // Listener POST returns empty on first call, triggers another loop iteration
    let listenerCalls = 0;
    const AdtHttpClass = jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
      post: jest.fn().mockImplementation((url) => {
        if (url.includes('/debugger/listeners')) {
          listenerCalls++;
          return Promise.resolve({ body: '', headers: {}, statusCode: 200 });
        }
        return Promise.resolve({
          body: '<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"><breakpoint id="BP1" adtcore:uri="/sap/bc/adt/oo/classes/zcl_util/source/main#start=25" xmlns:adtcore="http://www.sap.com/adt/core"/></dbg:breakpoints>',
          headers: {}, statusCode: 200
        });
      }),
      get: jest.fn(), delete: jest.fn()
    }));

    await expect(
      debugCommand.execute(['attach'], makeContext(AdtHttpClass))
    ).rejects.toThrow('exit:0');

    // Should have polled the listener at least once while waiting
    expect(listenerCalls).toBeGreaterThanOrEqual(1);
    mockExit.mockRestore();
  });
});
