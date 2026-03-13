'use strict';

/**
 * Unit tests for run command
 */

jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn(() => 'mock content')
}));

jest.mock('path', () => ({
  isAbsolute: jest.fn(() => false),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => '/' + args.join('/')),
  basename: jest.fn((p) => p.split('/').pop())
}));

// Mock AdtHttp so no real network calls are made
const mockRequest = jest.fn();
const mockFetchCsrfToken = jest.fn().mockResolvedValue('tok');
const mockAdtInstance = {
  csrfToken: 'tok',
  fetchCsrfToken: mockFetchCsrfToken,
  request: mockRequest
};

jest.mock('../../src/utils/adt-http', () => ({
  AdtHttp: jest.fn().mockImplementation(() => mockAdtInstance)
}));

const runCommand = require('../../src/commands/run');

// ─── helpers ────────────────────────────────────────────────────────────────

function makeContext() {
  return {
    loadConfig: jest.fn(() => ({ host: 'test.sap.com', sapport: 443, user: 'USR', password: 'PW', client: '100' })),
    getSafeguards: jest.fn(() => ({ disableRun: false, reason: null }))
  };
}

function captureConsole() {
  const lines = [];
  const origLog   = console.log;
  const origError = console.error;
  console.log   = (...a) => lines.push(a.join(' '));
  console.error = (...a) => lines.push(a.join(' '));
  return {
    restore() {
      console.log   = origLog;
      console.error = origError;
    },
    get output() { return lines.join('\n'); }
  };
}

// Intercept process.exit
let exitCode;
beforeAll(() => {
  jest.spyOn(process, 'exit').mockImplementation((code) => {
    exitCode = code;
    throw new Error(`process.exit(${code})`);
  });
});
afterAll(() => {
  process.exit.mockRestore();
});
beforeEach(() => {
  exitCode = undefined;
  mockRequest.mockReset();
  mockFetchCsrfToken.mockClear();
  mockRequest.mockResolvedValue({ body: '', statusCode: 200, headers: {} });
});

// ─── Module metadata ─────────────────────────────────────────────────────────

describe('run command - module metadata', () => {
  test('has correct name', () => {
    expect(runCommand.name).toBe('run');
  });

  test('requiresAbapConfig is true', () => {
    expect(runCommand.requiresAbapConfig).toBe(true);
  });

  test('requiresVersionCheck is false', () => {
    expect(runCommand.requiresVersionCheck).toBe(false);
  });
});

// ─── Argument validation ─────────────────────────────────────────────────────

describe('run command - argument validation', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  test('exits with error when neither --program nor --class given', async () => {
    await expect(runCommand.execute([], makeContext())).rejects.toThrow('process.exit(1)');
    expect(exitCode).toBe(1);
    expect(cap.output).toMatch(/--program.*--class|--class.*--program/i);
  });

  test('exits with error when both --program and --class given', async () => {
    await expect(runCommand.execute(['--program', 'ZPROG', '--class', 'ZCLS'], makeContext()))
      .rejects.toThrow('process.exit(1)');
    expect(exitCode).toBe(1);
    expect(cap.output).toMatch(/mutually exclusive/i);
  });

});

// ─── Project-level safeguard ─────────────────────────────────────────────────

describe('run command - disableRun safeguard', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  test('exits with error when disableRun is true', async () => {
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      getSafeguards: jest.fn(() => ({ disableRun: true, reason: null }))
    };
    await expect(runCommand.execute(['--program', 'ZPROG'], ctx))
      .rejects.toThrow('process.exit(1)');
    expect(exitCode).toBe(1);
    expect(cap.output).toMatch(/run command is disabled/i);
  });

  test('shows reason when provided', async () => {
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      getSafeguards: jest.fn(() => ({ disableRun: true, reason: 'Side effects in production' }))
    };
    await expect(runCommand.execute(['--class', 'ZCL_FOO'], ctx))
      .rejects.toThrow('process.exit(1)');
    expect(cap.output).toContain('Side effects in production');
  });

  test('proceeds normally when disableRun is false', async () => {
    mockRequest.mockResolvedValue({ body: 'ok', statusCode: 200 });
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      getSafeguards: jest.fn(() => ({ disableRun: false, reason: null }))
    };
    await runCommand.execute(['--program', 'ZPROG'], ctx);
    expect(mockRequest).toHaveBeenCalled();
  });
});

// ─── Program mode (--program) ─────────────────────────────────────────────────

describe('run command - program mode', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  test('calls POST to programrun URL', async () => {
    mockRequest.mockResolvedValue({ body: 'output', statusCode: 200 });
    await runCommand.execute(['--program', 'zprog'], makeContext());
    const [method, url] = mockRequest.mock.calls[0];
    expect(method).toBe('POST');
    expect(url).toBe('/sap/bc/adt/programs/programrun/ZPROG');
  });

  test('program name is uppercased', async () => {
    await runCommand.execute(['--program', 'zmylower'], makeContext());
    const [, url] = mockRequest.mock.calls[0];
    expect(url).toContain('ZMYLOWER');
  });

  test('uses programRun content-type and text/plain accept', async () => {
    await runCommand.execute(['--program', 'ZPROG'], makeContext());
    const [, , , opts] = mockRequest.mock.calls[0];
    expect(opts.contentType).toBe('application/vnd.sap.adt.programs.programRun+xml');
    expect(opts.accept).toBe('text/plain');
  });

  test('sends self-closing programRun XML body', async () => {
    await runCommand.execute(['--program', 'ZPROG'], makeContext());
    const [, , body] = mockRequest.mock.calls[0];
    expect(body).toMatch(/programRun[^>]*\/>/);
  });

  test('displays program output in human mode', async () => {
    mockRequest.mockResolvedValue({ body: 'Hello World', statusCode: 200 });
    await runCommand.execute(['--program', 'ZPROG'], makeContext());
    expect(cap.output).toContain('Hello World');
    expect(cap.output).toContain('--- Output ---');
    expect(cap.output).toContain('Completed: ZPROG');
  });

  test('--json output has program key', async () => {
    mockRequest.mockResolvedValue({ body: 'Hello', statusCode: 200 });
    await runCommand.execute(['--program', 'ZMY_REPORT', '--json'], makeContext());
    const parsed = JSON.parse(cap.output);
    expect(parsed.success).toBe(true);
    expect(parsed.program).toBe('ZMY_REPORT');
    expect(parsed.output).toBe('Hello');
    expect(parsed.class).toBeUndefined();
  });
});

// ─── Class mode (--class) ─────────────────────────────────────────────────────

describe('run command - class mode', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  test('calls POST to classrun URL', async () => {
    mockRequest.mockResolvedValue({ body: 'class output', statusCode: 200 });
    await runCommand.execute(['--class', 'zcl_my_class'], makeContext());
    const [method, url] = mockRequest.mock.calls[0];
    expect(method).toBe('POST');
    expect(url).toBe('/sap/bc/adt/oo/classrun/ZCL_MY_CLASS');
  });

  test('class name is uppercased', async () => {
    await runCommand.execute(['--class', 'zcl_lower'], makeContext());
    const [, url] = mockRequest.mock.calls[0];
    expect(url).toContain('ZCL_LOWER');
  });

  test('sends empty body', async () => {
    await runCommand.execute(['--class', 'ZCL_FOO'], makeContext());
    const [, , body] = mockRequest.mock.calls[0];
    expect(body).toBe('');
  });

  test('uses text/plain accept', async () => {
    await runCommand.execute(['--class', 'ZCL_FOO'], makeContext());
    const [, , , opts] = mockRequest.mock.calls[0];
    expect(opts.accept).toBe('text/plain');
  });

  test('displays class output in human mode', async () => {
    mockRequest.mockResolvedValue({ body: 'Hello from class', statusCode: 200 });
    await runCommand.execute(['--class', 'ZCL_FOO'], makeContext());
    expect(cap.output).toContain('Hello from class');
    expect(cap.output).toContain('Completed: ZCL_FOO');
  });

  test('--json output has class key not program key', async () => {
    mockRequest.mockResolvedValue({ body: 'out', statusCode: 200 });
    await runCommand.execute(['--class', 'ZCL_DEMO', '--json'], makeContext());
    const parsed = JSON.parse(cap.output);
    expect(parsed.success).toBe(true);
    expect(parsed.class).toBe('ZCL_DEMO');
    expect(parsed.output).toBe('out');
    expect(parsed.program).toBeUndefined();
  });
});

// ─── Shared output behaviour ─────────────────────────────────────────────────

describe('run command - shared output', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  test('shows (no output) when body is empty', async () => {
    mockRequest.mockResolvedValue({ body: '', statusCode: 200 });
    await runCommand.execute(['--program', 'ZPROG'], makeContext());
    expect(cap.output).toContain('(no output)');
  });

  test('prints error and exits 1 on ADT failure (program)', async () => {
    mockRequest.mockRejectedValue({ statusCode: 404, message: 'HTTP 404: not found' });
    await expect(runCommand.execute(['--program', 'ZNOPE'], makeContext()))
      .rejects.toThrow('process.exit(1)');
    expect(exitCode).toBe(1);
    expect(cap.output).toMatch(/Error:/);
  });

  test('prints error and exits 1 on ADT failure (class)', async () => {
    mockRequest.mockRejectedValue({ statusCode: 404, message: 'HTTP 404: not found' });
    await expect(runCommand.execute(['--class', 'ZCL_NOPE'], makeContext()))
      .rejects.toThrow('process.exit(1)');
    expect(exitCode).toBe(1);
    expect(cap.output).toMatch(/Error:/);
  });

  test('--json error response has target name', async () => {
    mockRequest.mockRejectedValue({ statusCode: 404, message: 'HTTP 404: not found' });
    await expect(runCommand.execute(['--class', 'ZCL_NOPE', '--json'], makeContext()))
      .rejects.toThrow('process.exit(1)');
    const parsed = JSON.parse(cap.output);
    expect(parsed.success).toBe(false);
    expect(parsed.target).toBe('ZCL_NOPE');
    expect(parsed.error).toMatch(/404/);
  });
});
