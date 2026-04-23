'use strict';

/**
 * Unit tests for dump command
 * Tests argument parsing, timezone helpers, rendering, and execute flow
 */

// Mock fs/path modules (required by the test harness)
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

// Access internal helpers via the module (they are not exported, so we test
// them indirectly through execute() where needed, and directly via re-export
// in logic tests below using the module's private functions exposed by
// requiring the file and calling execute with controlled mocks).

const dumpCommand = require('../../src/commands/dump');

// ─── helpers ────────────────────────────────────────────────────────────────

function makeContext(postResponse) {
  return {
    loadConfig: jest.fn(() => ({ host: 'test', port: 443 })),
    AbapHttp: jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('token123'),
      post: jest.fn().mockResolvedValue(postResponse)
    }))
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

// ─── Module metadata ─────────────────────────────────────────────────────────

describe('Dump Command - module metadata', () => {
  test('has correct name', () => {
    expect(dumpCommand.name).toBe('dump');
  });

  test('requiresAbapConfig is true', () => {
    expect(dumpCommand.requiresAbapConfig).toBe(true);
  });

  test('requiresVersionCheck is false', () => {
    expect(dumpCommand.requiresVersionCheck).toBe(false);
  });
});

// ─── Argument parsing ────────────────────────────────────────────────────────

describe('Dump Command - argument parsing', () => {
  beforeEach(() => {
    jest.spyOn(console, 'log').mockImplementation(() => {});
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('default limit is 20', async () => {
    let capturedData;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation((_url, data) => {
          capturedData = data;
          return Promise.resolve({ SUCCESS: true, DUMPS: [], TOTAL: 0 });
        })
      }))
    };

    await dumpCommand.execute([], ctx);
    expect(capturedData.limit).toBe(20);
  });

  test('--limit overrides default', async () => {
    let capturedData;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation((_url, data) => {
          capturedData = data;
          return Promise.resolve({ SUCCESS: true, DUMPS: [], TOTAL: 0 });
        })
      }))
    };

    await dumpCommand.execute(['--limit', '5'], ctx);
    expect(capturedData.limit).toBe(5);
  });

  test('--limit is capped at 100', async () => {
    let capturedData;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation((_url, data) => {
          capturedData = data;
          return Promise.resolve({ SUCCESS: true, DUMPS: [], TOTAL: 0 });
        })
      }))
    };

    await dumpCommand.execute(['--limit', '999'], ctx);
    expect(capturedData.limit).toBe(100);
  });

  test('--user is uppercased and sent', async () => {
    let capturedData;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation((_url, data) => {
          capturedData = data;
          return Promise.resolve({ SUCCESS: true, DUMPS: [], TOTAL: 0 });
        })
      }))
    };

    await dumpCommand.execute(['--user', 'developer'], ctx);
    expect(capturedData.user).toBe('DEVELOPER');
  });

  test('--program is uppercased and sent', async () => {
    let capturedData;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation((_url, data) => {
          capturedData = data;
          return Promise.resolve({ SUCCESS: true, DUMPS: [], TOTAL: 0 });
        })
      }))
    };

    await dumpCommand.execute(['--program', 'zmyreport'], ctx);
    expect(capturedData.program).toBe('ZMYREPORT');
  });

  test('--error is uppercased and sent', async () => {
    let capturedData;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation((_url, data) => {
          capturedData = data;
          return Promise.resolve({ SUCCESS: true, DUMPS: [], TOTAL: 0 });
        })
      }))
    };

    await dumpCommand.execute(['--error', 'cx_sy_ref_is_initial'], ctx);
    expect(capturedData.error).toBe('CX_SY_REF_IS_INITIAL');
  });

  test('no --date means no ts_from/ts_to in payload', async () => {
    let capturedData;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation((_url, data) => {
          capturedData = data;
          return Promise.resolve({ SUCCESS: true, DUMPS: [], TOTAL: 0 });
        })
      }))
    };

    await dumpCommand.execute([], ctx);
    expect(capturedData.ts_from).toBeUndefined();
    expect(capturedData.ts_to).toBeUndefined();
  });

  test('--date with specific date sends ts_from and ts_to', async () => {
    let capturedData;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation((_url, data) => {
          capturedData = data;
          return Promise.resolve({ SUCCESS: true, DUMPS: [], TOTAL: 0 });
        })
      }))
    };

    await dumpCommand.execute(['--date', '20240115', '--timezone', 'UTC'], ctx);
    expect(capturedData.ts_from).toBeDefined();
    expect(capturedData.ts_to).toBeDefined();
    // For UTC, date 20240115 00:00:00 → ts_from starts with 20240115
    expect(capturedData.ts_from).toMatch(/^20240115/);
  });
});

// ─── List mode rendering ─────────────────────────────────────────────────────

describe('Dump Command - list mode rendering', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  test('shows count header', async () => {
    const ctx = makeContext({
      SUCCESS: true,
      DUMPS: [
        { ID: '1', DATE: '2024-01-15', TIME: '10:00:00', USER: 'DEV', PROGRAM: 'ZTEST', ERROR: 'CX_SY_REF_IS_INITIAL' }
      ],
      TOTAL: 1
    });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/Short Dumps/i);
    expect(cap.output).toMatch(/1 found/);
  });

  test('shows dump fields in table row', async () => {
    const ctx = makeContext({
      SUCCESS: true,
      DUMPS: [
        { ID: '1', DATE: '2024-01-15', TIME: '10:00:00', USER: 'DEVUSER', PROGRAM: 'ZMYREPORT', ERROR: 'CX_ERROR' }
      ],
      TOTAL: 1
    });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/DEVUSER/);
    expect(cap.output).toMatch(/ZMYREPORT/);
    expect(cap.output).toMatch(/CX_ERROR/);
  });

  test('shows "no dumps" message when list is empty', async () => {
    const ctx = makeContext({ SUCCESS: true, DUMPS: [], TOTAL: 0 });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/No short dumps found/i);
  });

  test('shows truncation hint when total > limit', async () => {
    const dumps = Array.from({ length: 20 }, (_, i) => ({
      ID: String(i + 1), DATE: '2024-01-15', TIME: '10:00:00',
      USER: 'U', PROGRAM: 'P', ERROR: 'E'
    }));
    const ctx = makeContext({ SUCCESS: true, DUMPS: dumps, TOTAL: 50 });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/20.*50|Showing.*of/i);
  });

  test('shows --detail hint', async () => {
    const ctx = makeContext({
      SUCCESS: true,
      DUMPS: [{ ID: '1', DATE: '2024-01-15', TIME: '10:00:00', USER: 'U', PROGRAM: 'P', ERROR: 'E' }],
      TOTAL: 1
    });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/--detail/);
  });

  test('uses lowercase response fields (ABAP backend may return either case)', async () => {
    const ctx = makeContext({
      success: true,
      dumps: [
        { id: '1', date: '2024-01-15', time: '10:00:00', user: 'LOWUSER', program: 'LOWPROG', error: 'LOWERR' }
      ],
      total: 1
    });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/LOWUSER/);
    expect(cap.output).toMatch(/LOWPROG/);
  });

  test('uses UTC_TIMESTAMP for display when available', async () => {
    // UTC timestamp 20240115100000 → in UTC should show 2024-01-15 / 10:00:00
    const ctx = makeContext({
      SUCCESS: true,
      DUMPS: [{ ID: '1', UTC_TIMESTAMP: '20240115100000', USER: 'U', PROGRAM: 'P', ERROR: 'E' }],
      TOTAL: 1
    });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/2024-01-15/);
    expect(cap.output).toMatch(/10:00:00/);
  });
});

// ─── Error handling ──────────────────────────────────────────────────────────

describe('Dump Command - error handling', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  test('shows error when SUCCESS is false', async () => {
    const ctx = makeContext({ SUCCESS: false, ERROR: 'Something went wrong' });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/Something went wrong/);
  });

  test('shows error when ERROR field is set even if SUCCESS is true', async () => {
    const ctx = makeContext({ SUCCESS: true, ERROR: 'Partial failure', DUMPS: [] });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/Partial failure/);
  });

  test('shows fallback error message when ERROR field is absent', async () => {
    const ctx = makeContext({ SUCCESS: false });

    await dumpCommand.execute(['--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/Failed to query short dumps/i);
  });
});

// ─── JSON output ─────────────────────────────────────────────────────────────

describe('Dump Command - JSON output', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  test('--json prints raw JSON response', async () => {
    const response = { SUCCESS: true, DUMPS: [{ ID: '1' }], TOTAL: 1 };
    const ctx = makeContext(response);

    await dumpCommand.execute(['--json', '--timezone', 'UTC'], ctx);
    const parsed = JSON.parse(cap.output);
    expect(parsed).toEqual(response);
  });
});

// ─── Detail mode ─────────────────────────────────────────────────────────────

describe('Dump Command - detail mode', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  function makeDetailContext(listDumps, detailDump) {
    let callCount = 0;
    return {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation(() => {
          callCount++;
          if (callCount === 1) {
            return Promise.resolve({ SUCCESS: true, DUMPS: listDumps });
          }
          return Promise.resolve({ SUCCESS: true, DUMPS: [detailDump] });
        })
      }))
    };
  }

  test('renders detail fields', async () => {
    const listDumps = [{ ID: '42', USER: 'U', PROGRAM: 'P', ERROR: 'E' }];
    const detail = {
      ID: '42', ERROR: 'CX_SY_REF_IS_INITIAL', USER: 'DEVELOPER',
      PROGRAM: 'ZTEST', DATE: '2024-01-15', TIME: '10:00:00',
      WHAT_HAPPENED: 'A null reference was dereferenced.',
      ERROR_ANALYSIS: 'Check the variable before use.',
      CALL_STACK: []
    };

    const ctx = makeDetailContext(listDumps, detail);
    await dumpCommand.execute(['--detail', '1', '--timezone', 'UTC'], ctx);

    expect(cap.output).toMatch(/Short Dump Detail/i);
    expect(cap.output).toMatch(/CX_SY_REF_IS_INITIAL/);
    expect(cap.output).toMatch(/DEVELOPER/);
    expect(cap.output).toMatch(/ZTEST/);
    expect(cap.output).toMatch(/What happened/i);
    expect(cap.output).toMatch(/A null reference was dereferenced/);
    expect(cap.output).toMatch(/Error analysis/i);
  });

  test('renders call stack frames', async () => {
    const listDumps = [{ ID: '1', USER: 'U', PROGRAM: 'P', ERROR: 'E' }];
    const detail = {
      ID: '1', ERROR: 'ERR', USER: 'U', PROGRAM: 'P',
      CALL_STACK: [
        { LEVEL: '1', CLASS: 'ZCL_FOO', METHOD: 'DO_SOMETHING', LINE: '42' },
        { LEVEL: '2', CLASS: 'ZCL_BAR', METHOD: 'EXECUTE',      LINE: '10' }
      ]
    };

    const ctx = makeDetailContext(listDumps, detail);
    await dumpCommand.execute(['--detail', '1', '--timezone', 'UTC'], ctx);

    expect(cap.output).toMatch(/Call stack/i);
    expect(cap.output).toMatch(/ZCL_FOO->DO_SOMETHING/);
    expect(cap.output).toMatch(/ZCL_BAR->EXECUTE/);
    expect(cap.output).toMatch(/line 42/i);
  });

  test('error when --detail index out of range', async () => {
    const ctx = makeDetailContext([{ ID: '1', USER: 'U', PROGRAM: 'P', ERROR: 'E' }], {});

    await dumpCommand.execute(['--detail', '5', '--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/not found in results/i);
  });

  test('--json in detail mode prints raw response', async () => {
    const detailResponse = { SUCCESS: true, DUMPS: [{ ID: '1', ERROR: 'E' }] };
    let callCount = 0;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation(() => {
          callCount++;
          if (callCount === 1) return Promise.resolve({ SUCCESS: true, DUMPS: [{ ID: '1' }] });
          return Promise.resolve(detailResponse);
        })
      }))
    };

    await dumpCommand.execute(['--detail', '1', '--json', '--timezone', 'UTC'], ctx);
    const parsed = JSON.parse(cap.output);
    expect(parsed).toEqual(detailResponse);
  });
});

// ─── Timezone display ────────────────────────────────────────────────────────

describe('Dump Command - timezone display', () => {
  let cap;
  beforeEach(() => { cap = captureConsole(); });
  afterEach(() => { cap.restore(); });

  test('timezone label shown in list header', async () => {
    const ctx = makeContext({ SUCCESS: true, DUMPS: [], TOTAL: 0 });

    await dumpCommand.execute(['--timezone', 'Europe/Berlin'], ctx);
    expect(cap.output).toMatch(/Europe\/Berlin/);
  });

  test('timezone label shown in detail date line', async () => {
    let callCount = 0;
    const ctx = {
      loadConfig: jest.fn(() => ({})),
      AbapHttp: jest.fn().mockImplementation(() => ({
        fetchCsrfToken: jest.fn().mockResolvedValue('tok'),
        post: jest.fn().mockImplementation(() => {
          callCount++;
          if (callCount === 1) return Promise.resolve({ SUCCESS: true, DUMPS: [{ ID: '1' }] });
          return Promise.resolve({
            SUCCESS: true,
            DUMPS: [{ ID: '1', ERROR: 'E', UTC_TIMESTAMP: '20240115100000' }]
          });
        })
      }))
    };

    await dumpCommand.execute(['--detail', '1', '--timezone', 'UTC'], ctx);
    expect(cap.output).toMatch(/UTC/);
  });
});
