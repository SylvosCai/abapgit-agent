/**
 * Unit tests for the `unit` command (backend-based implementation).
 *
 * The command POSTs to /sap/bc/z_abapgit_agent/unit and renders results.
 * These tests cover:
 *   • _findMethodCoverage      heuristic
 *   • _normalizeBackendResult  snake_case → camelCase mapping
 *   • _buildUnitJUnit          per-method <properties> emission
 *   • execute()                end-to-end CLI flow with mocked AbapHttp
 */

jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn((p) => (String(p).includes('.abapgit-agent.json') ? '{}' : 'mock')),
  mkdirSync: jest.fn(),
  writeFileSync: jest.fn(),
}));

jest.mock('path', () => ({
  isAbsolute: jest.fn(() => false),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => '/' + args.join('/')),
  basename: jest.fn((p) => p.split('/').pop()),
  dirname: jest.fn((p) => p.split('/').slice(0, -1).join('/') || '.'),
}));

// Mock AbapHttp — captured request lets tests assert on the payload
const mockAbapPost = jest.fn();
jest.mock('../../src/utils/abap-http', () => ({
  AbapHttp: jest.fn().mockImplementation(() => ({
    fetchCsrfToken: jest.fn().mockResolvedValue('csrf-token'),
    csrfToken: 'csrf-token',
    post: mockAbapPost,
  })),
}));

// Mock project config so getCoverageConfig returns deterministic defaults
jest.mock('../../src/config', () => {
  const projectConfig = { threshold: 0, mode: 'fail', excludes: [] };
  return {
    getCoverageConfig: jest.fn(() => projectConfig),
    __setProjectCoverage: (cfg) => Object.assign(projectConfig, cfg),
  };
});

const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

const unitCmd = require('../../src/commands/unit');
const { _buildUnitJUnit, _normalizeBackendResult, _findMethodCoverage } = unitCmd;
const config = require('../../src/config');

const mockContext = {
  loadConfig: () => ({ host: 'h', sapport: 1, client: '100', user: 'u', password: 'p' }),
};

beforeEach(() => {
  mockAbapPost.mockReset();
  mockExit.mockClear();
  // Re-install the throw-on-exit behaviour each test (afterEach restoreAllMocks would clear it)
  mockExit.mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
  jest.spyOn(console, 'log').mockImplementation(() => {});
  jest.spyOn(console, 'warn').mockImplementation(() => {});
  jest.spyOn(console, 'error').mockImplementation(() => {});
  config.__setProjectCoverage({ threshold: 0, mode: 'fail', excludes: [] });
  // Reset fs.existsSync to default true (some tests use mockReturnValueOnce)
  require('fs').existsSync.mockReturnValue(true);
});

afterEach(() => {
  // Don't call restoreAllMocks — would un-do mockExit and the require-time mocks.
  // Just clear console spies so they reset per-test.
  jest.clearAllMocks();
});

// ---------------------------------------------------------------------------
// _findMethodCoverage heuristic
// ---------------------------------------------------------------------------
describe('_findMethodCoverage', () => {
  const cov = [
    { name: 'GET_INSTANCE',                          totalStatements: 3,  executedStatements: 3 },
    { name: 'ZIF_FOO~PARSE_FILE_TO_OBJECT',          totalStatements: 48, executedStatements: 35 },
    { name: 'ZIF_FOO~CONVERT_INDEX_TO_CM_SUFFIX',    totalStatements: 82, executedStatements: 42 },
  ];

  test('exact name match (no prefix)', () => {
    const r = _findMethodCoverage('GET_INSTANCE', cov);
    expect(r && r.name).toBe('GET_INSTANCE');
  });

  test('strips leading TEST_', () => {
    const r = _findMethodCoverage('TEST_GET_INSTANCE', cov);
    expect(r && r.name).toBe('GET_INSTANCE');
  });

  test('matches interface-prefixed coverage method', () => {
    const r = _findMethodCoverage('TEST_PARSE_FILE_TO_OBJECT', cov);
    expect(r && r.name).toBe('ZIF_FOO~PARSE_FILE_TO_OBJECT');
  });

  test('returns null for unknown method', () => {
    expect(_findMethodCoverage('TEST_DOES_NOT_EXIST', cov)).toBeNull();
  });

  test('returns null for empty coverage list', () => {
    expect(_findMethodCoverage('TEST_FOO', [])).toBeNull();
    expect(_findMethodCoverage('TEST_FOO', null)).toBeNull();
  });

  test('returns null for empty test name', () => {
    expect(_findMethodCoverage('', cov)).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// _normalizeBackendResult
// ---------------------------------------------------------------------------
describe('_normalizeBackendResult', () => {
  test('maps snake_case → camelCase, includes coverage when present', () => {
    const raw = {
      class_name: 'ZCL_FOO',
      file: 'abap/zcl_foo.clas.testclasses.abap',
      methods: [
        { test_class_name: 'LTCL_FOO', name: 'TEST_PASS', passed: true,  execution_time: 0.001,
          kind: '', title: '', details: [], stack: [] },
        { test_class_name: 'LTCL_FOO', name: 'TEST_FAIL', passed: false, execution_time: 0.002,
          kind: 'failedAssertion', title: '[FM02] reason', details: ['Detail line'],
          stack: [{ name: 'ZCL_FOO===CCAU 42 TEST_FAIL', line: 42 }] },
      ],
      coverage_present: true,
      coverage: {
        total_statements: 100,
        executed_statements: 80,
        statement_rate: 80,
        methods: [
          { name: 'M1', total_statements: 10, executed_statements: 5, rate: 50 },
        ],
      },
    };
    const r = _normalizeBackendResult(raw, '');
    expect(r.className).toBe('ZCL_FOO');
    expect(r.file).toBe('abap/zcl_foo.clas.testclasses.abap');
    expect(r.methods).toHaveLength(2);
    expect(r.methods[0].testClassName).toBe('LTCL_FOO');
    expect(r.methods[0].executionTime).toBe(0.001);
    expect(r.methods[0].passed).toBe(true);
    // Empty kind/title/details get omitted on pass
    expect(r.methods[0].kind).toBeUndefined();
    expect(r.methods[0].details).toBeUndefined();

    expect(r.methods[1].passed).toBe(false);
    expect(r.methods[1].kind).toBe('failedAssertion');
    expect(r.methods[1].details).toEqual(['Detail line']);
    expect(r.methods[1].stack).toEqual([{ name: 'ZCL_FOO===CCAU 42 TEST_FAIL', line: 42 }]);

    expect(r.coverage).toEqual({
      totalStatements: 100,
      executedStatements: 80,
      statementRate: 80,
      methods: [{ name: 'M1', totalStatements: 10, executedStatements: 5, rate: 50 }],
    });
  });

  test('omits coverage when coverage_present is false', () => {
    const raw = {
      class_name: 'ZCL_FOO',
      methods: [],
      coverage_present: false,
      coverage: { total_statements: 0, executed_statements: 0, statement_rate: 0, methods: [] },
    };
    const r = _normalizeBackendResult(raw, '');
    expect(r.coverage).toBeUndefined();
  });

  test('handles empty methods array', () => {
    const r = _normalizeBackendResult({ class_name: 'ZCL_X', methods: [] }, '');
    expect(r.methods).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// _buildUnitJUnit — per-method <properties>
// ---------------------------------------------------------------------------
describe('_buildUnitJUnit', () => {
  const baseResult = {
    className: 'ZCL_X',
    methods: [
      { testClassName: 'LTCL_X', name: 'TEST_FOO', passed: true, executionTime: 0.001 },
      { testClassName: 'LTCL_X', name: 'TEST_BAR', passed: true, executionTime: 0.002 },
      { testClassName: 'LTCL_X', name: 'TEST_FAIL_ME', passed: false, executionTime: 0.003,
        kind: 'failedAssertion', title: 'oops' },
    ],
    coverage: {
      totalStatements: 30,
      executedStatements: 20,
      statementRate: 66.6,
      methods: [
        { name: 'FOO', totalStatements: 10, executedStatements: 9, rate: 90 },
        { name: 'BAR', totalStatements: 20, executedStatements: 11, rate: 55 },
      ],
    },
  };

  test('emits per-suite coverage properties on <testsuite>', () => {
    const xml = _buildUnitJUnit([baseResult]);
    expect(xml).toContain('<property name="coverage.rate"          value="66.6"/>');
    expect(xml).toContain('<property name="coverage.lines.total"   value="30"/>');
    expect(xml).toContain('<property name="coverage.lines.covered" value="20"/>');
  });

  test('emits per-method coverage properties on matching <testcase>', () => {
    const xml = _buildUnitJUnit([baseResult]);
    // TEST_FOO → FOO match
    const fooBlock = xml.split('TEST_FOO')[1].split('</testcase>')[0];
    expect(fooBlock).toContain('<property name="coverage.method.lines.total"   value="10"/>');
    expect(fooBlock).toContain('<property name="coverage.method.rate"          value="90"/>');
  });

  test('does not emit per-method properties when no match', () => {
    const xml = _buildUnitJUnit([baseResult]);
    // TEST_FAIL_ME has no matching coverage method; properties should be omitted in its testcase
    const failBlock = xml.split('TEST_FAIL_ME')[1].split('</testcase>')[0];
    expect(failBlock).not.toContain('coverage.method.');
    expect(failBlock).toContain('<failure');
  });

  test('threshold failure synthesizes a coverage_threshold testcase', () => {
    const r = { ...baseResult, thresholdFailure: 'below 80%' };
    const xml = _buildUnitJUnit([r]);
    expect(xml).toContain('<testcase name="coverage_threshold"');
    expect(xml).toContain('below 80%');
  });

  test('class without coverage emits no <properties> on suite', () => {
    const r = { className: 'ZCL_NOCOV', methods: [{ testClassName: 'LTCL_X', name: 'A', passed: true, executionTime: 0 }] };
    const xml = _buildUnitJUnit([r]);
    expect(xml).not.toContain('coverage.rate');
  });
});

// ---------------------------------------------------------------------------
// execute() — full CLI flow
// ---------------------------------------------------------------------------
describe('execute', () => {
  function mockBackendOk(coverage = false) {
    const res = {
      results: [
        {
          class_name: 'ZCL_FOO',
          file: 'abap/zcl_foo.clas.testclasses.abap',
          methods: [
            { test_class_name: 'LTCL_FOO', name: 'TEST_A', passed: true, execution_time: 0.001,
              kind: '', title: '', details: [], stack: [] },
          ],
          coverage_present: coverage,
          coverage: coverage ? {
            total_statements: 50, executed_statements: 40, statement_rate: 80,
            methods: [{ name: 'A', total_statements: 50, executed_statements: 40, rate: 80 }],
          } : null,
        },
      ],
    };
    mockAbapPost.mockResolvedValue(res);
  }

  test('successful run with no coverage', async () => {
    mockBackendOk(false);
    await unitCmd.execute(['--files', 'abap/zcl_foo.clas.testclasses.abap'], mockContext);
    expect(mockAbapPost).toHaveBeenCalledWith(
      '/sap/bc/z_abapgit_agent/unit',
      { files: ['abap/zcl_foo.clas.testclasses.abap'], coverage: false },
      { csrfToken: 'csrf-token' }
    );
    expect(mockExit).not.toHaveBeenCalled();
  });

  test('--coverage propagates flag to backend', async () => {
    mockBackendOk(true);
    await unitCmd.execute(['--files', 'abap/zcl_foo.clas.testclasses.abap', '--coverage'], mockContext);
    const callArgs = mockAbapPost.mock.calls[0];
    expect(callArgs[1]).toEqual({ files: ['abap/zcl_foo.clas.testclasses.abap'], coverage: true });
  });

  test('multiple files passed through deduplicated', async () => {
    mockBackendOk(false);
    await unitCmd.execute([
      '--files',
      'abap/zcl_foo.clas.testclasses.abap,abap/zcl_bar.clas.testclasses.abap',
    ], mockContext);
    const callArgs = mockAbapPost.mock.calls[0];
    expect(callArgs[1].files).toEqual([
      'abap/zcl_foo.clas.testclasses.abap',
      'abap/zcl_bar.clas.testclasses.abap',
    ]);
  });

  test('missing file fails before calling backend', async () => {
    require('fs').existsSync.mockReturnValueOnce(false);
    await expect(
      unitCmd.execute(['--files', 'abap/missing.clas.testclasses.abap'], mockContext)
    ).rejects.toThrow('process.exit(1)');
    expect(mockAbapPost).not.toHaveBeenCalled();
  });

  test('failed test triggers exit 1', async () => {
    mockAbapPost.mockResolvedValue({
      results: [{
        class_name: 'ZCL_FOO',
        file: 'abap/zcl_foo.clas.testclasses.abap',
        methods: [
          { test_class_name: 'LTCL_FOO', name: 'TEST_FAIL', passed: false, execution_time: 0,
            kind: 'failedAssertion', title: '[FM02] oops', details: [], stack: [] },
        ],
        coverage_present: false,
        coverage: null,
      }],
    });
    await expect(
      unitCmd.execute(['--files', 'abap/zcl_foo.clas.testclasses.abap'], mockContext)
    ).rejects.toThrow('process.exit(1)');
  });

  test('backend error surfaces as command failure', async () => {
    mockAbapPost.mockRejectedValue(Object.assign(new Error('Internal Server Error'), { statusCode: 500 }));
    await expect(
      unitCmd.execute(['--files', 'abap/zcl_foo.clas.testclasses.abap'], mockContext)
    ).rejects.toThrow('process.exit(1)');
  });

  test('--help short-circuits without calling backend', async () => {
    await unitCmd.execute(['--help'], mockContext);
    expect(mockAbapPost).not.toHaveBeenCalled();
  });

  test('project config threshold fires when CLI flag absent', async () => {
    config.__setProjectCoverage({ threshold: 90, mode: 'fail', excludes: [] });
    mockBackendOk(true);   // 80% coverage – below 90 threshold
    await expect(
      unitCmd.execute(['--files', 'abap/zcl_foo.clas.testclasses.abap', '--coverage'], mockContext)
    ).rejects.toThrow('process.exit(1)');
  });

  test('CLI threshold overrides project config', async () => {
    config.__setProjectCoverage({ threshold: 90, mode: 'fail', excludes: [] });
    mockBackendOk(true);   // 80% coverage; CLI sets threshold=50 (below)
    await unitCmd.execute([
      '--files', 'abap/zcl_foo.clas.testclasses.abap',
      '--coverage',
      '--coverage-threshold', '50',
    ], mockContext);
    expect(mockExit).not.toHaveBeenCalled();
  });

  test('warn mode does not exit 1 when below threshold', async () => {
    config.__setProjectCoverage({ threshold: 90, mode: 'warn', excludes: [] });
    mockBackendOk(true);
    await unitCmd.execute([
      '--files', 'abap/zcl_foo.clas.testclasses.abap',
      '--coverage',
    ], mockContext);
    expect(mockExit).not.toHaveBeenCalled();
  });

  test('--junit-output writes XML with coverage properties', async () => {
    mockBackendOk(true);
    const fs = require('fs');
    await unitCmd.execute([
      '--files', 'abap/zcl_foo.clas.testclasses.abap',
      '--coverage',
      '--junit-output', 'reports/u.xml',
    ], mockContext);
    expect(fs.writeFileSync).toHaveBeenCalled();
    const writtenXml = fs.writeFileSync.mock.calls[0][1];
    expect(writtenXml).toContain('coverage.rate');
    expect(writtenXml).toContain('coverage.method');
  });
});
