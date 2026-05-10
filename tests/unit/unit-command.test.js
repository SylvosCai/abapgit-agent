/**
 * Unit tests for unit command (ADT-based implementation)
 */

// Mock fs module
jest.mock('fs', () => ({
  existsSync: jest.fn(() => true),
  readFileSync: jest.fn(() => 'mock content'),
  mkdirSync: jest.fn(),
  writeFileSync: jest.fn(),
}));

// Mock path module
jest.mock('path', () => ({
  isAbsolute: jest.fn(() => false),
  join: jest.fn((...args) => args.join('/')),
  resolve: jest.fn((...args) => '/' + args.join('/')),
  basename: jest.fn((p) => p.split('/').pop()),
  dirname: jest.fn((p) => p.split('/').slice(0, -1).join('/') || '.'),
}));

// Mock AdtHttp
jest.mock('../../src/utils/adt-http', () => ({
  AdtHttp: jest.fn().mockImplementation(() => ({
    fetchCsrfToken: jest.fn().mockResolvedValue('token'),
    post: jest.fn(),
  })),
}));

const mockExit = jest.spyOn(process, 'exit').mockImplementation((code) => {
  throw new Error(`process.exit(${code})`);
});

const { _buildRunConfigXml, _parseRunResult, _buildUnitJUnit } = require('../../src/commands/unit');

// ─── XML builder ─────────────────────────────────────────────────────────────

describe('buildRunConfigXml', () => {
  test('includes correct ADT URI', () => {
    const xml = _buildRunConfigXml('/sap/bc/adt/oo/classes/zcl_my_test', false);
    expect(xml).toContain('adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_test"');
  });

  test('sets coverage active=false when disabled', () => {
    const xml = _buildRunConfigXml('/sap/bc/adt/oo/classes/zcl_foo', false);
    expect(xml).toContain('<coverage active="false"/>');
  });

  test('sets coverage active=true when enabled', () => {
    const xml = _buildRunConfigXml('/sap/bc/adt/oo/classes/zcl_foo', true);
    expect(xml).toContain('<coverage active="true"/>');
  });

  test('includes required namespace declarations', () => {
    const xml = _buildRunConfigXml('/sap/bc/adt/oo/classes/zcl_foo', false);
    expect(xml).toContain('xmlns:aunit="http://www.sap.com/adt/aunit"');
    expect(xml).toContain('xmlns:adtcore="http://www.sap.com/adt/core"');
  });
});

// ─── XML response parser ──────────────────────────────────────────────────────

const PASS_XML = `<?xml version="1.0" encoding="UTF-8"?>
<aunit:runResult xmlns:aunit="http://www.sap.com/adt/aunit"
                 xmlns:adtcore="http://www.sap.com/adt/core">
  <program adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_test" adtcore:name="ZCL_MY_TEST">
    <testClasses>
      <testClass adtcore:name="ZCL_MY_TEST">
        <testMethods>
          <testMethod adtcore:name="TEST_PASSES" executionTime="0.001">
            <alerts/>
          </testMethod>
          <testMethod adtcore:name="TEST_ALSO_PASSES" executionTime="0.002">
            <alerts/>
          </testMethod>
        </testMethods>
      </testClass>
    </testClasses>
  </program>
</aunit:runResult>`;

const FAIL_XML = `<?xml version="1.0" encoding="UTF-8"?>
<aunit:runResult xmlns:aunit="http://www.sap.com/adt/aunit"
                 xmlns:adtcore="http://www.sap.com/adt/core">
  <program adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_test" adtcore:name="ZCL_MY_TEST">
    <testClasses>
      <testClass adtcore:name="LTCL_UNIT_TEST">
        <testMethods>
          <testMethod adtcore:name="TEST_PASSES" executionTime="0.001">
            <alerts/>
          </testMethod>
          <testMethod adtcore:name="TEST_FAILS" executionTime="0.003">
            <alerts>
              <alert kind="failedAssertion" severity="critical">
                <title>Assertion failed</title>
                <details>
                  <detail text="Expected 1 but was 2"/>
                  <detail text="at ZCL_MY_TEST line 42"/>
                </details>
                <stack>
                  <stackEntry adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_test/source/main#start=42" adtcore:name="ZCL_MY_TEST" adtcore:type="CLAS/OC" adtcore:description=""/>
                </stack>
              </alert>
            </alerts>
          </testMethod>
        </testMethods>
      </testClass>
    </testClasses>
  </program>
</aunit:runResult>`;

const SLOW_XML = `<?xml version="1.0" encoding="UTF-8"?>
<aunit:runResult xmlns:aunit="http://www.sap.com/adt/aunit"
                 xmlns:adtcore="http://www.sap.com/adt/core">
  <program adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_test" adtcore:name="ZCL_MY_TEST">
    <testClasses>
      <testClass adtcore:name="LTCL_UNIT_TEST">
        <testMethods>
          <testMethod adtcore:name="TEST_FAST" executionTime="0.001">
            <alerts/>
          </testMethod>
          <testMethod adtcore:name="TEST_SLOW" executionTime="1.234">
            <alerts/>
          </testMethod>
        </testMethods>
      </testClass>
    </testClasses>
  </program>
</aunit:runResult>`;

const COVERAGE_XML = `<?xml version="1.0" encoding="UTF-8"?>
<aunit:runResult xmlns:aunit="http://www.sap.com/adt/aunit"
                 xmlns:adtcore="http://www.sap.com/adt/core">
  <program adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_test" adtcore:name="ZCL_MY_TEST">
    <coverage adtcore:lines_total="41" adtcore:lines_covered="27" adtcore:coverage_rate="65.9"/>
    <testClasses>
      <testClass adtcore:name="ZCL_MY_TEST">
        <testMethods>
          <testMethod adtcore:name="TEST_PASSES" executionTime="0.001">
            <alerts/>
          </testMethod>
        </testMethods>
      </testClass>
    </testClasses>
  </program>
</aunit:runResult>`;

const EMPTY_XML = `<?xml version="1.0" encoding="UTF-8"?>
<aunit:runResult xmlns:aunit="http://www.sap.com/adt/aunit"
                 xmlns:adtcore="http://www.sap.com/adt/core">
  <program adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_test" adtcore:name="ZCL_MY_TEST">
    <testClasses/>
  </program>
</aunit:runResult>`;

describe('parseRunResult', () => {
  test('returns className from argument', () => {
    const r = _parseRunResult(PASS_XML, 'ZCL_MY_TEST');
    expect(r.className).toBe('ZCL_MY_TEST');
  });

  test('parses all passing methods', () => {
    const r = _parseRunResult(PASS_XML, 'ZCL_MY_TEST');
    expect(r.methods).toHaveLength(2);
    expect(r.methods.every(m => m.passed)).toBe(true);
    expect(r.methods.map(m => m.name)).toEqual(['TEST_PASSES', 'TEST_ALSO_PASSES']);
  });

  test('marks failed method as not passed', () => {
    const r = _parseRunResult(FAIL_XML, 'ZCL_MY_TEST');
    expect(r.methods).toHaveLength(2);
    const fail = r.methods.find(m => m.name === 'TEST_FAILS');
    expect(fail.passed).toBe(false);
    expect(fail.kind).toBe('failedAssertion');
    expect(fail.title).toBe('Assertion failed');
    expect(fail.details).toContain('Expected 1 but was 2');
    expect(fail.details).toContain('at ZCL_MY_TEST line 42');
  });

  test('parses testClassName from testClass element', () => {
    const r = _parseRunResult(FAIL_XML, 'ZCL_MY_TEST');
    expect(r.methods[0].testClassName).toBe('LTCL_UNIT_TEST');
    expect(r.methods[1].testClassName).toBe('LTCL_UNIT_TEST');
  });

  test('parses executionTime for each method', () => {
    const r = _parseRunResult(FAIL_XML, 'ZCL_MY_TEST');
    const pass = r.methods.find(m => m.name === 'TEST_PASSES');
    const fail = r.methods.find(m => m.name === 'TEST_FAILS');
    expect(pass.executionTime).toBe(0.001);
    expect(fail.executionTime).toBe(0.003);
  });

  test('parses stack entry with line number', () => {
    const r = _parseRunResult(FAIL_XML, 'ZCL_MY_TEST');
    const fail = r.methods.find(m => m.name === 'TEST_FAILS');
    expect(fail.stack).toHaveLength(1);
    expect(fail.stack[0].name).toBe('ZCL_MY_TEST');
    expect(fail.stack[0].line).toBe(42);
  });

  test('passing method alongside failing method remains passed', () => {
    const r = _parseRunResult(FAIL_XML, 'ZCL_MY_TEST');
    const pass = r.methods.find(m => m.name === 'TEST_PASSES');
    expect(pass.passed).toBe(true);
  });

  test('returns null coverageStats when no coverage element', () => {
    const r = _parseRunResult(PASS_XML, 'ZCL_MY_TEST');
    expect(r.coverageStats).toBeNull();
  });

  test('parses coverage stats from XML', () => {
    const r = _parseRunResult(COVERAGE_XML, 'ZCL_MY_TEST');
    expect(r.coverageStats).not.toBeNull();
    expect(r.coverageStats.totalLines).toBe(41);
    expect(r.coverageStats.coveredLines).toBe(27);
    expect(r.coverageStats.coverageRate).toBe(65.9);
  });

  test('returns empty methods for no tests', () => {
    const r = _parseRunResult(EMPTY_XML, 'ZCL_MY_TEST');
    expect(r.methods).toHaveLength(0);
  });
});

// ─── JUnit builder ────────────────────────────────────────────────────────────

describe('buildUnitJUnit', () => {
  test('emits individual testcase per passing method', () => {
    const results = [{ className: 'ZCL_MY_TEST', methods: [{ name: 'TEST_PASSES', passed: true }], coverageStats: null }];
    const xml = _buildUnitJUnit(results);
    expect(xml).toContain('<testcase name="TEST_PASSES" classname="Unit Tests / ZCL_MY_TEST"/>');
    expect(xml).not.toContain('passing test(s)');
  });

  test('emits failure element for failed method', () => {
    const results = [{
      className: 'ZCL_MY_TEST',
      methods: [{ name: 'TEST_FAILS', passed: false, kind: 'failedAssertion', title: 'Assertion failed', details: ['Expected 1 but was 2'] }],
      coverageStats: null,
    }];
    const xml = _buildUnitJUnit(results);
    expect(xml).toContain('<testcase name="TEST_FAILS" classname="Unit Tests / ZCL_MY_TEST"');
    expect(xml).toContain('<failure type="failedAssertion"');
    expect(xml).toContain('Assertion failed');
    expect(xml).toContain('Expected 1 but was 2');
  });

  test('emits time attribute from executionTime', () => {
    const results = [{ className: 'ZCL_MY_TEST', methods: [{ name: 'TEST_PASSES', passed: true, executionTime: 0.123 }], coverageStats: null }];
    const xml = _buildUnitJUnit(results);
    expect(xml).toContain('time="0.123"');
  });

  test('uses ZCL.LTCL hierarchy for classname attribute when testClassName present', () => {
    const results = [{ className: 'ZCL_MY_TEST', methods: [{ name: 'TEST_PASSES', passed: true, testClassName: 'LTCL_UNIT_TEST', executionTime: 0 }], coverageStats: null }];
    const xml = _buildUnitJUnit(results);
    expect(xml).toContain('classname="Unit Tests / ZCL_MY_TEST.LTCL_UNIT_TEST"');
  });

  test('emits (no tests) when methods array is empty', () => {
    const results = [{ className: 'ZCL_EMPTY', methods: [], coverageStats: null }];
    const xml = _buildUnitJUnit(results);
    expect(xml).toContain('(no tests)');
  });

  test('emits coverage properties when coverageStats present', () => {
    const results = [{
      className: 'ZCL_MY_TEST',
      methods: [{ name: 'TEST_PASSES', passed: true }],
      coverageStats: { totalLines: 41, coveredLines: 27, coverageRate: 65.9 },
    }];
    const xml = _buildUnitJUnit(results);
    expect(xml).toContain('<property name="coverage.rate"');
    expect(xml).toContain('value="65.9"');
    expect(xml).toContain('<property name="coverage.lines.total"');
    expect(xml).toContain('value="41"');
    expect(xml).toContain('<property name="coverage.lines.covered"');
    expect(xml).toContain('value="27"');
  });

  test('injects coverage_threshold failure when thresholdFailure set', () => {
    const results = [{
      className: 'ZCL_MY_TEST',
      methods: [{ name: 'TEST_PASSES', passed: true }],
      coverageStats: { totalLines: 10, coveredLines: 2, coverageRate: 20 },
      thresholdFailure: 'ZCL_MY_TEST: coverage 20% is below threshold 80%',
    }];
    const xml = _buildUnitJUnit(results);
    expect(xml).toContain('<testcase name="coverage_threshold"');
    expect(xml).toContain('coverage 20% is below threshold 80%');
  });

  test('escapes XML special characters in method names', () => {
    const results = [{
      className: 'ZCL_MY_TEST',
      methods: [{ name: 'TEST_<SPECIAL>&"', passed: true }],
      coverageStats: null,
    }];
    const xml = _buildUnitJUnit(results);
    expect(xml).toContain('TEST_&lt;SPECIAL&gt;&amp;&quot;');
    expect(xml).not.toContain('TEST_<SPECIAL>');
  });

  test('testsuite failures count includes threshold failure', () => {
    const results = [{
      className: 'ZCL_MY_TEST',
      methods: [{ name: 'TEST_PASSES', passed: true }],
      coverageStats: { totalLines: 10, coveredLines: 1, coverageRate: 10 },
      thresholdFailure: 'below threshold',
    }];
    const xml = _buildUnitJUnit(results);
    expect(xml).toContain('failures="1"');
  });
});

// ─── CLI integration (execute) ────────────────────────────────────────────────

describe('Unit Command - execute', () => {
  const { AdtHttp } = require('../../src/utils/adt-http');
  const unitCmd = require('../../src/commands/unit');
  const fs = require('fs');

  let mockAdt;
  let consoleOutput;
  let consoleErrors;

  beforeEach(() => {
    consoleOutput = [];
    consoleErrors = [];
    jest.spyOn(process, 'exit').mockImplementation((code) => { throw new Error(`process.exit(${code})`); });
    jest.spyOn(console, 'log').mockImplementation((...a) => consoleOutput.push(a.join(' ')));
    jest.spyOn(console, 'error').mockImplementation((...a) => consoleErrors.push(a.join(' ')));
    jest.spyOn(console, 'warn').mockImplementation(() => {});

    mockAdt = { fetchCsrfToken: jest.fn().mockResolvedValue('token'), post: jest.fn() };
    AdtHttp.mockImplementation(() => mockAdt);
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  function makeContext() {
    return { loadConfig: () => ({ host: 'host', user: 'u', password: 'p', client: '001', sapport: '50000', protocol: 'https' }) };
  }

  test('prints ✅ when all tests pass', async () => {
    mockAdt.post.mockResolvedValue({ body: PASS_XML });
    await unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], makeContext());
    expect(consoleOutput.some(l => l.includes('✅') && l.includes('ZCL_MY_TEST'))).toBe(true);
  });

  test('exits 1 and prints ❌ when tests fail', async () => {
    mockAdt.post.mockResolvedValue({ body: FAIL_XML });
    await expect(
      unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], makeContext())
    ).rejects.toThrow('process.exit(1)');
    expect(consoleOutput.some(l => l.includes('❌') && l.includes('ZCL_MY_TEST'))).toBe(true);
  });

  test('prints ➖ when no tests found', async () => {
    mockAdt.post.mockResolvedValue({ body: EMPTY_XML });
    await unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], makeContext());
    expect(consoleOutput.some(l => l.includes('➖') && l.includes('ZCL_MY_TEST'))).toBe(true);
  });

  test('shows individual test count in output', async () => {
    mockAdt.post.mockResolvedValue({ body: PASS_XML });
    await unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], makeContext());
    expect(consoleOutput.some(l => l.includes('Tests: 2') && l.includes('Passed: 2') && l.includes('Failed: 0'))).toBe(true);
  });

  test('shows coverage stats when --coverage', async () => {
    mockAdt.post.mockResolvedValue({ body: COVERAGE_XML });
    await unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap', '--coverage'], makeContext());
    expect(consoleOutput.some(l => l.includes('Coverage: 65.9%'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('Total Lines: 41'))).toBe(true);
  });

  test('builds correct ADT URI from file name', async () => {
    mockAdt.post.mockResolvedValue({ body: PASS_XML });
    await unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], makeContext());
    const [url, body] = mockAdt.post.mock.calls[0];
    expect(url).toBe('/sap/bc/adt/abapunit/testruns');
    expect(body).toContain('/sap/bc/adt/oo/classes/zcl_my_test');
  });

  test('uses application/* content-type and accept', async () => {
    mockAdt.post.mockResolvedValue({ body: PASS_XML });
    await unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], makeContext());
    const [, , opts] = mockAdt.post.mock.calls[0];
    expect(opts.contentType).toBe('application/*');
    expect(opts.accept).toBe('application/*');
  });

  test('file not found logs error and exits 1', async () => {
    fs.existsSync.mockReturnValueOnce(false);
    await expect(
      unitCmd.execute(['--files', 'src/zcl_missing.clas.testclasses.abap'], makeContext())
    ).rejects.toThrow('process.exit(1)');
    expect(consoleErrors.some(l => l.includes('File not found'))).toBe(true);
  });

  test('writes JUnit XML file', async () => {
    mockAdt.post.mockResolvedValue({ body: PASS_XML });
    await unitCmd.execute(
      ['--files', 'src/zcl_my_test.clas.testclasses.abap', '--junit-output', 'reports/unit.xml'],
      makeContext()
    );
    expect(fs.writeFileSync).toHaveBeenCalled();
    const [, xmlContent] = fs.writeFileSync.mock.calls[0];
    expect(xmlContent).toContain('<testsuites>');
    expect(xmlContent).toContain('<testcase name="TEST_PASSES"');
  });

  test('JSON output mode suppresses console output and prints JSON', async () => {
    mockAdt.post.mockResolvedValue({ body: PASS_XML });
    const logSpy = jest.spyOn(console, 'log').mockImplementation(() => {});
    await unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap', '--json'], makeContext());
    const jsonCall = logSpy.mock.calls.find(a => {
      try { JSON.parse(a[0]); return true; } catch { return false; }
    });
    expect(jsonCall).toBeDefined();
    const parsed = JSON.parse(jsonCall[0]);
    expect(Array.isArray(parsed)).toBe(true);
    expect(parsed[0].className).toBe('ZCL_MY_TEST');
  });

  test('shows failed method testClassName=>name and detail lines', async () => {
    mockAdt.post.mockResolvedValue({ body: FAIL_XML });
    await expect(
      unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], makeContext())
    ).rejects.toThrow('process.exit(1)');
    const output = consoleOutput.join('\n');
    expect(output).toContain('LTCL_UNIT_TEST=>TEST_FAILS');
    expect(output).toContain('Assertion failed');
    expect(output).toContain('Expected 1 but was 2');
  });

  test('shows slow test summary when method exceeds 0.1s', async () => {
    mockAdt.post.mockResolvedValue({ body: SLOW_XML });
    await unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap'], makeContext());
    expect(consoleOutput.some(l => l.includes('⏱') && l.includes('TEST_SLOW') && l.includes('1.234s'))).toBe(true);
    expect(consoleOutput.some(l => l.includes('TEST_FAST') && l.includes('⏱'))).toBe(false);
  });

  test('--verbose shows per-method time table', async () => {
    mockAdt.post.mockResolvedValue({ body: PASS_XML });
    await unitCmd.execute(['--files', 'src/zcl_my_test.clas.testclasses.abap', '--verbose'], makeContext());
    const output = consoleOutput.join('\n');
    expect(output).toContain('Method times:');
    expect(output).toContain('TEST_PASSES');
  });

  test('coverage threshold fail mode exits 1 and injects failure', async () => {
    mockAdt.post.mockResolvedValue({ body: COVERAGE_XML });
    await expect(
      unitCmd.execute(
        ['--files', 'src/zcl_my_test.clas.testclasses.abap', '--coverage', '--coverage-threshold', '80'],
        makeContext()
      )
    ).rejects.toThrow('process.exit(1)');
  });

  test('coverage threshold warn mode does not exit 1', async () => {
    mockAdt.post.mockResolvedValue({ body: COVERAGE_XML });
    await unitCmd.execute(
      ['--files', 'src/zcl_my_test.clas.testclasses.abap', '--coverage', '--coverage-threshold', '80', '--coverage-mode', 'warn'],
      makeContext()
    );
    // should not throw
  });
});
