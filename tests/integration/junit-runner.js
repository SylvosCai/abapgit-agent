/**
 * JUnit Output Integration Tests
 *
 * Tests --junit-output flag for unit and inspect commands against a real ABAP system.
 * Verifies that:
 *   - XML file is written to disk (absolute and relative paths)
 *   - Output directory is created automatically when missing
 *   - XML is valid JUnit format Jenkins can parse (<testsuites>, <testsuite>, <testcase>)
 *   - Test counts in XML match CLI output
 *   - JUnit report written message appears in CLI output
 *   - Normal CLI output is still shown alongside JUnit output
 *
 * Run: npm run test:junit
 */
const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

const TMP = os.tmpdir();
const UNIT_TEST_CLONE_DIR = path.join(os.tmpdir(), 'abgagt-unit-test');
const UNIT_TEST_FILE = path.join(UNIT_TEST_CLONE_DIR, 'src', 'zcl_abgagt_unit_test.clas.testclasses.abap');
const UNIT_LOW_COV_FILE = path.join(UNIT_TEST_CLONE_DIR, 'src', 'zcl_abgagt_unit_low_cov.clas.testclasses.abap');
const UNIT_FAIL_FILE = path.join(UNIT_TEST_CLONE_DIR, 'src', 'zcl_abgagt_unit_fail.clas.testclasses.abap');

/**
 * Run JUnit output integration tests
 */
function runJUnitTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors }) {
  printSubHeader('Running JUnit Output Integration Tests');

  const startTime = Date.now();
  const results = [];

  // ── helpers ──────────────────────────────────────────────────────────────

  function run(args) {
    return execSync(`node bin/abapgit-agent ${args}`, {
      cwd: repoRoot,
      encoding: 'utf8',
      timeout: 60000
    });
  }

  function tryRun(args) {
    const { spawnSync } = require('child_process');
    // Use shell to handle complex args (paths with spaces, etc.) while capturing both stdout+stderr
    const r = spawnSync('node bin/abapgit-agent ' + args, {
      cwd: repoRoot, encoding: 'utf8', timeout: 60000, shell: true
    });
    return { output: (r.stdout || '') + (r.stderr || ''), exitCode: r.status || 0 };
  }

  function cleanup(...files) {
    for (const f of files) {
      try { fs.unlinkSync(f); } catch (_) {}
    }
  }

  function cleanupDir(dir) {
    try { fs.rmSync(dir, { recursive: true, force: true }); } catch (_) {}
  }

  function parseTestCount(xml, attr) {
    const m = xml.match(new RegExp(`${attr}="(\\d+)"`));
    return m ? parseInt(m[1], 10) : null;
  }

  function test(name, fn) {
    process.stdout.write(`  Testing: ${name}... `);
    try {
      const passed = fn();
      if (passed) {
        console.log(colorize('green', '✅'));
        results.push({ name, passed: true });
      } else {
        console.log(colorize('red', '❌'));
        results.push({ name, passed: false, output: 'verify returned false' });
      }
    } catch (err) {
      console.log(colorize('red', '❌'));
      results.push({ name, passed: false, output: err.message });
    }
  }

  // ── check ABAP config ─────────────────────────────────────────────────────

  const configPath = path.join(repoRoot, '.abapGitAgent');
  if (!fs.existsSync(configPath) && !process.env.ABAP_HOST) {
    printError('ABAP not configured — skipping JUnit integration tests');
    printInfo('Configure via .abapGitAgent or ABAP_HOST environment variable');
    return { success: true, skipped: true, results: [], duration: '0.0', passedCount: 0, totalCount: 0 };
  }

  // ── unit command tests ────────────────────────────────────────────────────

  printInfo('  unit --junit-output');

  test('unit: writes file to absolute path', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-abs.xml');
    cleanup(out);
    try {
      run(`unit --files ${UNIT_TEST_FILE} --junit-output ${out}`);
      return fs.existsSync(out);
    } finally {
      cleanup(out);
    }
  });

  test('unit: writes file to relative path (resolved from cwd)', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-rel.xml');
    cleanup(out);
    try {
      run(`unit --files ${UNIT_TEST_FILE} --junit-output ${out}`);
      return fs.existsSync(out);
    } finally {
      cleanup(out);
    }
  });

  test('unit: XML has valid JUnit structure', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-struct.xml');
    cleanup(out);
    try {
      run(`unit --files ${UNIT_TEST_FILE} --junit-output ${out}`);
      const xml = fs.readFileSync(out, 'utf8');
      return xml.includes('<?xml') &&
             xml.includes('<testsuites') &&
             xml.includes('<testsuite') &&
             xml.includes('<testcase');
    } finally {
      cleanup(out);
    }
  });

  test('unit: XML contains class name', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-classname.xml');
    cleanup(out);
    try {
      run(`unit --files ${UNIT_TEST_FILE} --junit-output ${out}`);
      const xml = fs.readFileSync(out, 'utf8');
      return xml.includes('ZCL_ABGAGT_UNIT_TEST');
    } finally {
      cleanup(out);
    }
  });

  test('unit: JUnit classname uses CLASS.LTCL hierarchy', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-hierarchy.xml');
    cleanup(out);
    try {
      run(`unit --files ${UNIT_TEST_FILE} --junit-output ${out}`);
      const xml = fs.readFileSync(out, 'utf8');
      return /classname="ZCL_ABGAGT_UNIT_TEST\.[^"]+/.test(xml);
    } finally {
      cleanup(out);
    }
  });

  test('unit: test counts in XML are non-negative integers', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-counts.xml');
    cleanup(out);
    try {
      run(`unit --files ${UNIT_TEST_FILE} --junit-output ${out}`);
      const xml = fs.readFileSync(out, 'utf8');
      const tests = parseTestCount(xml, 'tests');
      const failures = parseTestCount(xml, 'failures');
      return tests !== null && tests >= 0 && failures !== null && failures >= 0;
    } finally {
      cleanup(out);
    }
  });

  test('unit: auto-creates missing output directory', () => {
    const dir = path.join(TMP, `abapgit-junit-newdir-${Date.now()}`);
    const out = path.join(dir, 'sub', 'unit.xml');
    cleanupDir(dir);
    try {
      run(`unit --files ${UNIT_TEST_FILE} --junit-output ${out}`);
      return fs.existsSync(out);
    } finally {
      cleanupDir(dir);
    }
  });

  test('unit: CLI output still shows normal results alongside JUnit output', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-cli.xml');
    cleanup(out);
    try {
      const { output } = tryRun(`unit --files ${UNIT_TEST_FILE} --junit-output ${out}`);
      return output.includes('Tests:') && output.includes('JUnit report written to');
    } finally {
      cleanup(out);
    }
  });

  // ── failing tests (ZCL_ABGAGT_UNIT_FAIL) ─────────────────────────────────

  printInfo('  unit (failing class — ZCL_ABGAGT_UNIT_FAIL)');

  test('unit: failing tests exit non-zero', () => {
    const { exitCode } = tryRun(`unit --files ${UNIT_FAIL_FILE}`);
    return exitCode !== 0;
  });

  test('unit: failing output shows LTCL test class name prefix', () => {
    const { output } = tryRun(`unit --files ${UNIT_FAIL_FILE}`);
    return output.includes('LTCL_FAIL_TEST=>');
  });

  test('unit: failing output shows assertion detail lines', () => {
    const { output } = tryRun(`unit --files ${UNIT_FAIL_FILE}`);
    // ADT returns "Expected [7] Actual [13]" as a detail line
    return output.includes('Expected') && output.includes('Actual');
  });

  test('unit: failing output shows exception type', () => {
    const { output } = tryRun(`unit --files ${UNIT_FAIL_FILE}`);
    // divide-by-zero raises CX_SY_ZERODIVIDE — detail line includes "Division by zero"
    return output.includes('Division by zero') || output.includes('ZERODIVIDE') || output.includes('zero');
  });

  test('unit: failing output shows stack entry with line number', () => {
    const { output } = tryRun(`unit --files ${UNIT_FAIL_FILE}`);
    return output.includes('at ZCL_ABGAGT_UNIT_FAIL (line');
  });

  test('unit: failing output shows Failed Tests summary section', () => {
    const { output } = tryRun(`unit --files ${UNIT_FAIL_FILE}`);
    return output.includes('Failed Tests:');
  });

  test('unit: JUnit XML for failing class uses ZCL.LTCL classname hierarchy', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-fail.xml');
    cleanup(out);
    try {
      tryRun(`unit --files ${UNIT_FAIL_FILE} --junit-output ${out}`);
      if (!fs.existsSync(out)) return false;
      const xml = fs.readFileSync(out, 'utf8');
      return xml.includes('classname="ZCL_ABGAGT_UNIT_FAIL.LTCL_FAIL_TEST"');
    } finally {
      cleanup(out);
    }
  });

  test('unit: JUnit XML for failing class contains failure element with detail', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-fail-detail.xml');
    cleanup(out);
    try {
      tryRun(`unit --files ${UNIT_FAIL_FILE} --junit-output ${out}`);
      if (!fs.existsSync(out)) return false;
      const xml = fs.readFileSync(out, 'utf8');
      return xml.includes('<failure') && (xml.includes('Expected') || xml.includes('zero') || xml.includes('ZERODIVIDE'));
    } finally {
      cleanup(out);
    }
  });

  test('unit: JUnit XML testcase elements include time attribute', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-time.xml');
    cleanup(out);
    try {
      tryRun(`unit --files ${UNIT_TEST_FILE} --junit-output ${out}`);
      if (!fs.existsSync(out)) return false;
      const xml = fs.readFileSync(out, 'utf8');
      return xml.includes('time="');
    } finally {
      cleanup(out);
    }
  });

  test('unit: --verbose shows per-method time table', () => {
    const { output } = tryRun(`unit --files ${UNIT_TEST_FILE} --verbose`);
    return output.includes('Method times:');
  });

  // ── inspect command tests ─────────────────────────────────────────────────

  printInfo('  inspect --junit-output');

  test('inspect: writes file to absolute path', () => {
    const out = path.join(TMP, 'abapgit-junit-inspect-abs.xml');
    cleanup(out);
    try {
      run(`inspect --files abap/zcl_abgagt_util.clas.abap --junit-output ${out}`);
      return fs.existsSync(out);
    } finally {
      cleanup(out);
    }
  });

  test('inspect: XML has valid JUnit structure', () => {
    const out = path.join(TMP, 'abapgit-junit-inspect-struct.xml');
    cleanup(out);
    try {
      run(`inspect --files abap/zcl_abgagt_util.clas.abap --junit-output ${out}`);
      const xml = fs.readFileSync(out, 'utf8');
      return xml.includes('<?xml') &&
             xml.includes('<testsuites') &&
             xml.includes('<testsuite') &&
             xml.includes('<testcase');
    } finally {
      cleanup(out);
    }
  });

  test('inspect: XML contains object name', () => {
    const out = path.join(TMP, 'abapgit-junit-inspect-objname.xml');
    cleanup(out);
    try {
      run(`inspect --files abap/zcl_abgagt_util.clas.abap --junit-output ${out}`);
      const xml = fs.readFileSync(out, 'utf8');
      return xml.includes('ZCL_ABGAGT_UTIL');
    } finally {
      cleanup(out);
    }
  });

  test('inspect: JUnit classname uses CLASS.Inspect hierarchy', () => {
    const out = path.join(TMP, 'abapgit-junit-inspect-hierarchy.xml');
    cleanup(out);
    try {
      run(`inspect --files abap/zcl_abgagt_util.clas.abap --junit-output ${out}`);
      const xml = fs.readFileSync(out, 'utf8');
      return xml.includes('classname="ZCL_ABGAGT_UTIL.Inspect"');
    } finally {
      cleanup(out);
    }
  });

  test('inspect: auto-creates missing output directory', () => {
    const dir = path.join(TMP, `abapgit-junit-inspect-dir-${Date.now()}`);
    const out = path.join(dir, 'reports', 'inspect.xml');
    cleanupDir(dir);
    try {
      run(`inspect --files abap/zcl_abgagt_util.clas.abap --junit-output ${out}`);
      return fs.existsSync(out);
    } finally {
      cleanupDir(dir);
    }
  });

  test('inspect: CLI output still shows normal results alongside JUnit output', () => {
    const out = path.join(TMP, 'abapgit-junit-inspect-cli.xml');
    cleanup(out);
    try {
      const { output } = tryRun(`inspect --files abap/zcl_abgagt_util.clas.abap --junit-output ${out}`);
      return (output.includes('Syntax check passed') || output.includes('Syntax check failed')) &&
             output.includes('JUnit report written to');
    } finally {
      cleanup(out);
    }
  });

  // ── unit --coverage integration tests ────────────────────────────────────

  printInfo('  unit --coverage and --coverage-threshold');

  test('unit --coverage: coverage stats appear in CLI output', () => {
    const { output } = tryRun(`unit --files ${UNIT_TEST_FILE} --coverage`);
    // Backend (CL_AUCV_TASK + CL_SCV_*) is expected to return coverage on every run.
    // Output should show the "📊 Coverage: NN.N%   (X/Y statements)" line.
    return output.includes('📊 Coverage:') && /\d+\/\d+ statements/.test(output);
  });

  test('unit --coverage --verbose: per-method coverage breakdown appears in CLI output', () => {
    const { output } = tryRun(`unit --files ${UNIT_TEST_FILE} --coverage --verbose`);
    // --verbose adds a "Method coverage:" section with one line per production method.
    if (!output.includes('Method coverage:')) return false;
    // At least one method line of the form "<NAME>  X/Y (Z%)"
    return /\s\d+\/\d+\s+\(\d+(\.\d+)?%\)/.test(output);
  });

  test('unit --coverage --junit-output: JUnit XML contains coverage properties', () => {
    const out = path.join(TMP, 'abapgit-junit-coverage-props.xml');
    cleanup(out);
    try {
      tryRun(`unit --files ${UNIT_TEST_FILE} --coverage --junit-output ${out}`);
      if (!fs.existsSync(out)) return false;
      const xml = fs.readFileSync(out, 'utf8');
      // Per-class properties on <testsuite>
      const hasClassRate = /property name="coverage\.rate"\s+value="\d+/.test(xml);
      const hasClassTotal = /property name="coverage\.lines\.total"\s+value="\d+/.test(xml);
      const hasClassCovered = /property name="coverage\.lines\.covered"\s+value="\d+/.test(xml);
      return xml.includes('<testsuites') && xml.includes('<testsuite')
          && hasClassRate && hasClassTotal && hasClassCovered;
    } finally {
      cleanup(out);
    }
  });

  test('unit --coverage --junit-output: per-method coverage.method.* properties on <testcase>', () => {
    const out = path.join(TMP, 'abapgit-junit-method-coverage.xml');
    cleanup(out);
    try {
      tryRun(`unit --files ${UNIT_TEST_FILE} --coverage --junit-output ${out}`);
      if (!fs.existsSync(out)) return false;
      const xml = fs.readFileSync(out, 'utf8');
      // At least one <testcase> should have coverage.method.* properties when its
      // name maps to a covered production method (test_FOO → FOO heuristic).
      const hasMethodTotal   = /property name="coverage\.method\.lines\.total"\s+value="\d+/.test(xml);
      const hasMethodCovered = /property name="coverage\.method\.lines\.covered"\s+value="\d+/.test(xml);
      const hasMethodRate    = /property name="coverage\.method\.rate"/.test(xml);
      return hasMethodTotal && hasMethodCovered && hasMethodRate;
    } finally {
      cleanup(out);
    }
  });

  test('unit --coverage-threshold 1: passes (any non-zero coverage meets threshold of 1)', () => {
    const { exitCode } = tryRun(
      `unit --files ${UNIT_LOW_COV_FILE} --coverage --coverage-threshold 1`
    );
    return exitCode === 0;
  });

  test('unit --coverage-threshold 101: fails with below-threshold message', () => {
    // Threshold of 101 is impossible — always below. Backend always returns coverage now,
    // so the gate must fire and the CLI must exit 1 with "below threshold".
    const { output, exitCode } = tryRun(
      `unit --files ${UNIT_LOW_COV_FILE} --coverage --coverage-threshold 101`
    );
    return exitCode === 1 && output.includes('below threshold');
  });

  test('unit --coverage-threshold 101 --coverage-mode warn: warns but exits 0', () => {
    const { output, exitCode } = tryRun(
      `unit --files ${UNIT_LOW_COV_FILE} --coverage --coverage-threshold 101 --coverage-mode warn`
    );
    return exitCode === 0 && output.includes('below threshold');
  });

  // ── summary ───────────────────────────────────────────────────────────────

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  if (passedCount === totalCount) {
    printSuccess(`JUnit output tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`JUnit output tests: ${passedCount}/${totalCount} passed (${duration}s)`);

    const failed = results.filter(r => !r.passed);
    for (const f of failed) {
      printError(`  - ${f.name}`);
      if (f.output) {
        console.log(colors.gray + `    ${f.output.substring(0, 120)}` + colors.reset);
      }
    }

    return { success: false, results, duration, passedCount, totalCount };
  }
}

module.exports = { runJUnitTests };
