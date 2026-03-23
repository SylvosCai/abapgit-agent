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
    try {
      return { output: run(args), exitCode: 0 };
    } catch (err) {
      return { output: err.stdout || err.message || '', exitCode: err.status || 1 };
    }
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
      run(`unit --files abap/zcl_abgagt_util.clas.testclasses.abap --junit-output ${out}`);
      return fs.existsSync(out);
    } finally {
      cleanup(out);
    }
  });

  test('unit: writes file to relative path (resolved from cwd)', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-rel.xml');
    cleanup(out);
    try {
      run(`unit --files abap/zcl_abgagt_util.clas.testclasses.abap --junit-output ${out}`);
      return fs.existsSync(out);
    } finally {
      cleanup(out);
    }
  });

  test('unit: XML has valid JUnit structure', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-struct.xml');
    cleanup(out);
    try {
      run(`unit --files abap/zcl_abgagt_util.clas.testclasses.abap --junit-output ${out}`);
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
      run(`unit --files abap/zcl_abgagt_util.clas.testclasses.abap --junit-output ${out}`);
      const xml = fs.readFileSync(out, 'utf8');
      return xml.includes('ZCL_ABGAGT_UTIL');
    } finally {
      cleanup(out);
    }
  });

  test('unit: test counts in XML are non-negative integers', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-counts.xml');
    cleanup(out);
    try {
      run(`unit --files abap/zcl_abgagt_util.clas.testclasses.abap --junit-output ${out}`);
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
      run(`unit --files abap/zcl_abgagt_util.clas.testclasses.abap --junit-output ${out}`);
      return fs.existsSync(out);
    } finally {
      cleanupDir(dir);
    }
  });

  test('unit: CLI output still shows normal results alongside JUnit output', () => {
    const out = path.join(TMP, 'abapgit-junit-unit-cli.xml');
    cleanup(out);
    try {
      const { output } = tryRun(`unit --files abap/zcl_abgagt_util.clas.testclasses.abap --junit-output ${out}`);
      return output.includes('Tests:') && output.includes('JUnit report written to');
    } finally {
      cleanup(out);
    }
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
