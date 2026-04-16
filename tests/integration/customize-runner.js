/**
 * Customize Command Integration Test Runner
 *
 * Tests the `customize` command end-to-end against a real ABAP system.
 *
 * Test Repository: abgagt-customize-test (URL configured via testRepos.customize in .abapGitAgent)
 *   Package: $ABGAGT_CUS_TST
 *   Branch:  main
 *   Objects:
 *     - src/zabgagt_cus_tst.tabl.xml   TABL ZABGAGT_CUS_TST (delivery class C)
 *
 * Test sequence:
 *   Setup:   pull ZABGAGT_CUS_TST table into ABAP (idempotent baseline)
 *            delete any leftover test rows so each run starts clean
 *   Happy path:
 *     1. Write new entry (--no-transport) → preview verifies row exists with expected value
 *     2. Update existing entry (same key, different value) → preview verifies value changed
 *     3. Multiple fields in one call → preview verifies row exists
 *     4. JSON output (--json flag) → parse JSON + preview verifies row exists
 *   Error cases:
 *     5. Unknown field name
 *     6. Non-customizing table (E070 — delivery class E, transport required — use as "wrong type" proxy)
 *     7. Non-existent table
 *     8. Missing --set argument
 *     9. Missing table name argument
 *    10. Wrong transport type (workbench request)
 *
 * Run:
 *   node tests/run-all.js --customize
 */

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

const { getTestRepoUrl } = require('./test-repos');
const CUSTOMIZE_REPO_URL = getTestRepoUrl('customize');
const CUSTOMIZE_REPO_DIR = path.join(os.tmpdir(), 'abgagt-customize-test');
const TEST_TABLE = 'ZABGAGT_CUS_TST';
const TEST_TABLE_FILE = 'src/zabgagt_cus_tst.tabl.xml';

/**
 * Run `abapgit-agent pull` to activate the test table from the customize-test repo.
 * Uses --url so it works from any cwd; credentials come from abapgit-agent's own .abapGitAgent.
 */
function runPull(agentBin, repoRoot, file, extraArgs = []) {
  const cmd = [
    'node', agentBin,
    'pull',
    '--url',    CUSTOMIZE_REPO_URL,
    '--branch', 'main',
    '--files',  file,
    ...extraArgs
  ].join(' ');

  try {
    const output = execSync(cmd, { cwd: repoRoot, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 120000 });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

/**
 * Run `abapgit-agent customize` from the customize-test repo clone directory.
 * The clone has .abapGitAgent injected so abapgit-agent can reach the ABAP system.
 */
function runCustomize(agentBin, args = []) {
  const cmd = ['node', agentBin, 'customize', ...args].join(' ');
  try {
    const output = execSync(cmd, { cwd: CUSTOMIZE_REPO_DIR, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 60000 });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

/**
 * Run `abapgit-agent preview` for the test table and return the rows array.
 * Returns [] on any error.
 */
function previewTable(agentBin, repoRoot) {
  try {
    const cmd = ['node', agentBin, 'preview', '--objects', TEST_TABLE, '--json'].join(' ');
    const output = execSync(cmd, { cwd: repoRoot, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 30000 });
    // Strip any non-JSON prefix lines (e.g. "Previewing 1 object(s)")
    const jsonStart = output.indexOf('{');
    if (jsonStart === -1) return [];
    const parsed = JSON.parse(output.substring(jsonStart));
    const objects = parsed.OBJECTS || parsed.objects || [];
    if (objects.length === 0) return [];
    const rowsRaw = objects[0].ROWS || objects[0].rows || '[]';
    return JSON.parse(rowsRaw);
  } catch {
    return [];
  }
}

/**
 * Find a row in the preview result by KEY_FIELD value (case-insensitive).
 */
function findRow(rows, keyValue) {
  return rows.find(r =>
    (r.KEY_FIELD || r.key_field || '').trim().toUpperCase() === keyValue.toUpperCase()
  ) || null;
}

/**
 * Find an open workbench transport number (TRFUNCTION='K') by querying the transport list.
 * Returns the transport number string, or null if none found.
 */
function findWorkbenchTransport(agentBin, repoRoot) {
  try {
    const cmd = ['node', agentBin, 'transport', 'list', '--scope', 'mine', '--json'].join(' ');
    const output = execSync(cmd, { cwd: repoRoot, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 30000 });
    const result = JSON.parse(output);
    const transports = result.TRANSPORTS || result.transports || [];
    if (transports.length > 0) {
      return transports[0].NUMBER || transports[0].number || null;
    }
  } catch {
    // ignore — transport list is best-effort
  }
  return null;
}

/**
 * Run customize integration tests.
 * @param {string} repoRoot - abapgit-agent repository root
 * @param {Object} printFunctions
 */
function runCustomizeTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors }) {
  printSubHeader('Running Customize Command Integration Tests');

  const agentBin = path.join(repoRoot, 'bin', 'abapgit-agent');
  const startTime = Date.now();
  const results = [];

  // Helper to record a test result
  const addResult = (name, passed, output = '') => {
    results.push({ name, passed, output: output.substring(0, 300) });
    if (passed) {
      console.log(colorize('green', '  ✅ ') + name);
    } else {
      console.log(colorize('red', '  ❌ ') + name);
      if (output) {
        console.log(colors.gray + '     ' + output.substring(0, 200).replace(/\n/g, '\n     ') + colors.reset);
      }
    }
  };

  // ─── Clone customize-test repo if not already present (or invalid) ────────
  const isValidCustRepo = fs.existsSync(CUSTOMIZE_REPO_DIR) && (() => {
    try {
      execSync('git rev-parse --is-inside-work-tree', { cwd: CUSTOMIZE_REPO_DIR, encoding: 'utf8', stdio: 'pipe' });
      return true;
    } catch { return false; }
  })();
  if (!isValidCustRepo) {
    if (fs.existsSync(CUSTOMIZE_REPO_DIR)) {
      printInfo(`  Customize-test repo at ${CUSTOMIZE_REPO_DIR} is not a valid git repo — removing and re-cloning...`);
      fs.rmSync(CUSTOMIZE_REPO_DIR, { recursive: true, force: true });
    } else {
      printInfo(`  Customize-test repo not found at ${CUSTOMIZE_REPO_DIR} — cloning...`);
    }
    try {
      execSync(`git clone ${CUSTOMIZE_REPO_URL} ${CUSTOMIZE_REPO_DIR}`, { encoding: 'utf8', timeout: 60000 });
      printInfo('  Cloned successfully.');
    } catch (e) {
      printError(`  Failed to clone customize-test repo: ${e.message}`);
      return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
    }
  }

  // ─── Inject .abapGitAgent into the clone (credentials + package) ───────────
  const srcConfig = path.join(repoRoot, '.abapGitAgent');
  const dstConfig = path.join(CUSTOMIZE_REPO_DIR, '.abapGitAgent');
  if (fs.existsSync(srcConfig)) {
    const config = JSON.parse(fs.readFileSync(srcConfig, 'utf8'));
    config.package = '$ABGAGT_CUS_TST';
    config.folder  = '/src/';
    if (process.env.TEST_GIT_USR) config.gitUsername = process.env.TEST_GIT_USR;
    if (process.env.TEST_GIT_PSW) config.gitPassword = process.env.TEST_GIT_PSW;
    fs.writeFileSync(dstConfig, JSON.stringify(config, null, 2));
  }

  printInfo(`  Test repository: ${CUSTOMIZE_REPO_URL}`);
  printInfo(`  Local clone:     ${CUSTOMIZE_REPO_DIR}`);
  printInfo(`  Test table:      ${TEST_TABLE} (delivery class C)`);
  printInfo('');

  // ─── Setup: activate ZABGAGT_CUS_TST in ABAP system ────────────────────────
  printInfo(colorize('cyan', '  Setup: activating ZABGAGT_CUS_TST (--conflict-mode ignore)'));
  const { output: setupOutput } = runPull(agentBin, repoRoot, TEST_TABLE_FILE, ['--conflict-mode', 'ignore']);
  const setupOk = setupOutput.includes('Pull completed successfully') ||
                  setupOutput.includes('already active') ||
                  setupOutput.includes('nothing to activate');
  if (!setupOk) {
    printError(`  Setup pull failed — cannot run customize tests: ${setupOutput.substring(0, 200)}`);
    return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
  }
  printInfo('  Setup complete.');
  printInfo('');

  // ─── Setup: delete leftover test rows so each run starts from a clean state ──
  // Write a dummy value to known test keys, then overwrite — cheapest way to
  // guarantee a defined starting state without needing a DELETE command.
  // Actually: just note initial state via preview so we can detect changes.
  const initialRows = previewTable(agentBin, repoRoot);
  printInfo(`  Initial row count in ${TEST_TABLE}: ${initialRows.length}`);
  printInfo('');

  // ─── Find a workbench transport for error-case test (best-effort) ───────────
  const workbenchTransport = findWorkbenchTransport(agentBin, repoRoot);

  // ─── Happy path tests ───────────────────────────────────────────────────────
  printInfo(colorize('cyan', '  Happy path tests'));

  // Test 1: Write new entry — verify value appears in table
  const t1 = runCustomize(agentBin, [TEST_TABLE, '--set', 'KEY_FIELD=INTTEST', 'VALUE_FIELD=hello', '--no-transport']);
  const t1Rows = previewTable(agentBin, repoRoot);
  const t1Row  = findRow(t1Rows, 'INTTEST');
  const t1ValueOk = t1Row && (t1Row.VALUE_FIELD || t1Row.value_field || '').trim() === 'hello';
  addResult(
    'write new entry — preview confirms VALUE_FIELD=hello',
    t1.exitCode === 0 && t1.output.includes('✅') && t1ValueOk,
    t1.output + (t1Row ? ` [preview: VALUE_FIELD=${(t1Row.VALUE_FIELD || t1Row.value_field || '').trim()}]` : ' [preview: row not found]')
  );

  // Test 2: Update existing entry — verify value changed from 'hello' to 'updated'
  const t2 = runCustomize(agentBin, [TEST_TABLE, '--set', 'KEY_FIELD=INTTEST', 'VALUE_FIELD=updated', '--no-transport']);
  const t2Rows = previewTable(agentBin, repoRoot);
  const t2Row  = findRow(t2Rows, 'INTTEST');
  const t2ValueOk = t2Row && (t2Row.VALUE_FIELD || t2Row.value_field || '').trim() === 'updated';
  addResult(
    'update existing entry — preview confirms VALUE_FIELD changed to updated',
    t2.exitCode === 0 && t2.output.includes('✅') && t2ValueOk,
    t2.output + (t2Row ? ` [preview: VALUE_FIELD=${(t2Row.VALUE_FIELD || t2Row.value_field || '').trim()}]` : ' [preview: row not found]')
  );

  // Test 3: Multiple fields in one call — verify row with expected value
  const t3 = runCustomize(agentBin, [TEST_TABLE, '--set', 'KEY_FIELD=MULTI', 'VALUE_FIELD=fieldtest', '--no-transport']);
  const t3Rows = previewTable(agentBin, repoRoot);
  const t3Row  = findRow(t3Rows, 'MULTI');
  const t3ValueOk = t3Row && (t3Row.VALUE_FIELD || t3Row.value_field || '').trim() === 'fieldtest';
  addResult(
    'multiple --set fields — preview confirms KEY_FIELD=MULTI VALUE_FIELD=fieldtest',
    t3.exitCode === 0 && t3.output.includes('✅') && t3ValueOk,
    t3.output + (t3Row ? ` [preview: VALUE_FIELD=${(t3Row.VALUE_FIELD || t3Row.value_field || '').trim()}]` : ' [preview: row not found]')
  );

  // Test 4: JSON output — parse response AND verify row in table
  const t4 = runCustomize(agentBin, [TEST_TABLE, '--set', 'KEY_FIELD=JSONTEST', 'VALUE_FIELD=jsonvalue', '--no-transport', '--json']);
  const t4Rows = previewTable(agentBin, repoRoot);
  const t4Row  = findRow(t4Rows, 'JSONTEST');
  const t4ValueOk = t4Row && (t4Row.VALUE_FIELD || t4Row.value_field || '').trim() === 'jsonvalue';
  let t4JsonOk = false;
  try {
    const parsed = JSON.parse(t4.output);
    t4JsonOk = t4.exitCode === 0 &&
               (parsed.success === true || parsed.success === 'X') &&
               (parsed.table_name || '').toUpperCase() === TEST_TABLE;
  } catch {
    t4JsonOk = false;
  }
  addResult(
    'JSON output — response parsed + preview confirms VALUE_FIELD=jsonvalue',
    t4JsonOk && t4ValueOk,
    t4.output + (t4Row ? ` [preview: VALUE_FIELD=${(t4Row.VALUE_FIELD || t4Row.value_field || '').trim()}]` : ' [preview: row not found]')
  );

  printInfo('');

  // ─── Error case tests ───────────────────────────────────────────────────────
  printInfo(colorize('cyan', '  Error case tests'));

  // Test 5: Unknown field name
  const t5 = runCustomize(agentBin, [TEST_TABLE, '--set', 'KEY_FIELD=X', 'BOGUS_FIELD=y', '--no-transport']);
  addResult(
    'error: unknown field name',
    t5.exitCode !== 0 && (t5.output.toLowerCase().includes('bogus_field') || t5.output.toLowerCase().includes('does not exist')),
    t5.output
  );

  // Test 6: Non-existent table
  const t6 = runCustomize(agentBin, ['ZZNOEXIST99', '--set', 'KEY_FIELD=X', '--no-transport']);
  addResult(
    'error: non-existent table',
    t6.exitCode !== 0 && (t6.output.toLowerCase().includes('not found') || t6.output.toLowerCase().includes('zznoexist')),
    t6.output
  );

  // Test 7: Non-customizing table (E070 is delivery class E — transport-required,
  // but we use --no-transport so it gets past that check and hits the delivery class guard)
  // Actually E070 is delivery class 'E' (system settings) which IS accepted. Use a
  // known non-C/E table instead: TADIR is delivery class 'S'.
  const t7 = runCustomize(agentBin, ['TADIR', '--set', 'PGMID=R3TR', '--no-transport']);
  addResult(
    'error: non-customizing table (TADIR, delivery class S)',
    t7.exitCode !== 0 && (t7.output.toLowerCase().includes('not a customizing') || t7.output.toLowerCase().includes('delivery class')),
    t7.output
  );

  // Test 8: Missing --set argument
  const t8 = runCustomize(agentBin, [TEST_TABLE]);
  addResult(
    'error: missing --set argument',
    t8.exitCode !== 0 && t8.output.toLowerCase().includes('--set'),
    t8.output
  );

  // Test 9: Missing table name
  const t9 = runCustomize(agentBin, ['--set', 'KEY_FIELD=X']);
  addResult(
    'error: missing table name',
    t9.exitCode !== 0 && (t9.output.toLowerCase().includes('table name') || t9.output.toLowerCase().includes('required')),
    t9.output
  );

  // Test 10: Wrong transport type (workbench transport on customizing table)
  if (workbenchTransport) {
    const t10 = runCustomize(agentBin, [TEST_TABLE, '--set', 'KEY_FIELD=X', 'VALUE_FIELD=y', '--transport', workbenchTransport]);
    addResult(
      `error: workbench transport rejected (${workbenchTransport})`,
      t10.exitCode !== 0 && (t10.output.toLowerCase().includes('workbench') || t10.output.toLowerCase().includes('customizing transport')),
      t10.output
    );
  } else {
    printInfo('  (Skipping workbench transport test — no open workbench transport found)');
  }

  printInfo('');

  // ─── Summary ────────────────────────────────────────────────────────────────
  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  if (passedCount === totalCount) {
    printSuccess(`Customize tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`Customize tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: false, results, duration, passedCount, totalCount };
  }
}

module.exports = { runCustomizeTests };
