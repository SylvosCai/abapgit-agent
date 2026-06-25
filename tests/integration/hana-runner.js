/**
 * HANA Command Integration Test Runner
 *
 * Tests the `hana` command end-to-end against a real ABAP system + HANA tenant.
 *
 * Test Repository: abgagt-hta-test (URL configured via testRepos.hana in .abapGitAgent)
 *   Branch:  main
 *   Fixture: hana/tmp/abgagt/ABGAGT_IT_TEST.calculationview
 *            Script-based calc view: SELECT 'integration test', 1 FROM DUMMY.
 *            No user-schema or user-table dependency; DUMMY is built into every HANA system.
 *
 * Test sequence (idempotent — always begins with a baseline reset):
 *   0. Reset baseline: drop ABGAGT_IT_TEST if it exists from a previous run.
 *   1. hana schema list   --filter '%SYS%'              → non-empty rows
 *   2. hana schema tables --schema SYS --filter 'DUMMY%' → DUMMY present
 *   3. hana schema fields --schema SYS --table DUMMY     → at least one column
 *   4. hana view deploy   --file <fixture> --force       → "deployed"
 *   5. hana view list     --package tmp.abgagt           → ABGAGT_IT_TEST present, A
 *   6. hana view drop     --file <fixture> --dry-run     → "would drop", no state change
 *   7. hana view list                                    → still present (dry-run)
 *   8. hana view drop     --file <fixture>               → "dropped"
 *   9. hana view list                                    → ABGAGT_IT_TEST absent
 *  10. hana view drop     --file <fixture>               → "already absent" (idempotency)
 *  11. hana view deploy   --file <fixture> --force       → recovery from accidental drop
 *  12. Final cleanup: drop the test view; system ends as it started
 *
 * Run:
 *   node tests/run-all.js --hana
 *   node tests/integration/hana-runner.js                (direct)
 */

'use strict';

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

const { getTestRepoUrl } = require('./test-repos');

const HANA_REPO_URL  = getTestRepoUrl('hana');
const HANA_REPO_DIR  = path.join(os.tmpdir(), 'abgagt-hana-test');
const HANA_PACKAGE   = 'tmp.abgagt';
const TEST_VIEW_NAME = 'ABGAGT_IT_TEST';
const TEST_VIEW_FILE = `hana/tmp/abgagt/${TEST_VIEW_NAME}.calculationview`;

/**
 * Run an abapgit-agent command from inside the cloned test repo (so it picks
 * up the test repo's injected .abapGitAgent + repo-relative paths).
 * Returns { output, exitCode }.
 */
function runAgent(agentBin, args = [], { timeout = 120000 } = {}) {
  // Shell-quote args containing whitespace so multi-word values like
  // --where "DUMMY_MEASURE = 1" survive the join.
  const quoted = args.map(a => /\s/.test(a) ? `'${a.replace(/'/g, "'\\''")}'` : a);
  const cmd = ['node', agentBin, ...quoted].join(' ');
  try {
    const output = execSync(cmd, {
      cwd: HANA_REPO_DIR,
      encoding: 'utf8',
      stdio: ['pipe', 'pipe', 'pipe'],
      timeout
    });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

/**
 * Drop the test view, treating "already absent" as success.
 * Used for both the initial reset and the final cleanup so the test is repeatable.
 */
function silentDrop(agentBin) {
  return runAgent(agentBin, ['hana', 'view', 'drop', '--file', TEST_VIEW_FILE], { timeout: 120000 });
}

/**
 * Run HANA integration tests.
 * @param {string} repoRoot - abapgit-agent repository root
 * @param {Object} printFunctions - { printSubHeader, printInfo, printSuccess, printError, colorize, colors }
 */
function runHanaTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors }) {
  printSubHeader('Running HANA Command Integration Tests');

  const agentBin = path.join(repoRoot, 'bin', 'abapgit-agent');
  const startTime = Date.now();
  const results = [];

  const addResult = (name, passed, output = '') => {
    results.push({ name, passed, output: output.substring(0, 300) });
    if (passed) {
      console.log(colorize('green', '  ✅ ') + name);
    } else {
      console.log(colorize('red', '  ❌ ') + name);
      if (output) {
        console.log(colors.gray + '     ' + output.substring(0, 300).replace(/\n/g, '\n     ') + colors.reset);
      }
    }
  };

  // ─── Clone or refresh the hana-test repo ───────────────────────────────────
  const isValidRepo = fs.existsSync(HANA_REPO_DIR) && (() => {
    try {
      execSync('git rev-parse --is-inside-work-tree', { cwd: HANA_REPO_DIR, encoding: 'utf8', stdio: 'pipe' });
      return true;
    } catch { return false; }
  })();
  if (!isValidRepo) {
    if (fs.existsSync(HANA_REPO_DIR)) {
      printInfo(`  HANA-test repo at ${HANA_REPO_DIR} is not a valid git repo — removing and re-cloning...`);
      fs.rmSync(HANA_REPO_DIR, { recursive: true, force: true });
    } else {
      printInfo(`  HANA-test repo not found at ${HANA_REPO_DIR} — cloning...`);
    }
    try {
      execSync(`git clone ${HANA_REPO_URL} ${HANA_REPO_DIR}`, { encoding: 'utf8', timeout: 60000 });
      printInfo('  Cloned successfully.');
    } catch (e) {
      printError(`  Failed to clone HANA-test repo: ${e.message}`);
      return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
    }
  } else {
    try {
      execSync('git pull --ff-only', { cwd: HANA_REPO_DIR, encoding: 'utf8', timeout: 30000 });
    } catch (e) {
      printInfo(`  Warning: git pull failed (${e.message.split('\n')[0]}) — continuing with existing clone`);
    }
  }

  // ─── Inject .abapGitAgent into the clone (credentials + package) ───────────
  const srcConfig = path.join(repoRoot, '.abapGitAgent');
  const dstConfig = path.join(HANA_REPO_DIR, '.abapGitAgent');
  if (fs.existsSync(srcConfig)) {
    const config = JSON.parse(fs.readFileSync(srcConfig, 'utf8'));
    config.package = '$ABGAGT_HTA_TEST';
    config.folder  = '/src/';
    if (process.env.TEST_GIT_USR) config.gitUsername = process.env.TEST_GIT_USR;
    if (process.env.TEST_GIT_PSW) config.gitPassword = process.env.TEST_GIT_PSW;
    fs.writeFileSync(dstConfig, JSON.stringify(config, null, 2));
  } else {
    printError(`  Source .abapGitAgent not found at ${srcConfig}`);
    return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
  }

  // Verify the fixture file exists in the cloned repo
  const fixturePath = path.join(HANA_REPO_DIR, TEST_VIEW_FILE);
  if (!fs.existsSync(fixturePath)) {
    printError(`  Fixture not found in cloned repo: ${TEST_VIEW_FILE}`);
    printError(`  Add it to the test repo at hana/tmp/abgagt/${TEST_VIEW_NAME}.calculationview`);
    return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
  }

  // ─── Step 0: Reset baseline ────────────────────────────────────────────────
  printInfo('  Resetting baseline (dropping leftover ABGAGT_IT_TEST if present)...');
  silentDrop(agentBin);  // intentionally not asserting — just clearing state

  // ─── Step 1-3: schema commands ─────────────────────────────────────────────
  {
    const r = runAgent(agentBin, ['hana', 'schema', 'list', '--filter', '%SYS%'], { timeout: 30000 });
    addResult('hana schema list --filter %SYS%', r.exitCode === 0 && /schema\(s\) found/.test(r.output), r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'schema', 'tables', '--schema', 'SYS', '--filter', 'DUMMY%'], { timeout: 30000 });
    addResult('hana schema tables --schema SYS --filter DUMMY%', r.exitCode === 0 && /DUMMY/.test(r.output), r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'schema', 'fields', '--schema', 'SYS', '--table', 'DUMMY'], { timeout: 30000 });
    addResult('hana schema fields --schema SYS --table DUMMY', r.exitCode === 0 && /column\(s\) found/.test(r.output), r.output);
  }

  // ─── Step 3a-3d: hana table preview against SYS.DUMMY (universal) ──────────
  // SYS.DUMMY is built into every HANA system and freely queryable. Uses both
  // success and error paths to verify the command's surface end-to-end.
  {
    const r = runAgent(agentBin, ['hana', 'table', 'preview', '--schema', 'SYS', '--table', 'DUMMY'], { timeout: 60000 });
    addResult('hana table preview --schema SYS --table DUMMY returns one row',
      r.exitCode === 0 && /1 row\(s\) returned/.test(r.output) && /\bDUMMY\b/.test(r.output),
      r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'table', 'preview', '--schema', 'SYS', '--table', 'DUMMY', '--columns', 'DUMMY'], { timeout: 60000 });
    addResult('hana table preview --columns DUMMY projects single column',
      r.exitCode === 0 && /1 row\(s\) returned/.test(r.output),
      r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'table', 'preview', '--schema', 'SYS', '--table', 'DUMMY', '--where', '1=0'], { timeout: 60000 });
    addResult('hana table preview --where "1=0" returns no rows',
      r.exitCode === 0 && /No rows returned/.test(r.output),
      r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'table', 'preview', '--schema', 'SYS', '--table', 'DOES_NOT_EXIST_XYZ'], { timeout: 60000 });
    addResult('hana table preview surfaces table_not_found',
      r.exitCode === 1 && /table_not_found|not found in schema/.test(r.output),
      r.output);
  }
  {
    // schema_not_mapped — pick a logical name that no system would ever map.
    const r = runAgent(agentBin, ['hana', 'table', 'preview', '--logical-schema', 'ABGAGT_IT_NONEXISTENT_LOGICAL_SCHEMA', '--table', 'FOO'], { timeout: 60000 });
    addResult('hana table preview surfaces schema_not_mapped for unknown logical schema',
      r.exitCode === 1 && /schema_not_mapped|not mapped in DB_SCHEMA_MAP/.test(r.output),
      r.output);
  }

  // ─── Step 4: deploy the test view ──────────────────────────────────────────
  {
    const r = runAgent(agentBin, ['hana', 'view', 'deploy', '--file', TEST_VIEW_FILE, '--force'], { timeout: 180000 });
    addResult('hana view deploy --force (initial)', r.exitCode === 0 && /Deployed 1 object/.test(r.output), r.output);
  }

  // ─── Step 5: list shows the deployed view ──────────────────────────────────
  {
    const r = runAgent(agentBin, ['hana', 'view', 'list', '--package', HANA_PACKAGE], { timeout: 60000 });
    const found = new RegExp(`${TEST_VIEW_NAME}\\b.*deployed`).test(r.output);
    addResult('hana view list shows deployed view', r.exitCode === 0 && found, r.output);
  }

  // ─── Step 5a-5e: preview the deployed view ─────────────────────────────────
  // The fixture's calc view returns one row: ('integration test', 1).
  {
    const r = runAgent(agentBin, ['hana', 'view', 'preview', '--file', TEST_VIEW_FILE], { timeout: 60000 });
    addResult('hana view preview default returns the fixture row',
      r.exitCode === 0 && /integration test/.test(r.output) && /1 row\(s\) returned/.test(r.output),
      r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'view', 'preview', '--file', TEST_VIEW_FILE, '--columns', 'MESSAGE'], { timeout: 60000 });
    addResult('hana view preview --columns projects requested column',
      r.exitCode === 0 && /integration test/.test(r.output) && !/DUMMY_MEASURE/.test(r.output),
      r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'view', 'preview', '--file', TEST_VIEW_FILE, '--where', 'DUMMY_MEASURE = 1'], { timeout: 60000 });
    addResult('hana view preview --where matches fixture row',
      r.exitCode === 0 && /1 row\(s\) returned/.test(r.output),
      r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'view', 'preview', '--file', TEST_VIEW_FILE, '--where', '1=0'], { timeout: 60000 });
    addResult('hana view preview --where "1=0" returns no rows',
      r.exitCode === 0 && /No rows returned/.test(r.output),
      r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'view', 'preview', '--file', TEST_VIEW_FILE, '--json'], { timeout: 60000 });
    let ok = false;
    try {
      const jsonLine = r.output.split('\n').find(l => l.trim().startsWith('{') || l.trim().startsWith('['));
      const m = r.output.match(/\{[\s\S]*\}\s*$/);
      const parsed = m ? JSON.parse(m[0]) : null;
      ok = parsed && parsed.success === true && parsed.row_count === 1;
    } catch { ok = false; }
    addResult('hana view preview --json emits valid JSON', r.exitCode === 0 && ok, r.output);
  }

  // ─── Step 6-7: dry-run drop does not change state ──────────────────────────
  {
    const r = runAgent(agentBin, ['hana', 'view', 'drop', '--file', TEST_VIEW_FILE, '--dry-run'], { timeout: 60000 });
    addResult('hana view drop --dry-run reports would-drop', r.exitCode === 0 && /would be dropped|would drop/.test(r.output), r.output);
  }
  {
    const r = runAgent(agentBin, ['hana', 'view', 'list', '--package', HANA_PACKAGE], { timeout: 60000 });
    const stillThere = new RegExp(`${TEST_VIEW_NAME}\\b.*deployed`).test(r.output);
    addResult('list after --dry-run shows view still present', r.exitCode === 0 && stillThere, r.output);
  }

  // ─── Step 8: real drop ─────────────────────────────────────────────────────
  {
    const r = runAgent(agentBin, ['hana', 'view', 'drop', '--file', TEST_VIEW_FILE], { timeout: 120000 });
    addResult('hana view drop succeeds', r.exitCode === 0 && /Dropped 1 object/.test(r.output), r.output);
  }

  // ─── Step 9: list confirms the view is gone ────────────────────────────────
  {
    const r = runAgent(agentBin, ['hana', 'view', 'list', '--package', HANA_PACKAGE], { timeout: 60000 });
    const gone = !new RegExp(`${TEST_VIEW_NAME}\\b`).test(r.output);
    addResult('list after drop shows view absent', r.exitCode === 0 && gone, r.output);
  }

  // ─── Step 10: drop is idempotent ───────────────────────────────────────────
  {
    const r = runAgent(agentBin, ['hana', 'view', 'drop', '--file', TEST_VIEW_FILE], { timeout: 120000 });
    addResult('hana view drop is idempotent (already absent)', r.exitCode === 0 && /already absent/.test(r.output), r.output);
  }

  // ─── Step 10a: preview after drop returns view_not_active ──────────────────
  {
    const r = runAgent(agentBin, ['hana', 'view', 'preview', '--file', TEST_VIEW_FILE], { timeout: 60000 });
    addResult('hana view preview fails with view_not_active when view absent',
      r.exitCode === 1 && /view_not_active|not active in HANA/.test(r.output),
      r.output);
  }

  // ─── Step 11: recovery — redeploy works ────────────────────────────────────
  {
    const r = runAgent(agentBin, ['hana', 'view', 'deploy', '--file', TEST_VIEW_FILE, '--force'], { timeout: 180000 });
    addResult('hana view deploy --force recovers from accidental drop', r.exitCode === 0 && /Deployed 1 object/.test(r.output), r.output);
  }

  // ─── Step 12: final cleanup ────────────────────────────────────────────────
  {
    const r = silentDrop(agentBin);
    addResult('final cleanup drop succeeds', r.exitCode === 0, r.output);
  }

  // ─── Summary ───────────────────────────────────────────────────────────────
  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  if (passedCount === totalCount) {
    printSuccess(`HANA tests: ${passedCount}/${totalCount} PASSED (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`HANA tests: ${passedCount}/${totalCount} PASSED (${duration}s)`);
    return { success: false, results, duration, passedCount, totalCount };
  }
}

module.exports = { runHanaTests };

// Allow direct invocation: node tests/integration/hana-runner.js
if (require.main === module) {
  const repoRoot = path.resolve(__dirname, '..', '..');
  const colors = {
    green: '\x1b[32m', red: '\x1b[31m', yellow: '\x1b[33m', cyan: '\x1b[36m',
    gray: '\x1b[90m', reset: '\x1b[0m', bold: '\x1b[1m'
  };
  const colorize = (color, text) => colors[color] + text + colors.reset;
  const printSubHeader = (s) => console.log(`\n${colorize('bold', s)}\n${'─'.repeat(s.length)}`);
  const printInfo      = (s) => console.log(colorize('cyan', s));
  const printSuccess   = (s) => console.log(colorize('green', '✅ ' + s));
  const printError     = (s) => console.error(colorize('red', '❌ ' + s));

  const result = runHanaTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors });
  process.exit(result.success ? 0 : 1);
}
