/**
 * DTEL SCRLEN Failure Integration Test Runner
 *
 * Tests that `abapgit-agent pull` correctly surfaces a DTEL activation failure
 * caused by SCRTEXT exceeding SCRLEN — instead of reporting a misleading false
 * success (the pre-fix behavior).
 *
 * Test Repository: abgagt-pull-test (URL configured via testRepos.pull)
 *   Branch:  test/dtel-scrlen   ← contains one commit adding the bad-DTEL fixture
 *   Fixture: src/zabgagt_xo_dtel_long.dtel.xml
 *            SCRLEN1/2/3 = 5/10/15, SCRTEXT_S/M/L = 9/18/33 chars
 *
 * Test sequence:
 *   1. Pre-flight (Change #4): pull with --files. Pre-flight catches all three
 *      SCRLEN violations locally. Asserts exit 1, no HTTP call, every violation
 *      enumerated in stderr.
 *   2. Server-side (Changes #1+#2+#3): pull with --url to bypass pre-flight.
 *      Server's analyze_silent_cancellation enumerates the violations into
 *      LOG_MESSAGES, the CLI classifies as failure (exit 1, ❌), and the full
 *      un-truncated message is printed below the log table.
 *   3. Server-side JSON: same as step 2 but with --json. Asserts the structured
 *      shape — SUCCESS='', MESSAGE matches /see log_messages/, LOG_MESSAGES has
 *      exactly one E-type DTEL entry mentioning all three SCRTEXT_* fields.
 *
 * Run:
 *   node tests/run-all.js --dtel-scrlen
 *   node tests/integration/dtel-scrlen-runner.js                (direct)
 */

'use strict';

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

const { getTestRepoUrl } = require('./test-repos');

const PULL_REPO_URL = getTestRepoUrl('pull');
const PULL_REPO_DIR = path.join(os.tmpdir(), 'abgagt-dtel-scrlen-test');
const TEST_BRANCH   = 'test/dtel-scrlen';
const FIXTURE_FILE  = 'src/zabgagt_xo_dtel_long.dtel.xml';
const DTEL_NAME     = 'ZABGAGT_XO_DTEL_LONG';

function runAgent(agentBin, args = [], { cwd, timeout = 120000 } = {}) {
  const quoted = args.map(a => /\s/.test(a) ? `'${a.replace(/'/g, "'\\''")}'` : a);
  const cmd = ['node', agentBin, ...quoted].join(' ');
  try {
    const output = execSync(cmd, { cwd, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

/**
 * Run DTEL SCRLEN failure-detection integration tests.
 * @param {string} repoRoot - abapgit-agent repository root
 * @param {Object} printFunctions
 */
function runDtelScrlenTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors }) {
  printSubHeader('Running DTEL SCRLEN Failure-Detection Tests');

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
        console.log(colors.gray + '     ' + output.substring(0, 500).replace(/\n/g, '\n     ') + colors.reset);
      }
    }
  };

  // ─── Clone or refresh the pull-test repo ───────────────────────────────────
  const isValidRepo = fs.existsSync(PULL_REPO_DIR) && (() => {
    try {
      execSync('git rev-parse --is-inside-work-tree', { cwd: PULL_REPO_DIR, encoding: 'utf8', stdio: 'pipe' });
      return true;
    } catch { return false; }
  })();
  if (!isValidRepo) {
    if (fs.existsSync(PULL_REPO_DIR)) fs.rmSync(PULL_REPO_DIR, { recursive: true, force: true });
    printInfo(`  Cloning pull-test repo to ${PULL_REPO_DIR}...`);
    try {
      execSync(`git clone ${PULL_REPO_URL} ${PULL_REPO_DIR}`, { encoding: 'utf8', timeout: 60000 });
    } catch (e) {
      printError(`  Failed to clone pull-test repo: ${e.message}`);
      return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
    }
  }

  // Always make sure the clone is on TEST_BRANCH with the latest fixture.
  try {
    execSync('git fetch origin --prune', { cwd: PULL_REPO_DIR, encoding: 'utf8', timeout: 30000, stdio: 'pipe' });
    execSync(`git checkout ${TEST_BRANCH}`, { cwd: PULL_REPO_DIR, encoding: 'utf8', stdio: 'pipe' });
    execSync(`git reset --hard origin/${TEST_BRANCH}`, { cwd: PULL_REPO_DIR, encoding: 'utf8', stdio: 'pipe' });
  } catch (e) {
    printError(`  Failed to sync ${TEST_BRANCH}: ${e.message.split('\n')[0]}`);
    return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
  }

  // ─── Inject .abapGitAgent into the clone (credentials + package) ───────────
  const srcConfig = path.join(repoRoot, '.abapGitAgent');
  const dstConfig = path.join(PULL_REPO_DIR, '.abapGitAgent');
  if (!fs.existsSync(srcConfig)) {
    printError(`  Source .abapGitAgent not found at ${srcConfig}`);
    return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
  }
  const config = JSON.parse(fs.readFileSync(srcConfig, 'utf8'));
  config.package = '$ABGABT_PULL_TEST';
  config.folder  = '/src/';
  if (process.env.TEST_GIT_USR) config.gitUsername = process.env.TEST_GIT_USR;
  if (process.env.TEST_GIT_PSW) config.gitPassword = process.env.TEST_GIT_PSW;
  fs.writeFileSync(dstConfig, JSON.stringify(config, null, 2));

  // Verify the fixture exists where the runner expects it
  if (!fs.existsSync(path.join(PULL_REPO_DIR, FIXTURE_FILE))) {
    printError(`  Fixture not found in cloned repo: ${FIXTURE_FILE}`);
    return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
  }

  // ─── Step 1: pre-flight catches violations locally (no HTTP) ───────────────
  {
    const r = runAgent(agentBin,
      ['pull', '--files', FIXTURE_FILE, '--branch', TEST_BRANCH, '--conflict-mode', 'ignore'],
      { cwd: PULL_REPO_DIR, timeout: 30000 });
    const ok = r.exitCode === 1
      && /SCRTEXT_S "nine char"/.test(r.output)
      && /SCRTEXT_M "eighteen char text"/.test(r.output)
      && /SCRTEXT_L "thirty character long screen text"/.test(r.output)
      && /Either shorten the label or raise/.test(r.output)
      && !/Starting pull for:/.test(r.output);  // proves no HTTP call was made
    addResult('pre-flight catches DTEL SCRLEN violations locally (exit 1, no HTTP)', ok, r.output);
  }

  // ─── Step 2: server-side detects + classifies as failure (Changes #1+#2+#3) ─
  // Use --url to bypass the pre-flight and hit the ABAP backend.
  {
    const r = runAgent(agentBin,
      ['pull', '--url', PULL_REPO_URL, '--files', FIXTURE_FILE, '--branch', TEST_BRANCH, '--conflict-mode', 'ignore'],
      { cwd: PULL_REPO_DIR, timeout: 120000 });
    const ok = r.exitCode === 1
      && /Pull completed with errors/.test(r.output)
      && /see log_messages/.test(r.output)
      && /Full message text \(1 truncated above\)/.test(r.output)  // Change #3
      && new RegExp(`DTEL ${DTEL_NAME}`).test(r.output)
      && /SCRTEXT_S.*SCRTEXT_M.*SCRTEXT_L/s.test(r.output);
    addResult('server-side: pull --url surfaces SCRLEN violations and exits 1', ok, r.output);
  }

  // ─── Step 3: JSON mode returns structured LOG_MESSAGES ─────────────────────
  {
    const r = runAgent(agentBin,
      ['pull', '--url', PULL_REPO_URL, '--files', FIXTURE_FILE, '--branch', TEST_BRANCH, '--conflict-mode', 'ignore', '--json'],
      { cwd: PULL_REPO_DIR, timeout: 120000 });
    let ok = false;
    try {
      const json = JSON.parse(r.output);
      const msg = (json.LOG_MESSAGES && json.LOG_MESSAGES[0]) || {};
      ok = json.SUCCESS === ''
        && /see log_messages/.test(json.MESSAGE || '')
        && Array.isArray(json.LOG_MESSAGES) && json.LOG_MESSAGES.length === 1
        && msg.TYPE === 'E'
        && msg.OBJ_TYPE === 'DTEL'
        && msg.OBJ_NAME === DTEL_NAME
        && /SCRTEXT_S/.test(msg.TEXT) && /SCRTEXT_M/.test(msg.TEXT) && /SCRTEXT_L/.test(msg.TEXT);
    } catch { ok = false; }
    addResult('server-side --json emits structured E-type LOG_MESSAGES entry', ok, r.output);
  }

  // ─── Summary ───────────────────────────────────────────────────────────────
  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  if (passedCount === totalCount) {
    printSuccess(`DTEL SCRLEN tests: ${passedCount}/${totalCount} PASSED (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`DTEL SCRLEN tests: ${passedCount}/${totalCount} PASSED (${duration}s)`);
    return { success: false, results, duration, passedCount, totalCount };
  }
}

module.exports = { runDtelScrlenTests };

// Allow direct invocation: node tests/integration/dtel-scrlen-runner.js
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

  const result = runDtelScrlenTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors });
  process.exit(result.success ? 0 : 1);
}
