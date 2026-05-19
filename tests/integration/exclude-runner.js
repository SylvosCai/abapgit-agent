/**
 * Exclude Command Integration Test Runner
 *
 * Tests `abapgit-agent exclude` and the exclude config against the dedicated
 * abgagt-exclude-test repo, which contains a controlled mix of supported and
 * unsupported ABAP object types.
 *
 * Repo inventory (src/):
 *   Supported:   zif_abgagt_excl_test.intf.*       (INTF)
 *                zcl_abgagt_excl_test.clas.*        (CLAS)
 *                zabgagt_excl_test.tabl.xml         (TABL)
 *                zcl_abgagt_excl_dpc_ext.clas.*     (CLAS — kept by pattern test)
 *   Unsupported: zabgagt_excl_test.avas.xml         (AVAS)
 *                zabgagt_excl_test.tran.xml         (TRAN)
 *                zabgagt_excl_test.badi.xml         (BADI)
 *   DPC/MPC:     zcl_abgagt_excl_dpc.clas.*        (matched by zcl_*_dpc.clas.*)
 *                zcl_abgagt_excl_mpc.clas.*        (matched by zcl_*_mpc.clas.*)
 *
 * Test cases:
 *   1. exclude with no config → "No exclude patterns configured" guidance
 *   2. unsupportedTypes: true → removes exactly the 3 unsupported-type files, keeps 7 others
 *   3. patterns only → removes DPC+MPC files (4 files), keeps everything else
 *   4. patterns + unsupportedTypes → removes DPC+MPC+unsupported (7 files total)
 *   5. pull --files with unsupportedTypes:true → BADI file skipped, INTF file
 *      activated in ABAP (uses abgagt-pull-test repo/package; repeatable)
 *
 * Each test resets the repo to its original state via git reset --hard origin/main after running.
 * Test 5 requires ABAP connectivity and the abgagt-pull-test repo to be set up (npm run test:setup).
 *
 * Run:
 *   node tests/run-all.js --exclude
 */

'use strict';

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');
const { getTestRepoUrl } = require('./test-repos');

const EXCLUDE_REPO_URL = getTestRepoUrl('exclude');
const CLONE_DIR = path.join(os.tmpdir(), 'abgagt-exclude-test');

// Known file counts in the test repo (src/ only, excluding package.devc.xml)
const UNSUPPORTED_FILES = [
  'src/zabgagt_excl_test.avas.xml',
  'src/zabgagt_excl_test.tran.xml',
  'src/zabgagt_excl_test.badi.xml',
];
const DPC_MPC_FILES = [
  'src/zcl_abgagt_excl_dpc.clas.abap',
  'src/zcl_abgagt_excl_dpc.clas.xml',
  'src/zcl_abgagt_excl_mpc.clas.abap',
  'src/zcl_abgagt_excl_mpc.clas.xml',
];
const SUPPORTED_KEPT_FILES = [
  'src/zif_abgagt_excl_test.intf.abap',
  'src/zif_abgagt_excl_test.intf.xml',
  'src/zcl_abgagt_excl_test.clas.abap',
  'src/zcl_abgagt_excl_test.clas.xml',
  'src/zabgagt_excl_test.tabl.xml',
  'src/zcl_abgagt_excl_dpc_ext.clas.abap',
  'src/zcl_abgagt_excl_dpc_ext.clas.xml',
];

/**
 * @param {string} repoRoot  - abapgit-agent project root (unused — we use CLONE_DIR)
 * @param {object} helpers
 */
function runExcludeTests(repoRoot, helpers) {
  const { printSubHeader, printInfo, printSuccess, printError } = helpers;

  const startTime = Date.now();
  const results = [];

  function addResult(name, passed, output = '') {
    results.push({ name, passed, output });
    if (passed) {
      printSuccess(`  ✓ ${name}`);
    } else {
      printError(`  ✗ ${name}`);
      if (output) printError(`    Output: ${output.slice(0, 400)}`);
    }
  }

  function git(cmd) {
    return execSync(`git ${cmd}`, { cwd: CLONE_DIR, encoding: 'utf8' }).trim();
  }

  function runCli(args = [], timeout = 60000) {
    try {
      return execSync(`abapgit-agent ${args.join(' ')}`, {
        cwd: CLONE_DIR,
        encoding: 'utf8',
        timeout,
        stdio: ['pipe', 'pipe', 'pipe'],
      });
    } catch (e) {
      return (e.stdout || '') + (e.stderr || '');
    }
  }

  function resetRepo() {
    // Restore any git rm --cached changes and the config file
    try { git('checkout HEAD -- .'); } catch (e) { /* nothing staged */ }
    try { git('checkout .'); } catch (e) { /* nothing modified */ }
    // Re-add any files removed by git rm --cached (they are untracked after rm)
    const untracked = execSync('git ls-files --others --exclude-standard', { cwd: CLONE_DIR, encoding: 'utf8' })
      .split('\n').filter(Boolean);
    if (untracked.length > 0) {
      git(`add ${untracked.map(f => `"${f}"`).join(' ')}`);
      git('reset HEAD -- .');   // unstage, just put back on disk as tracked
    }
    // If a commit was made (exclude adds a commit), revert to the original HEAD
    const log = git('log --oneline -2');
    if (log.includes('chore: exclude ABAP objects')) {
      git('revert HEAD --no-edit');
      git('reset HEAD~1 --soft'); // squash revert into index then reset clean
      git('checkout HEAD -- .');
    }
  }

  function writeProjectConfig(cfg) {
    fs.writeFileSync(path.join(CLONE_DIR, '.abapgit-agent.json'), JSON.stringify(cfg, null, 2));
  }

  function trackedFiles() {
    return git('ls-files').split('\n').filter(Boolean);
  }

  printSubHeader('Exclude Command Tests');

  // ── Ensure test repo is cloned ───────────────────────────────────────────────
  if (!fs.existsSync(path.join(CLONE_DIR, '.git'))) {
    printInfo(`  Cloning ${EXCLUDE_REPO_URL} → ${CLONE_DIR}`);
    try {
      execSync(`git clone "${EXCLUDE_REPO_URL}" "${CLONE_DIR}"`, { encoding: 'utf8', timeout: 60000 });
    } catch (e) {
      addResult('clone exclude test repo', false, e.message);
      return finalize();
    }
  } else {
    printInfo(`  Using existing clone at ${CLONE_DIR}`);
    try { execSync('git fetch origin && git reset --hard origin/main', { cwd: CLONE_DIR, encoding: 'utf8' }); } catch (e) { /* ok */ }
  }
  addResult('clone exclude test repo', fs.existsSync(path.join(CLONE_DIR, '.git')));

  // ── Test 1: no config → guidance ────────────────────────────────────────────
  printInfo('Test 1: exclude with no config → guidance');
  if (fs.existsSync(path.join(CLONE_DIR, '.abapgit-agent.json'))) {
    fs.unlinkSync(path.join(CLONE_DIR, '.abapgit-agent.json'));
  }
  const out1 = runCli(['exclude']);
  addResult('exclude with no config — prints guidance', out1.includes('No exclude patterns configured'), out1);

  // ── Test 2: unsupportedTypes: true ──────────────────────────────────────────
  printInfo('Test 2: unsupportedTypes:true removes 3 unsupported files, keeps 7 others');
  writeProjectConfig({ exclude: { unsupportedTypes: true } });
  const out2 = runCli(['exclude']);

  const removed2 = UNSUPPORTED_FILES.every(f => !trackedFiles().includes(f));
  const kept2 = SUPPORTED_KEPT_FILES.every(f => trackedFiles().includes(f));
  const removedCount2 = parseInt((out2.match(/Removed (\d+) file/) || [])[1]) || 0;

  addResult('unsupportedTypes removes all 3 unsupported-type files', removed2, out2);
  addResult('unsupportedTypes keeps all 7 supported-type files', kept2, trackedFiles().join(', '));
  addResult(`unsupportedTypes removes exactly 3 files (got ${removedCount2})`, removedCount2 === 3, out2);

  // Reset — git rm --cached leaves files untracked; clean them before reset
  try { execSync('git clean -fd', { cwd: CLONE_DIR, encoding: 'utf8' }); } catch (e) { /* ok */ }
  execSync('git fetch origin && git reset --hard origin/main', { cwd: CLONE_DIR, encoding: 'utf8' });

  // ── Test 3: patterns only ────────────────────────────────────────────────────
  printInfo('Test 3: patterns zcl_*_dpc.clas.* / zcl_*_mpc.clas.* removes 4 DPC/MPC files');
  writeProjectConfig({ exclude: { patterns: ['zcl_*_dpc.clas.*', 'zcl_*_mpc.clas.*'] } });
  const out3 = runCli(['exclude']);

  const removed3 = DPC_MPC_FILES.every(f => !trackedFiles().includes(f));
  const keptExt3 = trackedFiles().includes('src/zcl_abgagt_excl_dpc_ext.clas.abap');
  const keptUnsupported3 = UNSUPPORTED_FILES.every(f => trackedFiles().includes(f));
  const removedCount3 = parseInt((out3.match(/Removed (\d+) file/) || [])[1]) || 0;

  addResult('patterns removes 4 DPC/MPC files', removed3, out3);
  addResult('patterns keeps dpc_ext (no pattern match)', keptExt3, trackedFiles().join(', '));
  addResult('patterns keeps unsupported-type files (unsupportedTypes not set)', keptUnsupported3, trackedFiles().join(', '));
  addResult(`patterns removes exactly 4 files (got ${removedCount3})`, removedCount3 === 4, out3);

  // Reset
  execSync('git fetch origin && git reset --hard origin/main', { cwd: CLONE_DIR, encoding: 'utf8' });

  // ── Test 4: patterns + unsupportedTypes ─────────────────────────────────────
  printInfo('Test 4: patterns + unsupportedTypes removes 7 files total');
  writeProjectConfig({ exclude: { unsupportedTypes: true, patterns: ['zcl_*_dpc.clas.*', 'zcl_*_mpc.clas.*'] } });
  const out4 = runCli(['exclude']);

  const removedBoth4 = [...UNSUPPORTED_FILES, ...DPC_MPC_FILES].every(f => !trackedFiles().includes(f));
  const keptExt4 = trackedFiles().includes('src/zcl_abgagt_excl_dpc_ext.clas.abap');
  const removedCount4 = parseInt((out4.match(/Removed (\d+) file/) || [])[1]) || 0;

  addResult('patterns+unsupportedTypes removes all 7 excluded files', removedBoth4, out4);
  addResult('patterns+unsupportedTypes keeps dpc_ext', keptExt4, trackedFiles().join(', '));
  addResult(`patterns+unsupportedTypes removes exactly 7 files (got ${removedCount4})`, removedCount4 === 7, out4);

  // Reset
  execSync('git fetch origin && git reset --hard origin/main', { cwd: CLONE_DIR, encoding: 'utf8' });

  // ── Test 5: pull with excludeUnsupportedTypes — Node.js filter + ABAP activation ──
  // Injects .abapGitAgent pointing to $ABGAGT_PULL_TEST (which has ZIF_SIMPLE_TEST),
  // sets excludeUnsupportedTypes:true in .abapgit-agent.json, then runs:
  //   pull --files src/zif_simple_test.intf.abap,src/zabgagt_excl_test.badi.xml
  // The BADI file is an unsupported type and must be skipped; the INTF file must be
  // pulled and activated in ABAP. Verifies via pull output messages.
  // Repeatable: no git state is modified — pull only activates objects in ABAP.
  printInfo('Test 5: pull with unsupportedTypes:true — BADI skipped, INTF activated');

  // Copy the BADI file from the exclude test repo into the pull-test clone dir
  // so the path exists when pull tries to read it (pull reads local files).
  const PULL_TEST_DIR = path.join(os.tmpdir(), 'abgagt-pull-test');
  const pullTestReady = fs.existsSync(path.join(PULL_TEST_DIR, '.git'));

  if (!pullTestReady) {
    addResult('pull test: pull-test repo clone available', false, `${PULL_TEST_DIR} not found — run npm run test:setup first`);
  } else {
    // Inject .abapGitAgent using pull-test package + credentials from repoRoot
    const abapGitAgentConfig = path.join(repoRoot, '.abapGitAgent');
    if (fs.existsSync(abapGitAgentConfig)) {
      const cfg = JSON.parse(fs.readFileSync(abapGitAgentConfig, 'utf8'));
      cfg.package = '$ABGAGT_PULL_TEST';
      cfg.folder  = '/src/';
      if (process.env.TEST_GIT_USR) cfg.gitUsername = process.env.TEST_GIT_USR;
      if (process.env.TEST_GIT_PSW) cfg.gitPassword = process.env.TEST_GIT_PSW;
      fs.writeFileSync(path.join(PULL_TEST_DIR, '.abapGitAgent'), JSON.stringify(cfg, null, 2));
    }

    // Copy a BADI file from the exclude test repo so it exists on disk in pull-test dir
    const badiSrc  = path.join(CLONE_DIR, 'src/zabgagt_excl_test.badi.xml');
    const badiDest = path.join(PULL_TEST_DIR, 'src/zabgagt_excl_test.badi.xml');
    if (fs.existsSync(badiSrc)) fs.copyFileSync(badiSrc, badiDest);

    // Set exclude config: unsupportedTypes only (no patterns)
    fs.writeFileSync(
      path.join(PULL_TEST_DIR, '.abapgit-agent.json'),
      JSON.stringify({ exclude: { unsupportedTypes: true } }, null, 2)
    );

    printInfo('  Running pull --files with INTF + BADI (unsupported)...');
    const pullFiles = 'src/zif_simple_test.intf.abap,src/zabgagt_excl_test.badi.xml';
    const outPull5 = execSync(
      `abapgit-agent pull --files "${pullFiles}" --conflict-mode ignore`,
      { cwd: PULL_TEST_DIR, encoding: 'utf8', timeout: 60000, stdio: ['pipe', 'pipe', 'pipe'] }
    ).trim();

    // BADI must be skipped (excluded warning), INTF must be pulled
    const badiSkipped = outPull5.includes('zabgagt_excl_test.badi') ||
                        outPull5.toLowerCase().includes('exclud') ||
                        outPull5.toLowerCase().includes('skip');
    const intfPulled  = outPull5.includes('Pull completed') || outPull5.includes('Activated') ||
                        outPull5.includes('zif_simple_test');
    addResult('pull test: unsupported BADI file skipped with unsupportedTypes:true', badiSkipped, outPull5.slice(0, 400));
    addResult('pull test: supported INTF file pulled successfully', intfPulled, outPull5.slice(0, 400));

    // Cleanup: remove injected files from pull-test dir
    try { fs.unlinkSync(badiDest); } catch (e) { /* ok */ }
    try { fs.unlinkSync(path.join(PULL_TEST_DIR, '.abapgit-agent.json')); } catch (e) { /* ok */ }
    try { fs.unlinkSync(path.join(PULL_TEST_DIR, '.abapGitAgent')); } catch (e) { /* ok */ }
  }

  // ── Cleanup ──────────────────────────────────────────────────────────────────
  if (fs.existsSync(path.join(CLONE_DIR, '.abapgit-agent.json'))) {
    fs.unlinkSync(path.join(CLONE_DIR, '.abapgit-agent.json'));
  }
  if (fs.existsSync(path.join(CLONE_DIR, '.abapGitAgent'))) {
    fs.unlinkSync(path.join(CLONE_DIR, '.abapGitAgent'));
  }

  function finalize() {
    const duration = ((Date.now() - startTime) / 1000).toFixed(1);
    const passedCount = results.filter(r => r.passed).length;
    return { success: passedCount === results.length, results, duration, passedCount, totalCount: results.length };
  }

  return finalize();
}

module.exports = { runExcludeTests };
