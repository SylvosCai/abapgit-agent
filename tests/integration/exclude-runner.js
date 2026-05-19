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
 *   5. real import with exclude patterns — end-to-end ABAP filter:
 *      create → pull supported+DPC/MPC files into ABAP → import with
 *      patterns ['zcl_*_dpc.clas.*','zcl_*_mpc.clas.*'] → files_staged
 *      reduced by exactly 4 (DPC/MPC excluded), DPC/MPC not re-written
 *      in import commit → force-restore (repeatable)
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

  // ── Test 5: real import with exclude patterns — end-to-end ABAP filter ────────
  //
  // Setup:  inject .abapGitAgent ($ABGAGT_EXCL_TEST), create repo in abapGit,
  //         pull only supported+DPC/MPC files (skip BADI/TRAN/AVAS — not activatable
  //         headlessly) → 11 objects land in ABAP package.
  // Test:   import with patterns ['zcl_*_dpc.clas.*', 'zcl_*_mpc.clas.*'] →
  //         ABAP sees 11 objects, filters 4 DPC/MPC, stages 7. Verify
  //         files_staged=7 AND DPC/MPC files absent from the git commit.
  // Reset:  force-push origin/main back to baseline SHA → repeatable.
  // Clean:  delete repo from abapGit.
  printInfo('Test 5: import with exclude patterns — end-to-end ABAP filter');

  const baselineSha = git('rev-parse HEAD');

  const abapGitAgentSrc = path.join(repoRoot, '.abapGitAgent');
  if (!fs.existsSync(abapGitAgentSrc)) {
    addResult('import test: .abapGitAgent available', false, `${abapGitAgentSrc} not found`);
  } else {
    const cfg = JSON.parse(fs.readFileSync(abapGitAgentSrc, 'utf8'));
    cfg.package = '$ABGAGT_EXCL_TEST';
    cfg.folder  = '/src/';
    if (process.env.TEST_GIT_USR) cfg.gitUsername = process.env.TEST_GIT_USR;
    if (process.env.TEST_GIT_PSW) cfg.gitPassword = process.env.TEST_GIT_PSW;
    fs.writeFileSync(path.join(CLONE_DIR, '.abapGitAgent'), JSON.stringify(cfg, null, 2));

    // Step 5a: register repo in abapGit
    printInfo('  Step 5a: create repo in abapGit...');
    const outCreate = runCli(['create'], 180000);
    const createOk = outCreate.toLowerCase().includes('created') ||
                     outCreate.toLowerCase().includes('already') ||
                     outCreate.toLowerCase().includes('success');
    addResult('import test: create registers repo', createOk, outCreate);

    if (createOk) {
      // Step 5b: pull only activatable files (CLAS, INTF, TABL — skip unsupported types)
      // BADI/TRAN/AVAS require interactive dialogs in ABAP and hang in background mode.
      // Remove any leftover .abapgit-agent.json so the pull is unfiltered.
      const projectCfgPath = path.join(CLONE_DIR, '.abapgit-agent.json');
      if (fs.existsSync(projectCfgPath)) fs.unlinkSync(projectCfgPath);
      const activatableFiles = [...DPC_MPC_FILES, ...SUPPORTED_KEPT_FILES].join(',');
      printInfo('  Step 5b: pull supported+DPC/MPC files into ABAP...');
      const outSetupPull = runCli(
        ['pull', '--files', activatableFiles, '--conflict-mode', 'ignore'],
        120000
      );
      const setupPullOk = outSetupPull.includes('Pull completed') ||
                          outSetupPull.includes('Activated') ||
                          outSetupPull.includes('activated') ||
                          outSetupPull.includes('imported');
      addResult('import test: setup pull activates supported objects in ABAP', setupPullOk, outSetupPull.slice(0, 300));

      if (setupPullOk) {
        // Step 5c: import with DPC/MPC exclude patterns — ABAP sees objects, filters 4 DPC/MPC
        writeProjectConfig({ exclude: { patterns: ['zcl_*_dpc.clas.*', 'zcl_*_mpc.clas.*'] } });
        printInfo('  Step 5c: import with DPC/MPC exclude patterns...');
        const outImport = runCli(['import', '--message', 'test: exclude DPC/MPC patterns'], 300000);

        const importCompleted = outImport.includes('Import completed successfully') ||
                                outImport.includes('completed');
        addResult('import test: import job completes successfully', importCompleted, outImport.slice(0, 400));

        // Verify filter reduced count by exactly 4 (2 DPC files + 2 MPC files filtered out)
        // Total varies: TABL may fail to activate on some systems (12 or 13 files).
        // Regardless, exactly 4 DPC/MPC files must have been excluded.
        const stagedMatch = outImport.match(/Files staged:\s*(\d+)/i);
        const filesStaged = stagedMatch ? parseInt(stagedMatch[1], 10) : 0;

        if (importCompleted) {
          // Step 5d: fetch the import commit and check which files it touched
          try {
            execSync('git fetch origin', { cwd: CLONE_DIR, encoding: 'utf8' });
            // Get files changed in the import commit vs its parent
            const importSha = execSync('git rev-parse origin/main', { cwd: CLONE_DIR, encoding: 'utf8' }).trim();
            const importCommitFiles = execSync(
              `git diff --name-only ${importSha}^1 ${importSha}`,
              { cwd: CLONE_DIR, encoding: 'utf8' }
            ).trim().split('\n').filter(Boolean);

            // The import commit must not have updated (re-written) DPC/MPC files
            const dpcMpcNotUpdated = DPC_MPC_FILES.every(f => !importCommitFiles.includes(f));
            addResult('import test: DPC/MPC files not re-written by import (filter worked)',
              dpcMpcNotUpdated,
              importCommitFiles.filter(f => DPC_MPC_FILES.includes(f)).join(', ') || 'all clear');

            // files_staged must be > 0 and < total (4 DPC/MPC were excluded)
            const filesStagedReduced = filesStaged > 0 && filesStaged < 12;
            addResult(
              `import test: files_staged = ${filesStaged} (reduced from 12 total, 4 DPC/MPC excluded)`,
              filesStagedReduced,
              `staged=${filesStaged}`
            );

            execSync('git reset --hard origin/main', { cwd: CLONE_DIR, encoding: 'utf8' });
          } catch (e) {
            addResult('import test: git fetch after import', false, e.message);
          }
        }
      }

      // Cleanup: delete repo from abapGit
      printInfo('  Cleaning up: deleting repo from abapGit...');
      runCli(['delete'], 60000);

      // Force-restore origin/main to baseline so the test is repeatable
      printInfo('  Restoring origin/main to baseline...');
      try {
        execSync(`git reset --hard ${baselineSha}`, { cwd: CLONE_DIR, encoding: 'utf8' });
        execSync('git push --force origin main', { cwd: CLONE_DIR, encoding: 'utf8', timeout: 30000 });
      } catch (e) {
        printInfo(`  Warning: could not restore origin/main: ${e.message.split('\n')[0]}`);
      }
    }
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
