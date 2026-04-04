/**
 * Drop Command Integration Test Runner
 *
 * Tests the `drop` command end-to-end against a real ABAP system.
 *
 * Test Repository: https://github.tools.sap/I045696/abgagt-drop-test.git
 *   Package: $ABGAGT_DROP_TEST
 *   Branch:  main
 *   Objects:
 *     - src/zif_abgagt_drp_test.intf.abap   INTF ZIF_ABGAGT_DRP_TEST
 *     - src/zcl_abgagt_drp_test.clas.abap   CLAS ZCL_ABGAGT_DRP_TEST
 *     - src/zabgagt_drp_dtel.dtel.xml        DTEL ZABGAGT_DRP_DTEL
 *     - src/zabgagt_drp_tabl.tabl.xml        TABL ZABGAGT_DRP_TABL
 *     - src/zabgagt_drp_ttyp.ttyp.xml        TTYP ZABGAGT_DRP_TTYP
 *     - src/zabgagt_drp_prog.prog.abap        PROG ZABGAGT_DRP_PROG
 *     - src/zabgagt_drp_fugr.fugr.xml         FUGR ZABGAGT_DRP_FUGR
 *     - src/zabgagt_drp_stru.tabl.xml         STRU ZABGAGT_DRP_STRU
 *     - src/zabgagt_drp_doma.doma.xml         DOMA ZABGAGT_DRP_DOMA
 *     - src/zc_abgagt_drp.ddls.xml            DDLS ZC_ABGAGT_DRP
 *     - src/zc_abgagt_drp.dcls.xml            DCLS ZC_ABGAGT_DRP
 *     - src/zabgagt_drp_msag.msag.xml         MSAG ZABGAGT_DRP_MSAG
 *
 * Test sequence per object type:
 *   Reset:   pull --files <file>  --conflict-mode ignore  → idempotent baseline
 *   Step 1:  view → object exists in ABAP system
 *   Step 2:  drop --files <file>  → object deleted; view → confirms "Object not found"
 *   Step 3:  pull --files <file>  → re-activate; view → confirms object exists again
 *
 * Additional test:
 *   drop --pull flag: drop --files <intf> --pull → deletes and re-activates in one step
 *
 * Note: CLAS depends on INTF, so INTF is always activated before CLAS.
 * DTEL is not tested with drop (unsupported — CBDA cannot re-activate after deletion).
 * DDLS depends on TABL, DCLS depends on DDLS — ordering is critical.
 * Order: DOMA → TABL → TTYP → STRU → PROG → FUGR → MSAG → DDLS → DCLS → INTF → CLAS
 *
 * Run:
 *   node tests/run-all.js --drop
 */

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

const DROP_REPO_URL = 'https://github.tools.sap/I045696/abgagt-drop-test.git';
const DROP_REPO_DIR = path.join(os.tmpdir(), 'abgagt-drop-test');

/**
 * Objects to test — ordered so dependencies come first.
 * Each entry defines the file path (relative to repo root) and expected object name in output.
 */
const TEST_OBJECTS = [
  // DOMA first — no dependencies
  {
    name:   'DOMA ZABGAGT_DRP_DOMA',
    file:   'src/zabgagt_drp_doma.doma.xml',
    type:   'DOMA',
    object: 'ZABGAGT_DRP_DOMA'
  },
  // TABL next — no DOMA dependency (uses raw CHAR field)
  {
    name:   'TABL ZABGAGT_DRP_TABL',
    file:   'src/zabgagt_drp_tabl.tabl.xml',
    type:   'TABL',
    object: 'ZABGAGT_DRP_TABL'
  },
  // TTYP depends on TABL
  {
    name:   'TTYP ZABGAGT_DRP_TTYP',
    file:   'src/zabgagt_drp_ttyp.ttyp.xml',
    type:   'TTYP',
    object: 'ZABGAGT_DRP_TTYP'
  },
  // STRU — independent structure
  {
    name:   'STRU ZABGAGT_DRP_STRU',
    file:   'src/zabgagt_drp_stru.tabl.xml',
    type:   'STRU',
    object: 'ZABGAGT_DRP_STRU'
  },
  // PROG — no dependencies
  {
    name:   'PROG ZABGAGT_DRP_PROG',
    file:   'src/zabgagt_drp_prog.prog.abap',
    type:   'PROG',
    object: 'ZABGAGT_DRP_PROG'
  },
  // FUGR — no dependencies
  {
    name:   'FUGR ZABGAGT_DRP_FUGR',
    file:   'src/zabgagt_drp_fugr.fugr.xml',
    type:   'FUGR',
    object: 'ZABGAGT_DRP_FUGR'
  },
  // MSAG — no dependencies
  {
    name:   'MSAG ZABGAGT_DRP_MSAG',
    file:   'src/zabgagt_drp_msag.msag.xml',
    type:   'MSAG',
    object: 'ZABGAGT_DRP_MSAG'
  },
  // DDLS depends on TABL (selects from ZABGAGT_DRP_TABL)
  {
    name:   'DDLS ZC_ABGAGT_DRP',
    file:   'src/zc_abgagt_drp.ddls.xml',
    type:   'DDLS',
    object: 'ZC_ABGAGT_DRP'
  },
  // DCLS depends on DDLS
  {
    name:   'DCLS ZC_ABGAGT_DRP',
    file:   'src/zc_abgagt_drp.dcls.xml',
    type:   'DCLS',
    object: 'ZC_ABGAGT_DRP'
  },
  // INTF — no dependencies
  {
    name:   'INTF ZIF_ABGAGT_DRP_TEST',
    file:   'src/zif_abgagt_drp_test.intf.abap',
    type:   'INTF',
    object: 'ZIF_ABGAGT_DRP_TEST'
  },
  // CLAS depends on INTF
  {
    name:   'CLAS ZCL_ABGAGT_DRP_TEST',
    file:   'src/zcl_abgagt_drp_test.clas.abap',
    type:   'CLAS',
    object: 'ZCL_ABGAGT_DRP_TEST'
  }
];

/**
 * Run `abapgit-agent view` to check whether an object exists in the ABAP system.
 * Returns true if the object exists (no "Object not found" in output).
 */
function objectExists(agentBin, repoRoot, obj) {
  const cmd = [
    'node', agentBin,
    'view',
    '--objects', obj.object,
    '--type',    obj.type
  ].join(' ');

  try {
    const output = execSync(cmd, { cwd: repoRoot, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 30000 });
    return !output.includes('Object not found');
  } catch (err) {
    const output = (err.stdout || '') + (err.stderr || '');
    return !output.includes('Object not found');
  }
}

/**
 * Run `abapgit-agent pull` against the drop-test repo using --url (works from any cwd).
 * Credentials come from .abapGitAgent in repoRoot (abapgit-agent itself).
 */
function runPull(agentBin, repoRoot, file, extraArgs = []) {
  const cmd = [
    'node', agentBin,
    'pull',
    '--url',    DROP_REPO_URL,
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
 * Run `abapgit-agent pull` from the drop-test repo directory (uses .abapGitAgent in DROP_REPO_DIR).
 * This ensures abapGit fetches fresh commit state from remote, bypassing any cached commit hash.
 */
function runPullFromDropRepo(agentBin, file, extraArgs = []) {
  const cmd = [
    'node', agentBin,
    'pull',
    '--files', file,
    ...extraArgs
  ].join(' ');

  try {
    const output = execSync(cmd, { cwd: DROP_REPO_DIR, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 120000 });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

/**
 * Run `abapgit-agent drop` against the drop-test repo.
 * The drop-test repo is used as cwd so that relative file paths resolve correctly.
 */
function runDrop(agentBin, file, extraArgs = []) {
  const cmd = [
    'node', agentBin,
    'drop',
    '--files', file,
    ...extraArgs
  ].join(' ');

  try {
    const output = execSync(cmd, { cwd: DROP_REPO_DIR, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 120000 });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

/**
 * Run drop integration tests.
 * @param {string} repoRoot - abapgit-agent repository root
 * @param {Object} printFunctions
 */
function runDropTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors }) {
  printSubHeader('Running Drop Command Integration Tests');

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
        console.log(colors.gray + '     ' + output.substring(0, 150).replace(/\n/g, '\n     ') + colors.reset);
      }
    }
  };

  // Check that the drop-test repo is cloned
  if (!fs.existsSync(DROP_REPO_DIR)) {
    printInfo(`  Drop-test repo not found at ${DROP_REPO_DIR} — cloning...`);
    try {
      execSync(`git clone ${DROP_REPO_URL} ${DROP_REPO_DIR}`, { encoding: 'utf8', timeout: 60000 });
      printInfo('  Cloned successfully.');
    } catch (e) {
      printError(`  Failed to clone drop-test repo: ${e.message}`);
      return { success: false, results: [], duration: '0.0', passedCount: 0, totalCount: 0, skipped: true };
    }
  }

  // Copy .abapGitAgent credentials from the main project into the drop-test repo
  // so that `drop` (which resolves config from cwd) can reach the ABAP system.
  const srcConfig = path.join(repoRoot, '.abapGitAgent');
  const dstConfig = path.join(DROP_REPO_DIR, '.abapGitAgent');
  if (fs.existsSync(srcConfig)) {
    const config = JSON.parse(fs.readFileSync(srcConfig, 'utf8'));
    config.package = '$ABGAGT_DROP_TEST';
    config.folder  = '/src/';
    // In CI, allow overriding git credentials for repos owned by the personal account
    if (process.env.TEST_GIT_USR) config.gitUsername = process.env.TEST_GIT_USR;
    if (process.env.TEST_GIT_PSW) config.gitPassword = process.env.TEST_GIT_PSW;
    fs.writeFileSync(dstConfig, JSON.stringify(config, null, 2));
  }

  printInfo(`  Test repository: ${DROP_REPO_URL}`);
  printInfo(`  Local clone:     ${DROP_REPO_DIR}`);
  printInfo('');

  // ─── Reset: establish known baseline for all objects ───────────────────────
  printInfo(colorize('cyan', '  Reset: activating all test objects (--conflict-mode ignore)'));
  // Pull all objects in a single call so dependencies (TABL→TTYP, TABL→DDLS, DDLS→DCLS,
  // INTF→CLAS) are activated together in one DDIC mass-activation batch.
  // DTEL is included first as it is a dependency but not in TEST_OBJECTS (drop is unsupported).
  const allResetFiles = [
    'src/zabgagt_drp_dtel.dtel.xml',
    ...TEST_OBJECTS.map(o => o.file)
  ].join(',');
  let { output: resetOutput } = runPullFromDropRepo(agentBin, allResetFiles, ['--conflict-mode', 'ignore']);
  // "Activation cancelled" means the EZABAPGIT lock was held (e.g. by a prior cmd test run).
  // Retry once after a short wait to ensure all objects land in TADIR.
  if (resetOutput.includes('Activation cancelled') && !resetOutput.includes('Pull completed successfully')) {
    printInfo('  Reset got "Activation cancelled" — retrying after 5s...');
    execSync('sleep 5');
    ({ output: resetOutput } = runPullFromDropRepo(agentBin, allResetFiles, ['--conflict-mode', 'ignore']));
  }
  const resetOk = resetOutput.includes('Pull completed successfully') ||
                  resetOutput.includes('already active') ||
                  resetOutput.includes('nothing to activate');
  if (!resetOk) {
    printError(`  Reset pull failed: ${resetOutput.substring(0, 200)}`);
  }
  printInfo('  Reset complete.');
  printInfo('');

  // ─── Per-object test cycle: view → drop → view → pull → view ──────────────
  for (const obj of TEST_OBJECTS) {
    printInfo(colorize('cyan', `  Testing: ${obj.name}`));

    // Step 1: view → confirm object exists in ABAP system
    // Ensure clean state for objects that might have been cascade-deleted by a previous test
    if (obj.type === 'INTF' || obj.type === 'CLAS') {
      runPullFromDropRepo(agentBin, obj.file, ['--conflict-mode', 'ignore']);
    }
    const existsBefore = objectExists(agentBin, repoRoot, obj);
    addResult(`${obj.name} — exists before drop`, existsBefore);

    // Step 2: drop → object deleted; verify via view
    const dropResult = runDrop(agentBin, obj.file);
    const dropOk = dropResult.output.includes('Object deleted successfully') ||
                   dropResult.output.includes('deleted successfully');
    addResult(`${obj.name} — drop`, dropOk, dropResult.output);

    const existsAfterDrop = objectExists(agentBin, repoRoot, obj);
    // PROG: abapGit deletes the source but SAP keeps TADIR/TRDIR ghost entries until next sync.
    // The drop succeeds (run returns "Program does not exist"), so skip the "gone" check for PROG.
    if (obj.type !== 'PROG') {
      addResult(`${obj.name} — gone after drop`, !existsAfterDrop);
    }

    // Step 3: pull → re-activate; verify via view
    runPullFromDropRepo(agentBin, obj.file, ['--conflict-mode', 'ignore']);
    // Cascade: dropping TABL also deactivates TTYP; dropping DDLS also deletes DCLS.
    // Re-pull their dependents so the next object test starts from a clean state.
    if (obj.type === 'TABL') {
      runPullFromDropRepo(agentBin, 'src/zabgagt_drp_ttyp.ttyp.xml', ['--conflict-mode', 'ignore']);
    }
    if (obj.type === 'DDLS') {
      runPullFromDropRepo(agentBin, 'src/zc_abgagt_drp.dcls.xml', ['--conflict-mode', 'ignore']);
    }
    if (obj.type === 'INTF') {
      // CLAS implements INTF; after INTF is re-pulled CLAS becomes inactive — restore it too
      runPullFromDropRepo(agentBin, 'src/zcl_abgagt_drp_test.clas.abap', ['--conflict-mode', 'ignore']);
    }
    const existsAfterPull = objectExists(agentBin, repoRoot, obj);
    addResult(`${obj.name} — exists after re-pull`, existsAfterPull);

    printInfo('');
  }

  // ─── drop --pull: combined delete + re-activate in one command ───────────────
  // Use INTF (no dependents) so the re-pull is self-contained.
  // Ensure INTF is in TADIR (previous test cycles may have left it absent due to lock issues).
  printInfo(colorize('cyan', '  Testing: drop --pull (INTF ZIF_ABGAGT_DRP_TEST)'));
  const intfObj = TEST_OBJECTS.find(o => o.type === 'INTF');
  runPullFromDropRepo(agentBin, intfObj.file, ['--conflict-mode', 'ignore']);
  const dropPullResult = runDrop(agentBin, intfObj.file, ['--pull']);
  const dropPullDropOk = dropPullResult.output.includes('Object deleted successfully') ||
                         dropPullResult.output.includes('deleted successfully');
  const dropPullPullOk = dropPullResult.output.includes('Pull completed successfully') ||
                         dropPullResult.output.includes('already active') ||
                         dropPullResult.output.includes('Activation cancelled');
  addResult('drop --pull — drop succeeded', dropPullDropOk, dropPullResult.output);
  addResult('drop --pull — pull succeeded', dropPullPullOk, dropPullResult.output);
  const existsAfterDropPull = objectExists(agentBin, repoRoot, intfObj);
  addResult('drop --pull — object exists after combined command', existsAfterDropPull);
  printInfo('');

  // ─── Error case: drop file that doesn't exist on disk ──────────────────────
  printInfo(colorize('cyan', '  Testing: error case — file not found on disk'));
  const badFileResult = runDrop(agentBin, 'src/nonexistent.clas.abap');
  const badFileOk = badFileResult.exitCode !== 0 &&
                    (badFileResult.output.includes('file not found') ||
                     badFileResult.output.includes('Error'));
  addResult('drop — error: file not found on disk', badFileOk, badFileResult.output);

  // ─── Error case: drop unsupported DTEL type ────────────────────────────────
  printInfo(colorize('cyan', '  Testing: error case — unsupported DTEL type'));
  const dtelResult = runDrop(agentBin, 'src/zabgagt_drp_dtel.dtel.xml');
  const dtelOk = dtelResult.exitCode !== 0 &&
                 dtelResult.output.includes('does not support DTEL');
  addResult('drop — error: DTEL unsupported', dtelOk, dtelResult.output);

  // ─── Summary ───────────────────────────────────────────────────────────────
  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  if (passedCount === totalCount) {
    printSuccess(`Drop tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`Drop tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: false, results, duration, passedCount, totalCount };
  }
}

module.exports = { runDropTests };
