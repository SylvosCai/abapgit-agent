/**
 * Integration Test Setup Runner
 *
 * Ensures all prerequisites are satisfied before running integration tests.
 * Designed to be idempotent — safe to run on every `npm run test:all` invocation.
 * Skips steps that are already done (objects active, repos registered, etc.).
 *
 * What this sets up:
 *
 *   pull / full-pull / conflict / sync-xml / xml-only tests
 *     → clone abgagt-pull-test, register in ABAP, activate ZIF_SIMPLE_TEST
 *
 *   drop tests
 *     → clone abgagt-drop-test, inject .abapGitAgent, register, activate all objects
 *
 *   customize tests
 *     → clone abgagt-customize-test, inject .abapGitAgent, register, activate ZABGAGT_CUS_TST
 *
 *   lifecycle tests
 *     → clone abgagt-lifecycle-test (ABAP registration is self-managed by lifecycle-runner)
 *
 *   debug tests (breakpoint management — debug-runner.js)
 *     → clone abgagt-debug-test, inject .abapGitAgent, register, activate debug objects
 *
 *   run tests
 *     → clone abgagt-run-test, inject .abapGitAgent, register, activate ZABGAGT_RUN_TEST + ZCL_ABGAGT_RUN_TEST
 *
 *   syntax tests (FUGR)
 *     → clone abgagt-syntax-test, inject .abapGitAgent, register, activate ZABGAGT_ST_FG + ZABGAGT_ST_ERR
 *
 *   command tests / aunit tests / junit tests
 *     → no special setup needed (use existing project objects in ABAP)
 *
 * Usage:
 *   Called automatically at the start of npm run test:all.
 *   Can also be run standalone: node tests/run-all.js --setup
 */

'use strict';

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

const { getTestRepoUrl } = require('./test-repos');

// ─── Repo definitions ──────────────────────────────────────────────────────

const REPOS = [
  {
    key:     'pull',
    cloneDir: path.join(os.tmpdir(), 'abgagt-pull-test'),
    package: '$ABGAGT_PULL_TEST',
    folder:  '/src/',
    // All files to activate — single file is enough (ZIF_SIMPLE_TEST is the key object)
    activateFiles: ['src/zif_simple_test.intf.abap'],
    // Object to check whether activation already succeeded
    checkObject: { name: 'ZIF_SIMPLE_TEST', type: 'INTF' }
  },
  {
    key:     'drop',
    cloneDir: path.join(os.tmpdir(), 'abgagt-drop-test'),
    package: '$ABGAGT_DROP_TEST',
    folder:  '/src/',
    activateFiles: [
      'src/zabgagt_drp_doma.doma.xml',
      'src/zabgagt_drp_dtel.dtel.xml',
      'src/zabgagt_drp_tabl.tabl.xml',
      'src/zabgagt_drp_ttyp.ttyp.xml',
      'src/zabgagt_drp_stru.tabl.xml',
      'src/zabgagt_drp_prog.prog.abap',
      'src/zabgagt_drp_fugr.fugr.xml',
      'src/zabgagt_drp_msag.msag.xml',
      'src/zc_abgagt_drp.ddls.xml',
      'src/zc_abgagt_drp.dcls.xml',
      'src/zif_abgagt_drp_test.intf.abap',
      'src/zcl_abgagt_drp_test.clas.abap',
      'src/zabgagt_drp_enho.enho.xml'
    ],
    checkObject: { name: 'ZIF_ABGAGT_DRP_TEST', type: 'INTF' }
  },
  {
    key:     'customize',
    cloneDir: path.join(os.tmpdir(), 'abgagt-customize-test'),
    package: '$ABGAGT_CUS_TST',
    folder:  '/src/',
    activateFiles: ['src/zabgagt_cus_tst.tabl.xml'],
    checkObject: { name: 'ZABGAGT_CUS_TST', type: 'TABL' }
  },
  {
    key:     'debug',
    cloneDir: path.join(os.tmpdir(), 'abgagt-debug-test'),
    package: '$ABGAGT_DEBUG_TEST',
    folder:  '/src/',
    activateFiles: null, // activate all objects
    checkObject: { name: 'ZCL_ABGAGT_DBG_TRIGGER', type: 'CLAS' }
  },
  {
    key:     'lifecycle',
    cloneDir: path.join(os.tmpdir(), 'abgagt-lifecycle-test'),
    package: null,   // lifecycle-runner manages ABAP registration itself
    folder:  '/src/',
    activateFiles: null,
    checkObject: null  // no ABAP check — just ensure clone exists
  },
  {
    key:     'run',
    cloneDir: path.join(os.tmpdir(), 'abgagt-run-test'),
    package: '$ABGAGT_RUN_TEST',
    folder:  '/src/',
    activateFiles: [
      'src/zabgagt_run_test.prog.abap',
      'src/zcl_abgagt_run_test.clas.abap'
    ],
    checkObject: { name: 'ZCL_ABGAGT_RUN_TEST', type: 'CLAS' }
  },
  {
    key:     'syntax',
    cloneDir: path.join(os.tmpdir(), 'abgagt-syntax-test'),
    package: '$ABGAGT_SYNTAX_TEST',
    folder:  '/src/',
    activateFiles: [
      'src/zabgagt_st_fg.fugr.xml',
      'src/zabgagt_st_err.fugr.xml'
    ],
    checkObject: { name: 'ZABGAGT_ST_FG', type: 'FUGR' }
  }
];

// ─── Helpers ───────────────────────────────────────────────────────────────

/**
 * Run a command, return { output, exitCode }.
 */
function run(cmd, opts = {}) {
  try {
    const output = execSync(cmd, {
      encoding: 'utf8',
      stdio: ['pipe', 'pipe', 'pipe'],
      timeout: opts.timeout || 120000,
      ...opts
    });
    return { output: output || '', exitCode: 0 };
  } catch (err) {
    return {
      output: (err.stdout || '') + (err.stderr || '') || err.message,
      exitCode: err.status || 1
    };
  }
}

/**
 * Clone a git repo to cloneDir if it doesn't already exist as a valid git repo.
 * Returns true on success.
 */
function ensureClone(repoUrl, cloneDir, printInfo, printError) {
  const isValid = fs.existsSync(cloneDir) && (() => {
    try {
      execSync('git rev-parse --is-inside-work-tree', { cwd: cloneDir, stdio: 'pipe' });
      return true;
    } catch { return false; }
  })();

  if (isValid) {
    printInfo(`    clone OK: ${cloneDir}`);
    return true;
  }

  if (fs.existsSync(cloneDir)) {
    printInfo(`    removing invalid clone at ${cloneDir}...`);
    fs.rmSync(cloneDir, { recursive: true, force: true });
  }

  printInfo(`    cloning ${repoUrl} → ${cloneDir} ...`);
  const { output, exitCode } = run(`git clone ${repoUrl} ${cloneDir}`, { timeout: 60000 });
  if (exitCode !== 0) {
    printError(`    clone failed: ${output.substring(0, 200)}`);
    return false;
  }
  printInfo(`    cloned OK`);
  return true;
}

/**
 * Inject .abapGitAgent into cloneDir with the given package/folder overrides.
 */
function injectConfig(repoRoot, cloneDir, pkg, folder) {
  const srcConfig = path.join(repoRoot, '.abapGitAgent');
  if (!fs.existsSync(srcConfig)) return;
  const cfg = JSON.parse(fs.readFileSync(srcConfig, 'utf8'));
  cfg.package = pkg;
  cfg.folder  = folder;
  if (process.env.TEST_GIT_USR) cfg.gitUsername = process.env.TEST_GIT_USR;
  if (process.env.TEST_GIT_PSW) cfg.gitPassword = process.env.TEST_GIT_PSW;
  fs.writeFileSync(path.join(cloneDir, '.abapGitAgent'), JSON.stringify(cfg, null, 2));
}

/**
 * Check whether an ABAP object already exists in the system via `view`.
 */
function objectExists(agentBin, repoRoot, objName, objType) {
  const typeArg = objType ? `--type ${objType}` : '';
  const { output } = run(
    `node ${agentBin} view --objects ${objName} ${typeArg}`,
    { cwd: repoRoot, timeout: 30000 }
  );
  return !output.toLowerCase().includes('object not found') &&
         !output.toLowerCase().includes('not found') &&
         output.trim().length > 0;
}

/**
 * Check whether the abapGit repo is already registered in the ABAP system via `status`.
 * Returns true when status indicates the repo is created.
 */
function repoRegistered(agentBin, cloneDir) {
  const { output } = run(
    `node ${agentBin} status`,
    { cwd: cloneDir, timeout: 30000 }
  );
  const lower = output.toLowerCase();
  return lower.includes('created') && !lower.includes('not created');
}

/**
 * Register the repo in ABAP via `create`.
 * Returns true on success.
 */
function registerRepo(agentBin, cloneDir, printInfo, printError) {
  printInfo(`    registering repo in ABAP (create)...`);
  const { output, exitCode } = run(
    `node ${agentBin} create`,
    { cwd: cloneDir, timeout: 60000 }
  );
  if (exitCode === 0 || output.toLowerCase().includes('success') || output.toLowerCase().includes('created')) {
    printInfo(`    registered OK`);
    return true;
  }
  printError(`    create failed: ${output.substring(0, 200)}`);
  return false;
}

/**
 * Activate objects via `pull`.
 * Uses --conflict-mode ignore for idempotency.
 * filesArg: comma-separated file list, or null to activate all objects.
 */
function activateObjects(agentBin, cloneDir, filesArg, printInfo, printError) {
  const filesFlag = filesArg ? `--files ${filesArg}` : '';
  printInfo(`    activating objects${filesArg ? ' (' + filesArg.split(',').length + ' file(s))' : ' (all)'}...`);
  const { output, exitCode } = run(
    `node ${agentBin} pull ${filesFlag} --conflict-mode ignore`,
    { cwd: cloneDir, timeout: 180000 }
  );
  const ok = output.includes('Pull completed successfully') ||
             output.includes('already active') ||
             output.includes('nothing to activate') ||
             (exitCode === 0 && output.trim().length > 0);
  if (!ok) {
    printError(`    activation failed: ${output.substring(0, 300)}`);
    return false;
  }
  printInfo(`    activated OK`);
  return true;
}

// ─── Main setup function ───────────────────────────────────────────────────

/**
 * Run the setup phase before integration tests.
 *
 * @param {string} repoRoot - abapgit-agent repo root
 * @param {Object} helpers  - { printSubHeader, printInfo, printSuccess, printError, printWarning, colorize }
 * @returns {{ success: boolean, skippedCount: number, setupCount: number, failedCount: number }}
 */
function runSetup(repoRoot, { printSubHeader, printInfo, printSuccess, printError, printWarning, colorize }) {
  printSubHeader('Setup: Preparing Integration Test Prerequisites');

  const agentBin = path.join(repoRoot, 'bin', 'abapgit-agent');
  const startTime = Date.now();

  // ─── Check ABAP connectivity ──────────────────────────────────────────────
  printInfo(colorize('cyan', '  Checking ABAP connectivity...'));
  const configPath = path.join(repoRoot, '.abapGitAgent');
  if (!fs.existsSync(configPath)) {
    printWarning('  .abapGitAgent not found — skipping ABAP-dependent setup');
    printInfo('  (command tests, junit, aunit will still run if env vars are set)');
    return { success: true, skippedCount: REPOS.length, setupCount: 0, failedCount: 0 };
  }

  const { output: healthOut, exitCode: healthExit } = run(
    `node ${agentBin} health`,
    { cwd: repoRoot, timeout: 15000 }
  );
  const abapReachable = healthExit === 0 ||
    healthOut.toLowerCase().includes('healthy') ||
    healthOut.toLowerCase().includes('"status"');

  if (!abapReachable) {
    printError(`  ABAP system unreachable (health check failed): ${healthOut.substring(0, 150)}`);
    printWarning('  Skipping all ABAP-dependent setup — integration tests may fail');
    return { success: false, skippedCount: REPOS.length, setupCount: 0, failedCount: 0 };
  }
  printInfo(colorize('green', '  ABAP system reachable ✓'));
  printInfo('');

  // ─── Per-repo setup ───────────────────────────────────────────────────────
  let setupCount  = 0;
  let skippedCount = 0;
  let failedCount  = 0;

  for (const repo of REPOS) {
    const repoUrl = getTestRepoUrl(repo.key);
    printInfo(colorize('cyan', `  [${repo.key}] ${repoUrl}`));

    // 1. Clone
    const cloned = ensureClone(repoUrl, repo.cloneDir, printInfo, printError);
    if (!cloned) {
      printError(`  [${repo.key}] FAILED — cannot clone repo`);
      failedCount++;
      printInfo('');
      continue;
    }

    // Lifecycle repo: no ABAP setup needed — lifecycle-runner manages it
    if (repo.key === 'lifecycle') {
      printInfo(`    ABAP setup skipped (lifecycle-runner self-manages registration)`);
      skippedCount++;
      printInfo('');
      continue;
    }

    // 2. Inject .abapGitAgent into clone
    injectConfig(repoRoot, repo.cloneDir, repo.package, repo.folder);

    // 3. Check if already active (quick short-circuit)
    if (repo.checkObject) {
      printInfo(`    checking ${repo.checkObject.name} in ABAP...`);
      if (objectExists(agentBin, repoRoot, repo.checkObject.name, repo.checkObject.type)) {
        printInfo(colorize('green', `    ${repo.checkObject.name} already active — skipping setup`));
        skippedCount++;
        printInfo('');
        continue;
      }
    }

    // 4. Register repo in ABAP if not already registered
    if (!repoRegistered(agentBin, repo.cloneDir)) {
      const registered = registerRepo(agentBin, repo.cloneDir, printInfo, printError);
      if (!registered) {
        // Try import as fallback (create + import combined)
        printInfo(`    trying import as fallback...`);
        const { output: impOut, exitCode: impExit } = run(
          `node ${agentBin} import --message "setup: initial import"`,
          { cwd: repo.cloneDir, timeout: 180000 }
        );
        const importOk = impExit === 0 ||
          impOut.toLowerCase().includes('completed') ||
          impOut.toLowerCase().includes('no objects');
        if (!importOk) {
          printError(`  [${repo.key}] FAILED — cannot register: ${impOut.substring(0, 200)}`);
          failedCount++;
          printInfo('');
          continue;
        }
        printInfo(`    import OK (fallback)`);
      }
    } else {
      printInfo(`    repo already registered in ABAP`);
    }

    // 5. Activate objects
    const filesArg = repo.activateFiles ? repo.activateFiles.join(',') : null;
    const activated = activateObjects(agentBin, repo.cloneDir, filesArg, printInfo, printError);
    if (!activated) {
      printError(`  [${repo.key}] FAILED — cannot activate objects`);
      failedCount++;
      printInfo('');
      continue;
    }

    setupCount++;
    printInfo('');
  }

  // ─── Summary ─────────────────────────────────────────────────────────────
  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const total = REPOS.length;

  if (failedCount === 0) {
    printSuccess(`Setup complete: ${setupCount} set up, ${skippedCount} already ready (${duration}s)`);
    return { success: true, setupCount, skippedCount, failedCount, duration };
  } else {
    printError(`Setup finished with ${failedCount} failure(s): ${setupCount} set up, ${skippedCount} skipped (${duration}s)`);
    return { success: false, setupCount, skippedCount, failedCount, duration };
  }
}

module.exports = { runSetup };
