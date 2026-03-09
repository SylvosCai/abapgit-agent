/**
 * Conflict Detection Test Runner
 * Tests pull command conflict detection using ZIF_ABGAGT_CONFLICT_TEST
 *
 * The test object exists on two branches with different content:
 *   - main:               get_version method only
 *   - feature/test-branch: get_version + get_branch methods
 *
 * All scenarios are driven by pull sequencing against one object:
 *
 *   1. no-conflict (same branch, same content)  → Pull completed
 *   2. BRANCH_SWITCH (different branch, different content) → Pull aborted
 *   3. conflict-mode ignore overrides conflict   → Pull completed
 *   4. no-conflict after baseline update         → Pull completed
 *   5. BRANCH_SWITCH switching back              → Pull aborted
 *
 * Test repository: https://github.tools.sap/I045696/abgagt-pull-test.git
 */

const { execSync } = require('child_process');
const path = require('path');

const TEST_REPO_URL = 'https://github.tools.sap/I045696/abgagt-pull-test.git';
const TEST_OBJECT   = 'ZIF_ABGAGT_CONFLICT_TEST';
const BRANCH_MAIN   = 'main';
const BRANCH_FEAT   = 'feature/test-branch';

/**
 * Run a pull command scoped to the conflict test object and return { output, exitCode }.
 * Never throws — callers inspect the result.
 */
function runPull(repoRoot, branch, extraArgs = []) {
  const cmd = [
    'node', path.join(repoRoot, 'bin', 'abapgit-agent'),
    'pull',
    '--url', TEST_REPO_URL,
    '--branch', branch,
    '--files', `src/zif_abgagt_conflict_test.intf.abap`,
    ...extraArgs
  ].join(' ');

  try {
    const output = execSync(cmd, {
      cwd: repoRoot,
      encoding: 'utf8',
      stdio: ['pipe', 'pipe', 'pipe']
    });
    return { output, exitCode: 0 };
  } catch (err) {
    // execSync throws on non-zero exit; combine stdout+stderr
    const output = (err.stdout || '') + (err.stderr || '');
    return { output, exitCode: err.status || 1 };
  }
}

/**
 * Conflict detection test sequence.
 * Each step depends on the ABAP system state left by the previous step,
 * so steps must run in order.
 */
function runConflictTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors }) {
  printSubHeader('Running Conflict Detection Tests');

  const startTime = Date.now();
  const results   = [];

  printInfo(`Test repository: ${TEST_REPO_URL}`);
  printInfo(`Test object:     ${TEST_OBJECT}`);
  printInfo('');

  // ─── Step 1: establish baseline ────────────────────────────────────────────
  // Use --conflict-mode ignore so this step is idempotent: it resets the
  // baseline to main regardless of what state a previous test run left behind.
  printInfo(colorize('cyan', 'Step 1: establish baseline (pull main --conflict-mode ignore)'));
  {
    const { output, exitCode } = runPull(repoRoot, BRANCH_MAIN, ['--conflict-mode', 'ignore']);
    const passed = exitCode === 0 && output.includes('Pull completed');
    if (passed) {
      printSuccess(`✓ baseline pull from ${BRANCH_MAIN} succeeded`);
    } else {
      printError(`✗ baseline pull from ${BRANCH_MAIN} failed — cannot continue`);
      printError(`  ${output.split('\n').find(l => l.includes('Error') || l.includes('failed')) || ''}`);
      results.push({ name: `baseline pull from ${BRANCH_MAIN}`, passed: false, error: 'Baseline setup failed' });
      // Abort remaining tests — state is unknown
      const duration = ((Date.now() - startTime) / 1000).toFixed(1);
      return { success: false, results, duration, passedCount: 0, totalCount: 5 };
    }
    results.push({ name: `baseline pull from ${BRANCH_MAIN}`, passed: true });
  }
  printInfo('');

  // ─── Step 2: no-conflict on same branch ────────────────────────────────────
  printInfo(colorize('cyan', 'Step 2: no conflict — pull same branch again'));
  {
    const { output, exitCode } = runPull(repoRoot, BRANCH_MAIN);
    const passed = exitCode === 0 && output.includes('Pull completed');
    if (passed) {
      printSuccess(`✓ second pull from ${BRANCH_MAIN} completed without conflict`);
    } else {
      printError(`✗ expected no conflict on same branch`);
      printError(`  ${output.split('\n').find(l => l.trim()) || ''}`);
    }
    results.push({ name: 'no conflict on same branch', passed });
  }
  printInfo('');

  // ─── Step 3: BRANCH_SWITCH conflict ────────────────────────────────────────
  printInfo(colorize('cyan', `Step 3: BRANCH_SWITCH — pull ${BRANCH_FEAT} (different content)`));
  {
    const { output, exitCode } = runPull(repoRoot, BRANCH_FEAT);
    const hasBranchSwitch = output.includes('BRANCH_SWITCH');
    const hasAborted      = output.includes('Pull aborted');
    const passed = exitCode !== 0 && hasBranchSwitch && hasAborted;
    if (passed) {
      printSuccess(`✓ BRANCH_SWITCH conflict detected as expected`);
    } else {
      printError(`✗ expected BRANCH_SWITCH conflict`);
      printError(`  exitCode=${exitCode} hasBranchSwitch=${hasBranchSwitch} hasAborted=${hasAborted}`);
    }
    results.push({ name: 'BRANCH_SWITCH conflict detected', passed });
  }
  printInfo('');

  // ─── Step 4: conflict-mode ignore ──────────────────────────────────────────
  printInfo(colorize('cyan', `Step 4: --conflict-mode ignore bypasses BRANCH_SWITCH`));
  {
    const { output, exitCode } = runPull(repoRoot, BRANCH_FEAT, ['--conflict-mode', 'ignore']);
    const passed = exitCode === 0 && output.includes('Pull completed');
    if (passed) {
      printSuccess(`✓ pull with --conflict-mode ignore succeeded`);
    } else {
      printError(`✗ expected pull to succeed with --conflict-mode ignore`);
      printError(`  ${output.split('\n').find(l => l.includes('Error') || l.includes('failed')) || ''}`);
    }
    results.push({ name: '--conflict-mode ignore bypasses conflict', passed });
  }
  printInfo('');

  // ─── Step 5: no-conflict after baseline update ─────────────────────────────
  // Step 4 wrote the feature branch baseline — pulling again should be clean.
  printInfo(colorize('cyan', `Step 5: no conflict — pull ${BRANCH_FEAT} again after baseline updated`));
  {
    const { output, exitCode } = runPull(repoRoot, BRANCH_FEAT);
    const passed = exitCode === 0 && output.includes('Pull completed');
    if (passed) {
      printSuccess(`✓ no conflict after baseline update`);
    } else {
      printError(`✗ expected no conflict after baseline was updated in step 4`);
      printError(`  ${output.split('\n').find(l => l.trim()) || ''}`);
    }
    results.push({ name: 'no conflict after baseline update', passed });
  }
  printInfo('');

  const duration     = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount  = results.filter(r => r.passed).length;
  const totalCount   = results.length;
  const success      = passedCount === totalCount;

  return { success, results, duration, passedCount, totalCount };
}

module.exports = { runConflictTests };
