/**
 * Full Pull Test Runner (no --files)
 * Mirrors pull-runner.js but exercises the full-pull path — no --files flag.
 * Verifies that conflict detection and ref-switching work correctly when
 * activating all objects in a repository rather than a specific file list.
 *
 * Test Repository: https://github.tools.sap/I045696/abgagt-pull-test.git
 *   - v0.1.0:              get_message only
 *   - v1.0.0:              get_message + validate_input
 *   - feature/test-branch: get_message + calculate_sum
 *   - main:                get_message + validate_input (same as v1.0.0)
 *
 * Test sequence (all steps depend on ABAP system state from the previous step):
 *
 *   Reset:   pull --url ... --branch v0.1.0 --conflict-mode ignore  → idempotent baseline
 *   Step 1:  pull --url ... --branch v0.1.0  (no --files)           → no conflict (same ref)
 *            verify: only get_message
 *   Step 2:  pull --url ... --branch v1.0.0  (no --files)           → succeeds (same user branch switch)
 *            verify: get_message + validate_input
 *   Step 3:  pull --url ... --branch feature/test-branch (no --files) → succeeds (same user branch switch)
 *            verify: get_message + calculate_sum
 *   Step 4:  pull --url ... --branch main  (no --files)             → succeeds (same user branch switch)
 *            verify: get_message + validate_input
 *   Step 5:  pull --url ... --branch v1.0.0 (same content as main)  → no conflict
 *
 * Note: BRANCH_SWITCH aborts only when a *different* user last pulled from the
 * other branch. Since integration tests run as a single user, same-user branch
 * switches always succeed. The different-user scenario is covered by AUnit
 * (test_brsw_diff_user_abort).
 */

const { execSync } = require('child_process');
const path = require('path');

const TEST_REPO_URL = 'https://github.tools.sap/I045696/abgagt-pull-test.git';
const TEST_OBJECT   = 'ZIF_SIMPLE_TEST';

function runPull(repoRoot, branch, extraArgs = []) {
  const cmd = [
    'node', path.join(repoRoot, 'bin', 'abapgit-agent'),
    'pull',
    '--url', TEST_REPO_URL,
    '--branch', branch,
    // No --files — exercises the full-pull path
    ...extraArgs
  ].join(' ');

  try {
    const output = execSync(cmd, { cwd: repoRoot, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'] });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

function runView(repoRoot) {
  const cmd = [
    'node', path.join(repoRoot, 'bin', 'abapgit-agent'),
    'view', '--objects', TEST_OBJECT
  ].join(' ');

  try {
    return execSync(cmd, { cwd: repoRoot, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'] });
  } catch (err) {
    return (err.stdout || '') + (err.stderr || '');
  }
}

function runFullPullTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize }) {
  printSubHeader('Running Full Pull Tests (no --files, Git Refs with Verification)');

  const startTime = Date.now();
  const results   = [];

  printInfo(`Test repository: ${TEST_REPO_URL}`);
  printInfo(`Test object:     ${TEST_OBJECT}`);
  printInfo('');

  // ─── Reset: establish known baseline on v0.1.0 ─────────────────────────────
  printInfo(colorize('cyan', 'Reset: establish baseline on v0.1.0 (--conflict-mode ignore, no --files)'));
  {
    const { output, exitCode } = runPull(repoRoot, 'v0.1.0', ['--conflict-mode', 'ignore']);
    if (exitCode === 0 && output.includes('Pull completed')) {
      printSuccess('✓ baseline reset to v0.1.0');
    } else {
      printError('✗ baseline reset failed — cannot continue');
      printError(`  ${output.split('\n').find(l => l.includes('Error') || l.includes('failed')) || ''}`);
      return { success: false, results, duration: '0.0', passedCount: 0, totalCount: 9 };
    }
  }
  printInfo('');

  // ─── Step 1: no conflict on same ref ────────────────────────────────────────
  printInfo(colorize('cyan', 'Step 1: no conflict — full pull v0.1.0 again (same ref, no --files)'));
  {
    const { output, exitCode } = runPull(repoRoot, 'v0.1.0');
    const passed = exitCode === 0 && output.includes('Pull completed');
    passed
      ? printSuccess('✓ full pull v0.1.0 succeeded without conflict')
      : printError(`✗ expected no conflict on same ref\n  ${output.split('\n').find(l => l.trim()) || ''}`);
    results.push({ name: 'full pull: no conflict on same ref (v0.1.0)', passed });
  }

  // verify content: get_message only
  {
    const viewOutput = runView(repoRoot);
    const passed = viewOutput.includes('get_message') &&
                   !viewOutput.includes('validate_input') &&
                   !viewOutput.includes('calculate_sum');
    passed
      ? printSuccess('✓ v0.1.0 content correct (get_message only)')
      : printError('✗ v0.1.0 content unexpected');
    results.push({ name: 'full pull: verify v0.1.0 content', passed });
  }
  printInfo('');

  // ─── Step 2: v1.0.0 — same-user branch switch ───────────────────────────────
  // Same user switching refs → always succeeds (no BRANCH_SWITCH).
  printInfo(colorize('cyan', 'Step 2: full pull v1.0.0 (same-user branch switch — should succeed, no --files)'));
  {
    const { output, exitCode } = runPull(repoRoot, 'v1.0.0');
    const passed = exitCode === 0 && output.includes('Pull completed');
    passed
      ? printSuccess('✓ full pull v1.0.0 succeeded')
      : printError(`✗ expected pull to succeed\n  exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);
    results.push({ name: 'full pull: v1.0.0 (same-user branch switch)', passed });
  }

  // verify content: get_message + validate_input
  {
    const viewOutput = runView(repoRoot);
    const passed = viewOutput.includes('get_message') &&
                   viewOutput.includes('validate_input') &&
                   !viewOutput.includes('calculate_sum');
    passed
      ? printSuccess('✓ v1.0.0 content correct (get_message + validate_input)')
      : printError('✗ v1.0.0 content unexpected');
    results.push({ name: 'full pull: verify v1.0.0 content', passed });
  }
  printInfo('');

  // ─── Step 3: feature/test-branch — same-user branch switch ──────────────────
  printInfo(colorize('cyan', 'Step 3: full pull feature/test-branch (same-user branch switch — should succeed, no --files)'));
  {
    const { output, exitCode } = runPull(repoRoot, 'feature/test-branch');
    const passed = exitCode === 0 && output.includes('Pull completed');
    passed
      ? printSuccess('✓ full pull feature/test-branch succeeded')
      : printError(`✗ expected pull to succeed\n  exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);
    results.push({ name: 'full pull: feature/test-branch (same-user branch switch)', passed });
  }

  // verify content: get_message + calculate_sum
  {
    const viewOutput = runView(repoRoot);
    const passed = viewOutput.includes('get_message') &&
                   viewOutput.includes('calculate_sum') &&
                   !viewOutput.includes('validate_input');
    passed
      ? printSuccess('✓ feature/test-branch content correct (get_message + calculate_sum)')
      : printError('✗ feature/test-branch content unexpected');
    results.push({ name: 'full pull: verify feature/test-branch content', passed });
  }
  printInfo('');

  // ─── Step 4: main — same-user branch switch ──────────────────────────────────
  printInfo(colorize('cyan', 'Step 4: full pull main (same-user branch switch — should succeed, no --files)'));
  {
    const { output, exitCode } = runPull(repoRoot, 'main');
    const passed = exitCode === 0 && output.includes('Pull completed');
    passed
      ? printSuccess('✓ full pull main succeeded')
      : printError(`✗ expected pull to succeed\n  exitCode=${exitCode}\n  ${output.split('\n').find(l => l.includes('Error')) || ''}`);
    results.push({ name: 'full pull: main (same-user branch switch)', passed });
  }

  // verify content: get_message + validate_input
  {
    const viewOutput = runView(repoRoot);
    const passed = viewOutput.includes('get_message') &&
                   viewOutput.includes('validate_input') &&
                   !viewOutput.includes('calculate_sum');
    passed
      ? printSuccess('✓ main content correct (get_message + validate_input)')
      : printError('✗ main content unexpected');
    results.push({ name: 'full pull: verify main content', passed });
  }

  // ─── Step 5: branch switch with identical content → no conflict ─────────────
  // main and v1.0.0 have the same content for ZIF_SIMPLE_TEST.
  // Branch switches with git_changed=false must NOT trigger BRANCH_SWITCH.
  printInfo(colorize('cyan', 'Step 5: no conflict — full pull v1.0.0 after main (same content, no --files)'));
  {
    const { output, exitCode } = runPull(repoRoot, 'v1.0.0');
    const passed = exitCode === 0 && output.includes('Pull completed');
    passed
      ? printSuccess('✓ branch switch with identical content succeeded without conflict (full pull)')
      : printError(`✗ expected no conflict when content is identical across refs\n  ${output.split('\n').find(l => l.trim()) || ''}`);
    results.push({ name: 'full pull: no conflict on branch switch with identical content', passed });
  }
  printInfo('');

  const duration    = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount  = results.length;
  const success     = passedCount === totalCount;

  return { success, results, duration, passedCount, totalCount };
}

module.exports = { runFullPullTests };
