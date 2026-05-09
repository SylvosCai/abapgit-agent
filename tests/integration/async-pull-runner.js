/**
 * Async Pull Integration Test Runner
 * Verifies that pulling >10 files triggers the background job path (HTTP 202)
 * and that the pull still completes successfully with real-time progress output.
 *
 * Test Repository: abgagt-pull-type-test (URL configured via testRepos.pullType in .abapGitAgent)
 *   Branch: feature/async-test
 *     - src/zcl_abgagt_ptt.clas.abap + .xml  (1 class = 1 file for pull)
 *     - src/zif_abgagt_ptt_01..10.intf.abap  (10 interfaces)
 *   Total: 11 .abap files → exceeds the >10 threshold → background job scheduled
 *
 * Test sequence:
 *
 *   Reset:   pull all 11 files with --conflict-mode ignore → idempotent baseline
 *
 *   Step 1:  pull all 11 files (no reset flag)
 *            verify: output contains 'background job' (async path taken)
 *            verify: pull completed successfully
 *
 *   Step 2:  pull single file (synchronous path — should NOT mention background job)
 *            verify: output does NOT contain 'background job'
 *            verify: pull completed successfully
 */

'use strict';

const { execSync } = require('child_process');
const path = require('path');

const { getTestRepoUrl } = require('./test-repos');
const TEST_REPO_URL = getTestRepoUrl('pullType');
const TEST_BRANCH   = 'feature/async-test';

const ALL_FILES = [
  'src/zcl_abgagt_ptt.clas.abap',
  'src/zif_abgagt_ptt_01.intf.abap',
  'src/zif_abgagt_ptt_02.intf.abap',
  'src/zif_abgagt_ptt_03.intf.abap',
  'src/zif_abgagt_ptt_04.intf.abap',
  'src/zif_abgagt_ptt_05.intf.abap',
  'src/zif_abgagt_ptt_06.intf.abap',
  'src/zif_abgagt_ptt_07.intf.abap',
  'src/zif_abgagt_ptt_08.intf.abap',
  'src/zif_abgagt_ptt_09.intf.abap',
  'src/zif_abgagt_ptt_10.intf.abap',
];

function runPull(repoRoot, files, extraArgs = []) {
  const cmd = [
    'node', path.join(repoRoot, 'bin', 'abapgit-agent'),
    'pull',
    '--url',    TEST_REPO_URL,
    '--branch', TEST_BRANCH,
    '--files',  files.join(','),
    ...extraArgs
  ].join(' ');

  try {
    const output = execSync(cmd, { cwd: repoRoot, encoding: 'utf8', stdio: ['pipe', 'pipe', 'pipe'], timeout: 300000 });
    return { output, exitCode: 0 };
  } catch (err) {
    return { output: (err.stdout || '') + (err.stderr || ''), exitCode: err.status || 1 };
  }
}

function runAsyncPullTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize }) {
  printSubHeader('Running Async Pull Tests (>10 files → background job)');

  const startTime = Date.now();
  const results   = [];

  printInfo(`Test repository: ${TEST_REPO_URL}`);
  printInfo(`Branch:          ${TEST_BRANCH}`);
  printInfo(`Files:           ${ALL_FILES.length} (threshold: >10 → background job)`);
  printInfo('');

  // ─── Reset: establish known baseline ────────────────────────────────────────
  printInfo(colorize('cyan', 'Reset: establish baseline (--conflict-mode ignore)'));
  {
    const { output, exitCode } = runPull(repoRoot, ALL_FILES, ['--conflict-mode', 'ignore']);
    if (exitCode === 0 && output.includes('Pull completed')) {
      printSuccess('✓ baseline reset succeeded');
    } else {
      printError('✗ baseline reset failed — cannot continue');
      printError(`  ${output.split('\n').find(l => l.includes('Error') || l.includes('failed')) || output.substring(0, 200)}`);
      return { success: false, results, duration: '0.0', passedCount: 0, totalCount: 2 };
    }
  }
  printInfo('');

  // ─── Step 1: >10 files → async path ─────────────────────────────────────────
  printInfo(colorize('cyan', `Step 1: pull ${ALL_FILES.length} files → should run as background job`));
  {
    const { output, exitCode } = runPull(repoRoot, ALL_FILES);
    const isAsync   = output.includes('background job');
    const completed = exitCode === 0 && output.includes('Pull completed');
    const passed    = isAsync && completed;
    if (passed) {
      printSuccess(`✓ async path taken and pull completed`);
    } else {
      if (!isAsync) printError(`✗ expected 'background job' in output but not found`);
      if (!completed) printError(`✗ pull did not complete successfully (exitCode=${exitCode})`);
      printError(`  output: ${output.substring(0, 300)}`);
    }
    results.push({ name: `pull ${ALL_FILES.length} files triggers background job`, passed });
  }
  printInfo('');

  // ─── Step 2: 1 file → synchronous path ──────────────────────────────────────
  printInfo(colorize('cyan', 'Step 2: pull 1 file → should run synchronously (no background job)'));
  {
    const { output, exitCode } = runPull(repoRoot, ['src/zcl_abgagt_ptt.clas.abap']);
    const isSync    = !output.includes('background job');
    const completed = exitCode === 0 && output.includes('Pull completed');
    const passed    = isSync && completed;
    if (passed) {
      printSuccess('✓ synchronous path taken and pull completed');
    } else {
      if (!isSync) printError(`✗ unexpected 'background job' in output for single-file pull`);
      if (!completed) printError(`✗ pull did not complete successfully (exitCode=${exitCode})`);
      printError(`  output: ${output.substring(0, 300)}`);
    }
    results.push({ name: 'pull 1 file runs synchronously (no background job)', passed });
  }
  printInfo('');

  const duration    = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount  = results.length;
  const success     = passedCount === totalCount;

  return { success, results, duration, passedCount, totalCount };
}

module.exports = { runAsyncPullTests };
