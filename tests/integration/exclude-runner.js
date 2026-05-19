/**
 * Exclude Command Integration Test Runner
 *
 * Tests the `exclude` command and exclude.patterns config against the current
 * project repository (no separate test repo needed — uses existing ABAP objects).
 *
 * Test cases:
 *   1. `exclude` with no patterns configured → "No exclude patterns configured"
 *   2. `exclude` with patterns → finds matching git-tracked files, removes them,
 *      commits, verifies files are untracked but still present in ABAP system
 *   3. `pull --files` with an excluded file → warning printed, file skipped
 *   4. Cleanup: restore excluded files (git revert the exclude commit)
 *
 * Run:
 *   node tests/run-all.js --exclude
 */

'use strict';

const { execSync, execFileSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

/**
 * @param {string} repoRoot  - path to repo root (contains .abapGitAgent)
 * @param {object} helpers   - { printSubHeader, printInfo, printSuccess, printError, colorize, colors }
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
      if (output) printError(`    Output: ${output.slice(0, 300)}`);
    }
  }

  function runCli(cmd, args = [], opts = {}) {
    try {
      return execFileSync('abapgit-agent', [cmd, ...args], {
        cwd: repoRoot,
        encoding: 'utf8',
        timeout: 60000,
        stdio: ['pipe', 'pipe', 'pipe'],
        ...opts
      });
    } catch (e) {
      return (e.stdout || '') + (e.stderr || '') || e.message;
    }
  }

  function gitExecLocal(cmd) {
    return execSync(cmd, { cwd: repoRoot, encoding: 'utf8' }).trim();
  }

  printSubHeader('Exclude Command Tests');

  // ─── Test 1: no patterns configured ───────────────────────────────────────
  printInfo('Test 1: exclude with no patterns');

  // Temporarily use a project config with no exclude section
  const projectConfigPath = path.join(repoRoot, '.abapgit-agent.json');
  let originalConfig = null;
  if (fs.existsSync(projectConfigPath)) {
    originalConfig = fs.readFileSync(projectConfigPath, 'utf8');
  }

  // Write a config without exclude section
  const configWithoutExclude = originalConfig
    ? JSON.parse(originalConfig)
    : {};
  delete configWithoutExclude.exclude;
  fs.writeFileSync(projectConfigPath, JSON.stringify(configWithoutExclude, null, 2));

  const output1 = runCli('exclude');
  const test1Passed = output1.includes('No exclude patterns configured');
  addResult('exclude with no patterns — prints guidance', test1Passed, output1);

  // ─── Test 2: exclude with patterns ────────────────────────────────────────
  printInfo('Test 2: exclude command removes matching files from git');

  // Find a tracked ABAP file to use as test target (pick the first .abap file)
  const trackedFiles = gitExecLocal('git ls-files abap/*.clas.abap 2>/dev/null || git ls-files -- "*.clas.abap"')
    .split('\n').filter(Boolean);

  let testFilePassed = false;
  let testFileOutput = 'No tracked .clas.abap files found';

  if (trackedFiles.length > 0) {
    const testFile = trackedFiles[0];
    const baseName = path.basename(testFile);
    const pattern = baseName; // exact match pattern

    // Write config with exclude pattern matching our test file
    const configWithExclude = JSON.parse(originalConfig || '{}');
    configWithExclude.exclude = { patterns: [pattern] };
    fs.writeFileSync(projectConfigPath, JSON.stringify(configWithExclude, null, 2));

    const output2 = runCli('exclude');
    testFileOutput = output2;

    const removedFromGit = output2.includes('Removed') && output2.includes('file');
    const fileStillOnDisk = fs.existsSync(path.join(repoRoot, testFile));

    if (removedFromGit) {
      // Restore: undo the exclude commit so we don't break the repo
      try {
        gitExecLocal('git revert HEAD --no-edit');
        printInfo(`  Restored ${testFile} via git revert`);
      } catch (e) {
        printInfo(`  Warning: could not revert exclude commit: ${e.message}`);
      }
    }

    testFilePassed = removedFromGit;
    addResult('exclude removes matching file from git tracking', testFilePassed, testFileOutput);
    addResult('excluded file still exists on disk (ABAP object untouched)', fileStillOnDisk, testFile);
  } else {
    addResult('exclude removes matching file from git tracking', false, testFileOutput);
    addResult('excluded file still exists on disk (ABAP object untouched)', false, 'no test files found');
  }

  // ─── Test 3: pull --files skips excluded file ──────────────────────────────
  printInfo('Test 3: pull --files skips excluded file with warning');

  if (trackedFiles.length > 0) {
    const testFile = trackedFiles[0];
    const baseName = path.basename(testFile);

    // Set exclude pattern for this file
    const configWithExclude = JSON.parse(originalConfig || '{}');
    configWithExclude.exclude = { patterns: [baseName] };
    fs.writeFileSync(projectConfigPath, JSON.stringify(configWithExclude, null, 2));

    const output3 = runCli('pull', ['--files', testFile, '--conflict-mode', 'ignore']);
    const warnsPrinted = output3.includes('Skipping') && output3.includes('excluded');
    addResult('pull --files warns about excluded file and skips it', warnsPrinted, output3);
  } else {
    addResult('pull --files warns about excluded file and skips it', false, 'no test files found');
  }

  // ─── Restore original config ───────────────────────────────────────────────
  if (originalConfig !== null) {
    fs.writeFileSync(projectConfigPath, originalConfig);
  } else {
    fs.unlinkSync(projectConfigPath);
  }
  printInfo('  Restored .abapgit-agent.json to original state');

  // ─── Summary ───────────────────────────────────────────────────────────────
  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const success = passedCount === results.length;

  return { success, results, duration, passedCount, totalCount: results.length };
}

module.exports = { runExcludeTests };
