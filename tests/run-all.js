/**
 * Unified Test Suite
 *
 * Runs all test types:
 * 1. Setup phase - clone test repos, register in ABAP, activate test objects (auto, idempotent)
 * 2. npm test (Jest) - JavaScript unit tests
 * 3. AUnit tests - ABAP test classes
 * 4. Command tests - CLI commands against real ABAP system
 * 5. Lifecycle tests - init, create, import, delete workflow
 * 6. Pull tests - git ref switching (tags/branches) workflow
 * 7. Drop tests - drop command end-to-end (drop + re-pull per object type)
 * 8. Debug scenarios - REPL and scripted AI (--json) session tests
 *
 * Usage:
 *   npm run test:all              # Run all tests (setup phase runs automatically)
 *   npm run test:setup            # Run only the setup phase
 *   npm run test:all -- --no-setup  # Skip auto-setup (manage prerequisites manually)
 *   npm run test:jest             # Jest only
 *   npm run test:aunit            # AUnit only
 *   npm run test:cmd              # Command tests only
 *   npm run test:cmd --demo       # Command tests in demo mode (shows command and output)
 *   npm run test:lifecycle        # Lifecycle tests only
 *   npm run test:pull             # Pull workflow tests only
 *   npm run test:drop             # Drop command tests only
 *   npm run test:debug:scenarios  # Debug scenarios only (REPL + scripted AI + variable inspection)
 */

const { execSync, spawn } = require('child_process');
const path = require('path');
const fs = require('fs');
const readline = require('readline');

const repoRoot = path.join(__dirname, '..');

// Import test runners from integration folder
const { runAUnitTests } = require('./integration/aunit-runner');
const { runLifecycleTests } = require('./integration/lifecycle-runner');
const { runPullTests } = require('./integration/pull-runner');
const { runFullPullTests } = require('./integration/pull-full-runner');
const { runConflictTests } = require('./integration/conflict-runner');
const { runSyncXmlTests } = require('./integration/sync-xml-runner');
const { runXmlOnlyTests } = require('./integration/xml-only-runner');
const { runAsyncPullTests } = require('./integration/async-pull-runner');
const { runJUnitTests } = require('./integration/junit-runner');
const { runDebugTests } = require('./integration/debug-runner');
const { runDropTests } = require('./integration/drop-runner');
const { runCustomizeTests } = require('./integration/customize-runner');
const { runExcludeTests } = require('./integration/exclude-runner');
const { runHanaTests } = require('./integration/hana-runner');
const { runDtelScrlenTests } = require('./integration/dtel-scrlen-runner');
const { runSetup } = require('./integration/setup-runner');

// Colors for output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  cyan: '\x1b[36m',
  gray: '\x1b[90m'
};

function colorize(color, text) {
  return `${colors[color]}${text}${colors.reset}`;
}

function printHeader(text) {
  console.log('\n' + '='.repeat(70));
  console.log(colorize('bright', `  ${text}`));
  console.log('='.repeat(70));
}

function printSubHeader(text) {
  console.log('\n' + '-'.repeat(70));
  console.log(colorize('cyan', `  ${text}`));
  console.log('-'.repeat(70));
}

function printSuccess(text) {
  console.log(colorize('green', `  ✅ ${text}`));
}

function printError(text) {
  console.log(colorize('red', `  ❌ ${text}`));
}

function printWarning(text) {
  console.log(colorize('yellow', `  ⚠️  ${text}`));
}

function printInfo(text) {
  console.log(`  ${text}`);
}

/**
 * Run Jest tests (npm test)
 */
function runJestTests() {
  printSubHeader('Running npm test (Jest)');

  const startTime = Date.now();

  try {
    execSync('npm test', {
      cwd: repoRoot,
      stdio: 'inherit'
    });

    const duration = ((Date.now() - startTime) / 1000).toFixed(1);
    printSuccess(`Jest tests passed (${duration}s)`);
    return { success: true, duration, error: null };
  } catch (error) {
    const duration = ((Date.now() - startTime) / 1000).toFixed(1);
    printError(`Jest tests failed (${duration}s)`);
    return { success: false, duration, error: error.message };
  }
}

/**
 * Wrapper function for AUnit tests
 */
function runAUnitTestsWrapper() {
  return runAUnitTests(repoRoot, {
    printSubHeader,
    printSuccess,
    printError,
    printWarning
  });
}

// Import command test cases from integration folder
const commandTestCases = require('./integration/abap-commands');

/**
 * Wrapper function for lifecycle tests
 */
function runLifecycleTestsWrapper() {
  return runLifecycleTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

/**
 * Wrapper function for pull tests
 */
function runPullTestsWrapper() {
  return runPullTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

/**
 * Wrapper function for full pull tests (no --files)
 */
function runFullPullTestsWrapper() {
  return runFullPullTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

function runConflictTestsWrapper() {
  return runConflictTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

function runSyncXmlTestsWrapper() {
  return runSyncXmlTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

function runXmlOnlyTestsWrapper() {
  return runXmlOnlyTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize
  });
}

function runAsyncPullTestsWrapper() {
  return runAsyncPullTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize
  });
}

function runJUnitTestsWrapper() {
  return runJUnitTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

function runDebugTestsWrapper() {
  return runDebugTests(repoRoot, {
    printSubHeader,
    printSuccess,
    printError,
    printWarning,
    printInfo,
    colorize,
    colors
  });
}

function runDropTestsWrapper() {
  return runDropTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

function runCustomizeTestsWrapper() {
  return runCustomizeTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

function runHanaTestsWrapper() {
  return runHanaTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

function runDtelScrlenTestsWrapper() {
  return runDtelScrlenTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

function runExcludeTestsWrapper() {
  return runExcludeTests(repoRoot, {
    printSubHeader,
    printInfo,
    printSuccess,
    printError,
    colorize,
    colors
  });
}

/**
 * Pause and wait for user input
 * Press 'a' to skip all remaining pauses
 * @param {string} message - Message to display
 * @returns {Promise<void>}
 */
let skipRemainingPauses = false;

function pause(message = '  Press any key to continue (or "a" to skip all)...') {
  // If already set to skip, resolve immediately
  if (skipRemainingPauses) {
    return Promise.resolve();
  }

  return new Promise((resolve) => {
    // Print the message first
    process.stdout.write(colorize('yellow', message));

    // Set raw mode to capture individual keypresses
    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });

    // Use a one-time keypress listener for the 'a' key
    const onKeyPress = (char) => {
      if (char === 'a' || char === 'A') {
        skipRemainingPauses = true;
        console.log(colorize('cyan', '  → Skipping all remaining pauses'));
      }
      // Remove listener after any key press
      process.stdin.removeListener('data', onKeyPress);
      // Restore normal mode and close
      process.stdin.setRawMode(false);
      rl.close();
      resolve();
    };

    // Enable raw mode to capture keypress
    process.stdin.setRawMode(true);
    process.stdin.resume();
    process.stdin.setEncoding('utf8');

    // Listen for keypress
    process.stdin.on('data', onKeyPress);
  });
}

/**
 * Run command in demo mode - shows command and streams output in real-time
 * @param {string} command - The command to run
 * @param {string} testName - Display name for the test
 * @param {Function} verify - Verification function
 * @returns {Promise<{passed: boolean, output: string}>}
 */
function runDemoCommand(command, testName, verify, shouldPause = true) {
  return new Promise(async (resolve) => {
    // Print command banner
    console.log('\n' + '='.repeat(70));
    console.log(colorize('cyan', `  ▶ abapgit-agent ${command}`));
    console.log('='.repeat(70));
    console.log('');

    const child = spawn('node', ['bin/abapgit-agent', ...command.split(' ')], {
      cwd: repoRoot,
      shell: true,
      timeout: 120000
    });

    let output = '';

    child.stdout.on('data', (data) => {
      const text = data.toString();
      process.stdout.write(text);
      output += text;
    });

    child.stderr.on('data', (data) => {
      const text = data.toString();
      process.stderr.write(text);
      output += text;
    });

    child.on('close', async (code) => {
      console.log(''); // Add newline after output

      // Verify result
      let commandPassed = false;
      if (verify) {
        commandPassed = verify(output);
      } else {
        commandPassed = code === 0;
      }

      // Print result
      if (commandPassed) {
        console.log(colorize('green', '  ✅ PASSED'));
      } else {
        console.log(colorize('red', '  ❌ FAILED'));
      }

      // Pause for user to review output
      if (shouldPause) {
        await pause();
      }

      resolve({ passed: commandPassed, output });
    });

    child.on('error', async (error) => {
      console.error(colorize('red', `  ❌ ERROR: ${error.message}`));
      if (shouldPause) {
        await pause();
      }
      resolve({ passed: false, output: error.message });
    });
  });
}

/**
 * Run demo mode command tests (async)
 */
async function runDemoCommandTests(testCases, startTime) {
  const results = [];

  for (const testCase of testCases) {
    const command = [testCase.run || testCase.command, ...testCase.args].join(' ');

    const result = await runDemoCommand(command, testCase.name, testCase.verify);
    results.push({ ...testCase, passed: result.passed, output: result.output.substring(0, 200) });
  }

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  if (passedCount === totalCount) {
    printSuccess(`Command tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`Command tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: false, results, duration, passedCount, totalCount };
  }
}

/**
 * Run command tests against real ABAP system
 * @param {boolean} demoMode - If true, show command and output for each test
 * @param {string} commandFilter - Optional command name to filter tests (e.g., 'syntax', 'pull')
 */
function runCommandTests(demoMode = false, commandFilter = null) {
  printSubHeader('Running Command Tests (Real ABAP System)' + (demoMode ? ' [DEMO MODE]' : ''));

  // Filter test cases by command if specified
  const filteredCases = commandFilter
    ? commandTestCases.filter(tc => tc.command === commandFilter)
    : commandTestCases;

  if (commandFilter && filteredCases.length === 0) {
    printWarning(`No tests found for command: ${commandFilter}`);
    return { success: true, results: [], duration: '0.0', passedCount: 0, totalCount: 0 };
  }

  if (commandFilter) {
    printInfo(`  Filtering tests for command: ${commandFilter}`);
  }

  const startTime = Date.now();
  const results = [];

  // Check if ABAP is configured
  const configPath = path.join(repoRoot, '.abapGitAgent');
  if (!fs.existsSync(configPath)) {
    printWarning('.abapGitAgent not found - checking if ABAP is configured via env vars');

    if (!process.env.ABAP_HOST || !process.env.ABAP_USER) {
      printError('ABAP not configured. Skipping command tests.');
      printInfo('Configure via .abapGitAgent or environment variables:');
      printInfo('  ABAP_HOST, ABAP_USER, ABAP_PASSWORD, etc.');
      return {
        success: false,
        results: [],
        duration: '0.0',
        skipped: true,
        error: 'ABAP not configured'
      };
    }
  }

  // Use synchronous execution for non-demo mode, async for demo mode
  if (demoMode) {
    return runDemoCommandTests(filteredCases, startTime);
  }

  // Original synchronous implementation
  for (const testCase of filteredCases) {
    process.stdout.write(`  Testing: ${testCase.command} ${testCase.name}... `);

    let commandPassed = false;
    let output = '';

    try {
      // Run setup if provided
      if (testCase.setup) {
        testCase.setup();
      }

      const args = [testCase.run || testCase.command, ...testCase.args];
      const execCwd = testCase.cwd || repoRoot;
      const agentBin = path.join(repoRoot, 'bin', 'abapgit-agent');
      output = execSync(
        `node ${agentBin} ${args.join(' ')}`,
        { cwd: execCwd, encoding: 'utf8', timeout: 120000 }
      );

      // Run custom verification if provided
      if (testCase.verify) {
        commandPassed = testCase.verify(output);
      } else {
        // Fallback to basic pattern matching
        const successPatterns = [
          'completed successfully',
          'passed',
          'SUCCESS',
          'OK',
          'Retrieved',
          'ENABLED',
          'is healthy',
          'All tests passed',
          'Syntax check passed',
          'Objects in',
          'Package Tree',
          'Viewing'
        ];
        commandPassed = testCase.expectSuccess
          ? successPatterns.some(p => output.includes(p) || output.toLowerCase().includes(p.toLowerCase()))
          : output.toLowerCase().includes('failed') || output.toLowerCase().includes('error');
      }
    } catch (error) {
      output = error.stdout || error.message;
      // If command crashed, it's a failure (unless we expect failure)
      commandPassed = !testCase.expectSuccess;
    } finally {
      // Run cleanup if provided
      if (testCase.cleanup) {
        try {
          testCase.cleanup();
        } catch (cleanupError) {
          // Ignore cleanup errors
        }
      }
    }

    if (commandPassed) {
      console.log(colorize('green', '✅'));
      results.push({ ...testCase, passed: true, output: '' });
    } else {
      console.log(colorize('red', '❌'));
      results.push({ ...testCase, passed: false, output: output.substring(0, 200) });
    }
  }

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  let passedCount = results.filter(r => r.passed).length;
  let totalCount = results.length;

  // Run lifecycle tests (init, create, import, delete) ONLY if no command filter is specified
  if (!commandFilter) {
    const lifecycleResults = runLifecycleTestsWrapper();
    if (lifecycleResults && !lifecycleResults.skipped && lifecycleResults.results) {
      // Merge lifecycle results into command test results
      results.push(...lifecycleResults.results);
      passedCount += lifecycleResults.passedCount || 0;
      totalCount += lifecycleResults.totalCount || 0;
      printInfo(`  (Including ${lifecycleResults.passedCount || 0}/${lifecycleResults.totalCount || 0} lifecycle tests)`);
    }

    // Run pull workflow tests (git ref switching with verification)
    const pullResults = runPullTestsWrapper();
    if (pullResults && !pullResults.skipped && pullResults.results) {
      // Merge pull results into command test results
      results.push(...pullResults.results);
      passedCount += pullResults.passedCount || 0;
      totalCount += pullResults.totalCount || 0;
      printInfo(`  (Including ${pullResults.passedCount || 0}/${pullResults.totalCount || 0} pull workflow tests)`);
    }

    // Run full pull tests (no --files — exercises full-pull path + conflict detection)
    const fullPullResults = runFullPullTestsWrapper();
    if (fullPullResults && !fullPullResults.skipped && fullPullResults.results) {
      results.push(...fullPullResults.results);
      passedCount += fullPullResults.passedCount || 0;
      totalCount += fullPullResults.totalCount || 0;
      printInfo(`  (Including ${fullPullResults.passedCount || 0}/${fullPullResults.totalCount || 0} full pull tests)`);
    }

    // Run --sync-xml integration tests (end-to-end: detect mismatch → rewrite → re-pull)
    const syncXmlResults = runSyncXmlTestsWrapper();
    if (syncXmlResults && !syncXmlResults.skipped && syncXmlResults.results) {
      results.push(...syncXmlResults.results);
      passedCount += syncXmlResults.passedCount || 0;
      totalCount += syncXmlResults.totalCount || 0;
      printInfo(`  (Including ${syncXmlResults.passedCount || 0}/${syncXmlResults.totalCount || 0} sync-xml tests)`);
    }

    // Run XML-only object pull tests (TABL, DTEL, TTYP via --files name.type.xml)
    const xmlOnlyResults = runXmlOnlyTestsWrapper();
    if (xmlOnlyResults && !xmlOnlyResults.skipped && xmlOnlyResults.results) {
      results.push(...xmlOnlyResults.results);
      passedCount += xmlOnlyResults.passedCount || 0;
      totalCount += xmlOnlyResults.totalCount || 0;
      printInfo(`  (Including ${xmlOnlyResults.passedCount || 0}/${xmlOnlyResults.totalCount || 0} xml-only tests)`);
    }

    // Run async pull tests (>10 files → background job path)
    const asyncPullResults = runAsyncPullTestsWrapper();
    if (asyncPullResults && !asyncPullResults.skipped && asyncPullResults.results) {
      results.push(...asyncPullResults.results);
      passedCount += asyncPullResults.passedCount || 0;
      totalCount += asyncPullResults.totalCount || 0;
      printInfo(`  (Including ${asyncPullResults.passedCount || 0}/${asyncPullResults.totalCount || 0} async-pull tests)`);
    }
  }

  if (passedCount === totalCount) {
    printSuccess(`Command tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`Command tests: ${passedCount}/${totalCount} passed (${duration}s)`);

    // Show failed tests
    const failed = results.filter(r => !r.passed);
    if (failed.length > 0) {
      console.log('\nFailed tests:');
      for (const f of failed) {
        printError(`  - ${[f.command, f.name].filter(Boolean).join(' ')}`);
        if (f.output) {
          console.log(colors.gray + `    ${f.output.substring(0, 100)}...` + colors.reset);
        }
      }
    }

    return { success: false, results, duration, passedCount, totalCount };
  }
}

/**
 * Run REPL debug scenario tests (bash script — scenarios 1 & 2 only, require interactive TTY)
 */
function runReplDebugScenarios() {
  const label = 'Debug Scenarios (REPL mode — 1/2)';
  printSubHeader(`Running ${label}`);

  const scriptPath = path.join(__dirname, 'integration', 'debug-repl-scenarios.sh');
  if (!fs.existsSync(scriptPath)) {
    printWarning('debug-repl-scenarios.sh not found — skipping');
    return { success: true, skipped: true, duration: '0.0', passedCount: 0, totalCount: 0 };
  }

  const resultsFile = `${process.env.TMPDIR || '/tmp'}/debug_scenarios_result`;
  if (fs.existsSync(resultsFile)) fs.unlinkSync(resultsFile);

  const startTime = Date.now();

  // Use spawnSync with stdio: 'inherit' so scenario output streams live to the
  // terminal instead of buffering. bash is required for REPL scenarios (FIFOs, [[ ]]).
  const result = require('child_process').spawnSync('bash', [scriptPath, 'repl'], {
    cwd: repoRoot,
    stdio: 'inherit',
    timeout: 300000  // 5 minutes — scenarios involve real ABAP round-trips
  });

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const success = result.status === 0;

  // Read pass/fail counts written by the bash script's cleanup trap
  let passedCount = 0;
  let totalCount = 0;
  if (fs.existsSync(resultsFile)) {
    const [p, f] = fs.readFileSync(resultsFile, 'utf8').trim().split(' ').map(Number);
    passedCount = p || 0;
    totalCount = (p || 0) + (f || 0);
    fs.unlinkSync(resultsFile);
  }

  if (success) {
    printSuccess(`${label}: ${passedCount}/${totalCount} passed (${duration}s)`);
  } else {
    printError(`${label}: ${passedCount}/${totalCount} passed (${duration}s)`);
  }

  return { success, duration, passedCount, totalCount };
}

/**
 * Run scripted debug scenario tests (bash script — scenarios 3/4/5, CI-safe)
 */
function runScriptedDebugScenarios() {
  const label = 'Debug Scenarios (Scripted AI mode — 3/4/5)';
  printSubHeader(`Running ${label}`);

  const scriptPath = path.join(__dirname, 'integration', 'debug-scripted-scenarios.sh');
  if (!fs.existsSync(scriptPath)) {
    printWarning('debug-scripted-scenarios.sh not found — skipping');
    return { success: true, skipped: true, duration: '0.0', passedCount: 0, totalCount: 0 };
  }

  const resultsFile = `${process.env.TMPDIR || '/tmp'}/debug_scripted_result`;
  if (fs.existsSync(resultsFile)) fs.unlinkSync(resultsFile);

  const startTime = Date.now();

  const result = require('child_process').spawnSync('bash', [scriptPath, 'all'], {
    cwd: repoRoot,
    stdio: 'inherit',
    timeout: 600000  // 10 minutes — 3 scenarios with sleeps between them
  });

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const success = result.status === 0;

  let passedCount = 0;
  let totalCount = 0;
  if (fs.existsSync(resultsFile)) {
    const [p, f] = fs.readFileSync(resultsFile, 'utf8').trim().split(' ').map(Number);
    passedCount = p || 0;
    totalCount = (p || 0) + (f || 0);
    fs.unlinkSync(resultsFile);
  }

  if (success) {
    printSuccess(`${label}: ${passedCount}/${totalCount} passed (${duration}s)`);
  } else {
    printError(`${label}: ${passedCount}/${totalCount} passed (${duration}s)`);
  }

  return { success, duration, passedCount, totalCount };
}


/**
 * Registry of every integration test suite this runner can dispatch.
 *
 * Each entry owns a suite's identity: CLI flag, summary label, wrapper, CI default,
 * and any per-suite quirks (async, optional sub-header, cooldowns). Adding a new
 * suite is one row in this array — main()'s selection / setup-gate / execution /
 * summary loops all read from here.
 *
 *   key            — property name in the `results` object
 *   flag           — CLI flag (e.g. '--cmd'); matched against process.argv
 *   label          — printed in the summary line; should read naturally as a prefix
 *                    before "PASSED" / "FAILED" / "SKIPPED"
 *   run            — wrapper function; receives `ctx = { demoMode, commandFilter }`
 *   ciDefault      — true / false / 'localOnly' (= !isCI). Used only when the user
 *                    passed no suite flags; explicit flags override defaults.
 *   needsAbap      — when true, triggers the runSetup() phase before dispatch.
 *                    runSetup clones / verifies all test repos and (re-)activates
 *                    their ABAP objects. Suites with their own clone+setup logic
 *                    (e.g. hana, dtel-scrlen, exclude) set this false — they
 *                    manage their own preconditions inside the runner.
 *   async          — true when the wrapper returns a Promise; runner will await.
 *   subHeader      — optional banner printed before invocation (mirrors the legacy
 *                    `printSubHeader('Running X Only')` lines).
 *   cooldownAfter  — when set, sleeps 15s before this suite runs if the named
 *                    suite also ran in this invocation. Currently only `drop` uses
 *                    this (waits for the EZABAPGIT lock to drain after `cmd`).
 */
const SUITES = [
  { key: 'jest',           flag: '--jest',           label: 'npm test (Jest)',                       run: () => runJestTests(),                                                ciDefault: true                              },
  { key: 'aunit',          flag: '--aunit',          label: 'AUnit Tests',                           run: () => runAUnitTestsWrapper(),                                        ciDefault: true,        needsAbap: true     },
  { key: 'cmd',            flag: '--cmd',            label: 'Command Tests',                         run: (ctx) => runCommandTests(ctx.demoMode, ctx.commandFilter),           ciDefault: true,        needsAbap: true, async: true },
  { key: 'lifecycle',      flag: '--lifecycle',      label: 'Lifecycle Tests',                       run: () => runLifecycleTestsWrapper(),                                    ciDefault: false,       needsAbap: true,    subHeader: 'Running Lifecycle Tests Only' },
  { key: 'pull',           flag: '--pull',           label: 'Pull Tests',                            run: () => runPullTestsWrapper(),                                         ciDefault: false,       needsAbap: true,    subHeader: 'Running Pull Tests Only' },
  { key: 'fullPull',       flag: '--full-pull',      label: 'Full Pull Tests',                       run: () => runFullPullTestsWrapper(),                                     ciDefault: false,       needsAbap: true,    subHeader: 'Running Full Pull Tests Only (no --files)' },
  { key: 'conflict',       flag: '--conflict',       label: 'Conflict Detection Tests',              run: () => runConflictTestsWrapper(),                                     ciDefault: false,       needsAbap: true,    subHeader: 'Running Conflict Detection Tests Only' },
  { key: 'syncXml',        flag: '--sync-xml',       label: '--sync-xml Integration Tests',          run: () => runSyncXmlTestsWrapper(),                                      ciDefault: false,       needsAbap: true,    subHeader: 'Running --sync-xml Integration Tests Only' },
  { key: 'xmlOnly',        flag: '--xml-only',       label: 'XML-Only Object Pull Tests',            run: () => runXmlOnlyTestsWrapper(),                                      ciDefault: false,       needsAbap: true,    subHeader: 'Running XML-Only Object Pull Tests Only' },
  { key: 'junit',          flag: '--junit',          label: 'JUnit Output Tests',                    run: () => runJUnitTestsWrapper(),                                        ciDefault: true,        needsAbap: true     },
  { key: 'debugBp',        flag: '--debug',          label: 'Debug Breakpoint Tests',                run: () => runDebugTestsWrapper(),                                        ciDefault: true,        needsAbap: true     },
  { key: 'drop',           flag: '--drop',           label: 'Drop Tests',                            run: () => runDropTestsWrapper(),                                         ciDefault: true,        needsAbap: true,    cooldownAfter: 'cmd' },
  { key: 'customize',      flag: '--customize',      label: 'Customize Tests',                       run: () => runCustomizeTestsWrapper(),                                    ciDefault: true,        needsAbap: true     },
  { key: 'asyncPull',      flag: '--async-pull',     label: 'Async Pull Tests',                      run: () => runAsyncPullTestsWrapper(),                                    ciDefault: false,       needsAbap: true     },
  { key: 'exclude',        flag: '--exclude',        label: 'Exclude Tests',                         run: () => runExcludeTestsWrapper(),                                      ciDefault: true                              },
  { key: 'hana',           flag: '--hana',           label: 'HANA Tests',                            run: () => runHanaTestsWrapper(),                                         ciDefault: true                              },
  { key: 'dtelScrlen',     flag: '--dtel-scrlen',    label: 'DTEL SCRLEN Tests',                     run: () => runDtelScrlenTestsWrapper(),                                   ciDefault: true                              },
  { key: 'debugScripted',  flag: '--debug-scripted', label: 'Debug Scenarios (Scripted)',            run: () => runScriptedDebugScenarios(),                                   ciDefault: 'localOnly', needsAbap: true     },
  { key: 'debugRepl',      flag: '--debug-repl',     label: 'Debug Scenarios (REPL)',                run: () => runReplDebugScenarios(),                                       ciDefault: 'localOnly', needsAbap: true     },
];


function printSummary(results) {
  printHeader('TEST SUITE SUMMARY');

  let totalDuration = 0;
  let allPassed = true;

  // Walk SUITES in declaration order so summary output preserves the
  // (intentionally-ordered) sequence developers and CI logs are used to.
  for (const suite of SUITES) {
    const r = results[suite.key];
    if (!r) continue;

    if (r.skipped) {
      // Some runners attach an explanatory `error` field to a skip; surface it.
      printWarning(`${suite.label}: SKIPPED${r.error ? ` - ${r.error}` : ''}`);
      continue;
    }

    if (r.duration != null) totalDuration += parseFloat(r.duration);

    // Jest doesn't expose passed/total counts; suppress when missing so the line
    // reads `npm test (Jest): PASSED (12.3s)` rather than `/<undef> PASSED`.
    const counts = (r.passedCount != null && r.totalCount != null)
      ? `${r.passedCount}/${r.totalCount} `
      : '';
    const durationStr = r.duration != null ? ` (${r.duration}s)` : '';

    if (r.success) {
      printSuccess(`${suite.label}: ${counts}PASSED${durationStr}`);
    } else {
      printError(`${suite.label}: ${counts}FAILED${durationStr}`);
      allPassed = false;
    }
  }

  console.log('\n' + '='.repeat(70));
  if (allPassed) {
    console.log(colorize('bright', colorize('green', `  ✅ ALL TESTS PASSED (Total: ${totalDuration.toFixed(1)}s)`)));
  } else {
    console.log(colorize('bright', colorize('red', `  ❌ SOME TESTS FAILED (Total: ${totalDuration.toFixed(1)}s)`)));
  }
  console.log('='.repeat(70) + '\n');

  return allPassed;
}

/**
 * Main function
 */
async function main() {
  const args = process.argv.slice(2);

  // --setup: run only the setup phase (clone repos, register in ABAP, activate objects)
  if (args.includes('--setup')) {
    printHeader('INTEGRATION TEST SETUP');
    const setupResult = runSetup(repoRoot, {
      printSubHeader, printInfo, printSuccess, printError, printWarning, colorize
    });
    process.exit(setupResult.success ? 0 : 1);
    return;
  }

  // Logic: if any specific --<flag> is passed, run ONLY matching suite(s).
  // Otherwise run the CI-default set declared in SUITES.

  // Demo mode shows command and output for each test (only meaningful for `cmd`).
  const demoMode = args.includes('--demo');

  // Command filter for running tests of a specific command only (e.g., --command=syntax)
  const commandFilterArg = args.find(arg => arg.startsWith('--command='));
  const commandFilter = commandFilterArg ? commandFilterArg.split('=')[1] : null;

  // Selection: explicit --flag(s) win; otherwise run the CI-default set.
  // SUITES is declared at module scope; see the comment block above its definition.
  const isCI = !!(process.env.BUILD_NUMBER || process.env.CI || process.env.GITHUB_ACTIONS);
  const resolveDefault = (s) =>
    s.ciDefault === 'localOnly' ? !isCI : !!s.ciDefault;

  const explicit = SUITES.filter(s => args.includes(s.flag));
  const toRun = explicit.length > 0 ? explicit : SUITES.filter(resolveDefault);
  const toRunKeys = new Set(toRun.map(s => s.key));

  // Surface non-trivial CI defaulting once, like the legacy code did.
  if (explicit.length === 0 && isCI) {
    const dropped = SUITES
      .filter(s => s.ciDefault === 'localOnly')
      .map(s => s.key);
    if (dropped.length > 0) {
      printInfo(`  ⚠️  CI environment detected — skipping local-only suites: ${dropped.join(', ')}`);
    }
  }

  printHeader('UNIFIED TEST SUITE');

  const results = {};

  // Run setup phase before any ABAP-dependent integration tests.
  // Skipped when running Jest-only (no ABAP connection needed).
  // --no-setup flag disables auto-setup for developers who manage prerequisites manually.
  const needsAbap = toRun.some(s => s.needsAbap);
  const skipAutoSetup = args.includes('--no-setup');
  if (needsAbap && !skipAutoSetup) {
    const setupResult = runSetup(repoRoot, {
      printSubHeader, printInfo, printSuccess, printError, printWarning, colorize
    });
    if (!setupResult.success && setupResult.failedCount > 0) {
      printError('  Setup failed — aborting test run. Fix the setup errors above and retry.');
      printInfo('  Tip: run `npm run test:setup` standalone to diagnose.');
      process.exit(1);
    }
    printInfo('');
  }

  // Execute selected suites in declaration order.
  // - Honors `subHeader` (printed before invocation).
  // - Honors `cooldownAfter` (15s pause when the named suite also ran this invocation —
  //   currently only `drop` after `cmd`, waiting for the EZABAPGIT lock to drain).
  // - Honors `async: true` for runners that return a Promise (only `cmd` today).
  const ctx = { demoMode, commandFilter };
  for (const suite of toRun) {
    if (suite.cooldownAfter && toRunKeys.has(suite.cooldownAfter)) {
      printInfo(`  Cooling down 15s after ${suite.cooldownAfter} tests before ${suite.key} tests...`);
      await new Promise(r => setTimeout(r, 15000));
    }
    if (suite.subHeader) printSubHeader(suite.subHeader);
    results[suite.key] = suite.async ? await suite.run(ctx) : suite.run(ctx);
  }

  // Print summary and exit with appropriate code
  const allPassed = printSummary(results);

  if (!allPassed) {
    process.exit(1);
  }
}

main();
