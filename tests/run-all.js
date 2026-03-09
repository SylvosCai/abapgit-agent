/**
 * Unified Test Suite
 *
 * Runs all test types:
 * 1. npm test (Jest) - JavaScript unit tests
 * 2. AUnit tests - ABAP test classes
 * 3. Command tests - CLI commands against real ABAP system
 * 4. Lifecycle tests - init, create, import, delete workflow
 * 5. Pull tests - git ref switching (tags/branches) workflow
 * 6. Debug scenarios - REPL and scripted AI (--json) session tests
 *
 * Usage:
 *   npm run test:all              # Run all tests
 *   npm run test:jest             # Jest only
 *   npm run test:aunit            # AUnit only
 *   npm run test:cmd              # Command tests only
 *   npm run test:cmd --demo       # Command tests in demo mode (shows command and output)
 *   npm run test:lifecycle        # Lifecycle tests only
 *   npm run test:pull             # Pull workflow tests only
 *   npm run test:debug:scenarios  # Debug scenarios only (REPL + scripted AI)
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
const { runConflictTests } = require('./integration/conflict-runner');

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
    const command = [testCase.command, ...testCase.args].join(' ');

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

      const args = [testCase.command, ...testCase.args];
      output = execSync(
        `node bin/abapgit-agent ${args.join(' ')}`,
        { cwd: repoRoot, encoding: 'utf8', timeout: 120000 }
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
        printError(`  - ${f.command} ${f.name}`);
        if (f.output) {
          console.log(colors.gray + `    ${f.output.substring(0, 100)}...` + colors.reset);
        }
      }
    }

    return { success: false, results, duration, passedCount, totalCount };
  }
}

/**
 * Run debug scenario tests (bash script — REPL and scripted AI/--json modes)
 */
function runDebugScenarios() {
  printSubHeader('Running Debug Scenarios (REPL + Scripted AI mode)');

  const scriptPath = path.join(__dirname, 'integration', 'debug-scenarios.sh');
  if (!fs.existsSync(scriptPath)) {
    printWarning('debug-scenarios.sh not found — skipping');
    return { success: true, skipped: true, duration: '0.0', passedCount: 0, totalCount: 0 };
  }

  const resultsFile = `${process.env.TMPDIR || '/tmp'}/debug_scenarios_result`;
  if (fs.existsSync(resultsFile)) fs.unlinkSync(resultsFile);

  const startTime = Date.now();

  // Use spawnSync with stdio: 'inherit' so scenario output streams live to the
  // terminal instead of buffering until the script finishes (~60s).
  const result = require('child_process').spawnSync('bash', [scriptPath], {
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
    printSuccess(`Debug Scenarios: ${passedCount}/${totalCount} passed (${duration}s)`);
  } else {
    printError(`Debug Scenarios: ${passedCount}/${totalCount} passed (${duration}s)`);
  }

  return { success, duration, passedCount, totalCount };
}

/**
 * Print final summary
 */
function printSummary(results) {
  printHeader('TEST SUITE SUMMARY');

  let totalDuration = 0;
  let allPassed = true;

  // npm test (Jest)
  if (results.jest) {
    totalDuration += parseFloat(results.jest.duration);
    if (results.jest.success) {
      printSuccess(`npm test (Jest): PASSED (${results.jest.duration}s)`);
    } else {
      printError(`npm test (Jest): FAILED (${results.jest.duration}s)`);
      allPassed = false;
    }
  }

  // AUnit tests
  if (results.aunit) {
    totalDuration += parseFloat(results.aunit.duration);
    if (results.aunit.success) {
      printSuccess(`AUnit Tests: ${results.aunit.passedCount}/${results.aunit.totalCount} classes PASSED (${results.aunit.duration}s)`);
    } else {
      printError(`AUnit Tests: ${results.aunit.passedCount}/${results.aunit.totalCount} classes FAILED (${results.aunit.duration}s)`);
      allPassed = false;
    }
  }

  // Command tests
  if (results.cmd) {
    if (results.cmd.skipped) {
      printWarning(`Command Tests: SKIPPED - ${results.cmd.error}`);
    } else {
      totalDuration += parseFloat(results.cmd.duration);
      if (results.cmd.success) {
        printSuccess(`Command Tests: ${results.cmd.passedCount}/${results.cmd.totalCount} PASSED (${results.cmd.duration}s)`);
      } else {
        printError(`Command Tests: ${results.cmd.passedCount}/${results.cmd.totalCount} FAILED (${results.cmd.duration}s)`);
        allPassed = false;
      }
    }
  }

  // Lifecycle tests
  if (results.lifecycle) {
    totalDuration += parseFloat(results.lifecycle.duration);
    if (results.lifecycle.success) {
      printSuccess(`Lifecycle Tests: ${results.lifecycle.passedCount}/${results.lifecycle.totalCount} PASSED (${results.lifecycle.duration}s)`);
    } else {
      printError(`Lifecycle Tests: ${results.lifecycle.passedCount}/${results.lifecycle.totalCount} FAILED (${results.lifecycle.duration}s)`);
      allPassed = false;
    }
  }

  // Conflict detection tests
  if (results.conflict) {
    totalDuration += parseFloat(results.conflict.duration);
    if (results.conflict.success) {
      printSuccess(`Conflict Detection Tests: ${results.conflict.passedCount}/${results.conflict.totalCount} PASSED (${results.conflict.duration}s)`);
    } else {
      printError(`Conflict Detection Tests: ${results.conflict.passedCount}/${results.conflict.totalCount} FAILED (${results.conflict.duration}s)`);
      allPassed = false;
    }
  }

  // Debug scenario tests
  if (results.debug) {
    if (results.debug.skipped) {
      printWarning('Debug Scenarios: SKIPPED');
    } else {
      totalDuration += parseFloat(results.debug.duration);
      if (results.debug.success) {
        printSuccess(`Debug Scenarios: ${results.debug.passedCount}/${results.debug.totalCount} PASSED (${results.debug.duration}s)`);
      } else {
        printError(`Debug Scenarios: ${results.debug.passedCount}/${results.debug.totalCount} FAILED (${results.debug.duration}s)`);
        allPassed = false;
      }
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

  // Logic: if any specific test type is specified, run ONLY that type
  // Otherwise run all tests
  const hasSpecificTest = args.some(arg => ['--jest', '--aunit', '--cmd', '--lifecycle', '--pull', '--conflict'].includes(arg));

  // Demo mode shows command and output for each test
  const demoMode = args.includes('--demo');

  // Command filter for running tests of specific command only (e.g., --command=syntax)
  const commandFilterArg = args.find(arg => arg.startsWith('--command='));
  const commandFilter = commandFilterArg ? commandFilterArg.split('=')[1] : null;

  let runJest, runAunit, runCmd, runLifecycle, runPull, runConflict, runDebug;

  if (args.includes('--jest')) {
    runJest = true;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runConflict = false;
    runDebug = false;
  } else if (args.includes('--aunit')) {
    runJest = false;
    runAunit = true;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runConflict = false;
    runDebug = false;
  } else if (args.includes('--cmd')) {
    runJest = false;
    runAunit = false;
    runCmd = true;
    runLifecycle = false;
    runPull = false;
    runConflict = false;
    runDebug = false;
  } else if (args.includes('--lifecycle')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = true;
    runPull = false;
    runConflict = false;
    runDebug = false;
  } else if (args.includes('--pull')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = true;
    runConflict = false;
    runDebug = false;
  } else if (args.includes('--conflict')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runConflict = true;
    runDebug = false;
  } else if (args.includes('--debug-scenarios')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runConflict = false;
    runDebug = true;
  } else {
    // Run all tests
    runJest = true;
    runAunit = true;
    runCmd = true;
    runLifecycle = false;  // Lifecycle tests run as part of cmd tests
    runPull = false;       // Pull tests run as part of cmd tests
    runConflict = false;   // Conflict tests run standalone (stateful, sequential)
    runDebug = true;
  }

  printHeader('UNIFIED TEST SUITE');

  const results = {};

  // Run Jest tests
  if (runJest) {
    results.jest = runJestTests();
  }

  // Run AUnit tests
  if (runAunit) {
    results.aunit = runAUnitTestsWrapper();
  }

  // Run Command tests
  if (runCmd) {
    results.cmd = await runCommandTests(demoMode, commandFilter);
  }

  // Run Lifecycle tests only
  if (runLifecycle) {
    printSubHeader('Running Lifecycle Tests Only');
    const lifecycleResults = runLifecycleTestsWrapper();
    results.lifecycle = lifecycleResults;
  }

  // Run Pull tests only
  if (runPull) {
    printSubHeader('Running Pull Tests Only');
    const pullResults = runPullTestsWrapper();
    results.pull = pullResults;
  }

  // Run Conflict detection tests only
  if (runConflict) {
    printSubHeader('Running Conflict Detection Tests Only');
    const conflictResults = runConflictTestsWrapper();
    results.conflict = conflictResults;
  }

  // Run Debug scenario tests (REPL + scripted AI/--json)
  if (runDebug) {
    results.debug = runDebugScenarios();
  }

  // Print summary and exit with appropriate code
  const allPassed = printSummary(results);

  if (!allPassed) {
    process.exit(1);
  }
}

main();
