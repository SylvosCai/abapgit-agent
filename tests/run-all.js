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
const { runJUnitTests } = require('./integration/junit-runner');
const { runDebugTests } = require('./integration/debug-runner');
const { runDropTests } = require('./integration/drop-runner');
const { runCustomizeTests } = require('./integration/customize-runner');
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

  // Full pull tests (no --files)
  if (results.fullPull) {
    totalDuration += parseFloat(results.fullPull.duration);
    if (results.fullPull.success) {
      printSuccess(`Full Pull Tests: ${results.fullPull.passedCount}/${results.fullPull.totalCount} PASSED (${results.fullPull.duration}s)`);
    } else {
      printError(`Full Pull Tests: ${results.fullPull.passedCount}/${results.fullPull.totalCount} FAILED (${results.fullPull.duration}s)`);
      allPassed = false;
    }
  }

  // JUnit output tests
  if (results.junit) {
    if (results.junit.skipped) {
      printWarning('JUnit Output Tests: SKIPPED - ABAP not configured');
    } else {
      totalDuration += parseFloat(results.junit.duration);
      if (results.junit.success) {
        printSuccess(`JUnit Output Tests: ${results.junit.passedCount}/${results.junit.totalCount} PASSED (${results.junit.duration}s)`);
      } else {
        printError(`JUnit Output Tests: ${results.junit.passedCount}/${results.junit.totalCount} FAILED (${results.junit.duration}s)`);
        allPassed = false;
      }
    }
  }

  // Debug breakpoint tests
  if (results.debugBp) {
    if (results.debugBp.skipped) {
      printWarning('Debug Breakpoint Tests: SKIPPED (abgagt-debug-test repo not found)');
    } else {
      totalDuration += parseFloat(results.debugBp.duration);
      if (results.debugBp.success) {
        printSuccess(`Debug Breakpoint Tests: ${results.debugBp.passedCount}/${results.debugBp.totalCount} PASSED (${results.debugBp.duration}s)`);
      } else {
        printError(`Debug Breakpoint Tests: ${results.debugBp.passedCount}/${results.debugBp.totalCount} FAILED (${results.debugBp.duration}s)`);
        allPassed = false;
      }
    }
  }

  // Drop command tests
  if (results.drop) {
    if (results.drop.skipped) {
      printWarning('Drop Tests: SKIPPED');
    } else {
      totalDuration += parseFloat(results.drop.duration);
      if (results.drop.success) {
        printSuccess(`Drop Tests: ${results.drop.passedCount}/${results.drop.totalCount} PASSED (${results.drop.duration}s)`);
      } else {
        printError(`Drop Tests: ${results.drop.passedCount}/${results.drop.totalCount} FAILED (${results.drop.duration}s)`);
        allPassed = false;
      }
    }
  }

  // Customize command tests
  if (results.customize) {
    if (results.customize.skipped) {
      printWarning('Customize Tests: SKIPPED');
    } else {
      totalDuration += parseFloat(results.customize.duration);
      if (results.customize.success) {
        printSuccess(`Customize Tests: ${results.customize.passedCount}/${results.customize.totalCount} PASSED (${results.customize.duration}s)`);
      } else {
        printError(`Customize Tests: ${results.customize.passedCount}/${results.customize.totalCount} FAILED (${results.customize.duration}s)`);
        allPassed = false;
      }
    }
  }

  // Scripted debug scenarios (3/4/5) — Node.js runner, CI-safe
  if (results.debugScripted) {
    if (results.debugScripted.skipped) {
      printWarning('Debug Scenarios (Scripted): SKIPPED');
    } else {
      totalDuration += parseFloat(results.debugScripted.duration);
      if (results.debugScripted.success) {
        printSuccess(`Debug Scenarios (Scripted): ${results.debugScripted.passedCount}/${results.debugScripted.totalCount} PASSED (${results.debugScripted.duration}s)`);
      } else {
        printError(`Debug Scenarios (Scripted): ${results.debugScripted.passedCount}/${results.debugScripted.totalCount} FAILED (${results.debugScripted.duration}s)`);
        allPassed = false;
      }
    }
  }

  // REPL debug scenarios (1/2) — bash script, local dev only
  if (results.debugRepl) {
    if (results.debugRepl.skipped) {
      printWarning('Debug Scenarios (REPL): SKIPPED');
    } else {
      totalDuration += parseFloat(results.debugRepl.duration);
      if (results.debugRepl.success) {
        printSuccess(`Debug Scenarios (REPL): ${results.debugRepl.passedCount}/${results.debugRepl.totalCount} PASSED (${results.debugRepl.duration}s)`);
      } else {
        printError(`Debug Scenarios (REPL): ${results.debugRepl.passedCount}/${results.debugRepl.totalCount} FAILED (${results.debugRepl.duration}s)`);
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

  // --setup: run only the setup phase (clone repos, register in ABAP, activate objects)
  if (args.includes('--setup')) {
    printHeader('INTEGRATION TEST SETUP');
    const setupResult = runSetup(repoRoot, {
      printSubHeader, printInfo, printSuccess, printError, printWarning, colorize
    });
    process.exit(setupResult.success ? 0 : 1);
    return;
  }

  // Logic: if any specific test type is specified, run ONLY that type
  // Otherwise run all tests
  const hasSpecificTest = args.some(arg => ['--jest', '--aunit', '--cmd', '--lifecycle', '--pull', '--full-pull', '--conflict', '--sync-xml', '--xml-only', '--junit', '--debug', '--debug-scripted', '--debug-repl', '--drop', '--customize'].includes(arg));

  // Demo mode shows command and output for each test
  const demoMode = args.includes('--demo');

  // Command filter for running tests of specific command only (e.g., --command=syntax)
  const commandFilterArg = args.find(arg => arg.startsWith('--command='));
  const commandFilter = commandFilterArg ? commandFilterArg.split('=')[1] : null;

  let runJest, runAunit, runCmd, runLifecycle, runPull, runFullPull, runConflict, runDebugRepl, runDebugScripted, runDebugBp, runSyncXml, runXmlOnly, runJunit, runDrop, runCustomize;

  if (args.includes('--jest')) {
    runJest = true;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--aunit')) {
    runJest = false;
    runAunit = true;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--cmd')) {
    runJest = false;
    runAunit = false;
    runCmd = true;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--lifecycle')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = true;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--pull')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = true;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--full-pull')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = true;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--conflict')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = true;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--sync-xml')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = true;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--xml-only')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = true;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--junit')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = true;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--debug')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = true;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--debug-scripted')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = true;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--debug-repl')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = true;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = false;
  } else if (args.includes('--drop')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = true;
    runCustomize = false;
  } else if (args.includes('--customize')) {
    runJest = false;
    runAunit = false;
    runCmd = false;
    runLifecycle = false;
    runPull = false;
    runFullPull = false;
    runConflict = false;
    runSyncXml = false;
    runXmlOnly = false;
    runJunit = false;
    runDebugRepl = false;
    runDebugScripted = false;
    runDebugBp = false;
    runDrop = false;
    runCustomize = true;
  } else {
    // Run all tests.
    // In CI (Jenkins/GitHub Actions): skip REPL scenarios (1 & 2) — they require
    // an interactive TTY that CI pods don't have. Scripted scenarios (3, 4, 5) use
    // --json throughout — no TTY needed — and are stable enough to run in CI.
    const isCI = !!(process.env.BUILD_NUMBER || process.env.CI || process.env.GITHUB_ACTIONS);
    runJest = true;
    runAunit = true;
    runCmd = true;
    runJunit = true;
    runDrop = true;
    runCustomize = true;
    runLifecycle = false;      // Lifecycle tests run as part of cmd tests
    runPull = false;           // Pull tests run as part of cmd tests
    runFullPull = false;       // Full pull tests run as part of cmd tests
    runConflict = false;       // Conflict tests run standalone (stateful, sequential)
    runSyncXml = false;        // Sync-xml tests run as part of cmd tests (--command=pull)
    runXmlOnly = false;        // XML-only tests run as part of cmd tests (--command=pull)
    runDebugRepl = !isCI;      // Skip REPL scenarios in CI (require interactive TTY)
    runDebugScripted = !isCI;  // Skip scripted scenarios in CI (require TCP session affinity that CI network breaks)
    runDebugBp = true;         // Always run — only needs ABAP system connection
    if (isCI) {
      printInfo('  ⚠️  CI environment detected — skipping debug scenarios 1-5 (REPL needs TTY, scripted needs TCP affinity)');
    }
  }

  printHeader('UNIFIED TEST SUITE');

  const results = {};

  // Run setup phase before any ABAP-dependent integration tests.
  // Skipped when running Jest-only (no ABAP connection needed).
  // --no-setup flag disables auto-setup for developers who manage prerequisites manually.
  const skipAutoSetup = args.includes('--no-setup');
  const needsAbap = runAunit || runCmd || runDrop || runCustomize || runDebugBp ||
                    runPull || runFullPull || runConflict || runSyncXml || runXmlOnly ||
                    runJunit || runDebugRepl || runDebugScripted || runLifecycle;
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

  // Run Full Pull tests only (no --files)
  if (runFullPull) {
    printSubHeader('Running Full Pull Tests Only (no --files)');
    const fullPullResults = runFullPullTestsWrapper();
    results.fullPull = fullPullResults;
  }

  // Run Conflict detection tests only
  if (runConflict) {
    printSubHeader('Running Conflict Detection Tests Only');
    const conflictResults = runConflictTestsWrapper();
    results.conflict = conflictResults;
  }

  // Run --sync-xml integration tests only
  if (runSyncXml) {
    printSubHeader('Running --sync-xml Integration Tests Only');
    const syncXmlResults = runSyncXmlTestsWrapper();
    results.syncXml = syncXmlResults;
  }

  // Run XML-only object pull tests only
  if (runXmlOnly) {
    printSubHeader('Running XML-Only Object Pull Tests Only');
    const xmlOnlyResults = runXmlOnlyTestsWrapper();
    results.xmlOnly = xmlOnlyResults;
  }

  // Run JUnit output integration tests
  if (runJunit) {
    results.junit = runJUnitTestsWrapper();
  }

  // Run Debug breakpoint tests (requires abgagt-debug-test repo)
  if (runDebugBp) {
    results.debugBp = runDebugTestsWrapper();
  }

  // Cooldown between command tests and drop tests.
  // Command tests make ~90 HTTP requests; the EZABAPGIT lock may still be held
  // briefly when drop tests start, causing the reset pull to return
  // "Activation cancelled" — leaving PROG/FUGR absent from TADIR.
  if (runCmd && runDrop) {
    printInfo('  Cooling down 15s after command tests before drop tests...');
    await new Promise(r => setTimeout(r, 15000));
  }

  // Run Drop command tests
  if (runDrop) {
    results.drop = runDropTestsWrapper();
  }

  // Run Customize command tests
  if (runCustomize) {
    results.customize = runCustomizeTestsWrapper();
  }

  // Run Debug scenario tests — always last.
  // REPL scenarios (1 & 2) require an interactive TTY and bash — local dev only.
  // Scripted scenarios (3/4/5) use bash but no TTY — safe to run in CI.
  if (runDebugScripted) {
    results.debugScripted = runScriptedDebugScenarios();
  }
  if (runDebugRepl) {
    results.debugRepl = runReplDebugScenarios();
  }

  // Print summary and exit with appropriate code
  const allPassed = printSummary(results);

  if (!allPassed) {
    process.exit(1);
  }
}

main();
