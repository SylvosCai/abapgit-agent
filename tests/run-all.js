/**
 * Unified Test Suite
 *
 * Runs all test types:
 * 1. npm test (Jest) - JavaScript unit tests
 * 2. AUnit tests - ABAP test classes
 * 3. Command tests - CLI commands against real ABAP system
 *
 * Usage:
 *   npm run test:all        # Run all tests
 *   npm run test:jest      # Jest only
 *   npm run test:aunit     # AUnit only
 *   npm run test:cmd       # Command tests only
 *   npm run test:cmd --demo # Command tests in demo mode (shows command and output)
 */

const { execSync, spawn } = require('child_process');
const path = require('path');
const fs = require('fs');
const readline = require('readline');

const repoRoot = path.join(__dirname, '..');

// Import test runners from integration folder
const { runAUnitTests } = require('./integration/aunit-runner');
const { runLifecycleTests } = require('./integration/lifecycle-runner');

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
 */
function runCommandTests(demoMode = false) {
  printSubHeader('Running Command Tests (Real ABAP System)' + (demoMode ? ' [DEMO MODE]' : ''));

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
    return runDemoCommandTests(commandTestCases, startTime);
  }

  // Original synchronous implementation
  for (const testCase of commandTestCases) {
    process.stdout.write(`  Testing: ${testCase.command} ${testCase.name}... `);

    let commandPassed = false;
    let output = '';

    try {
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

  // Run lifecycle tests (init, create, import, delete) if test directory exists
  const lifecycleResults = runLifecycleTestsWrapper();
  if (lifecycleResults && !lifecycleResults.skipped && lifecycleResults.results) {
    // Merge lifecycle results into command test results
    results.push(...lifecycleResults.results);
    passedCount += lifecycleResults.passedCount || 0;
    totalCount += lifecycleResults.totalCount || 0;
    printInfo(`  (Including ${lifecycleResults.passedCount || 0}/${lifecycleResults.totalCount || 0} lifecycle tests)`);
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
  const hasSpecificTest = args.some(arg => ['--jest', '--aunit', '--cmd'].includes(arg));

  // Demo mode shows command and output for each test
  const demoMode = args.includes('--demo');

  let runJest, runAunit, runCmd;

  if (args.includes('--jest')) {
    runJest = true;
    runAunit = false;
    runCmd = false;
  } else if (args.includes('--aunit')) {
    false;
    runAunit = true;
    runCmd = false;
  } else if (args.includes('--cmd')) {
    runJest = false;
    runAunit = false;
    runCmd = true;
  } else {
    // Run all tests
    runJest = true;
    runAunit = true;
    runCmd = true;
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
    results.cmd = await runCommandTests(demoMode);
  }

  // Print summary and exit with appropriate code
  const allPassed = printSummary(results);

  if (!allPassed) {
    process.exit(1);
  }
}

main();
