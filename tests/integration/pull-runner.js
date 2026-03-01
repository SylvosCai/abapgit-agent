/**
 * Pull Command Test Runner
 * Tests pull command with different git refs (tags, branches) and verifies activated content
 *
 * These tests verify that:
 * 1. Pull command correctly switches between git refs (tags and branches)
 * 2. Objects are activated with correct content from each ref
 * 3. View command can verify the activated content
 *
 * Test Repository: https://github.tools.sap/I045696/abgagt-pull-test.git
 *   - v0.1.0: Interface with only get_message method
 *   - v1.0.0: Interface with get_message + validate_input methods
 *   - feature/test-branch: Interface with get_message + calculate_sum methods
 *   - main: Same as v1.0.0 (get_message + validate_input)
 */

const { execSync } = require('child_process');
const path = require('path');

// Test repository configuration
const TEST_REPO_URL = 'https://github.tools.sap/I045696/abgagt-pull-test.git';
const TEST_OBJECT = 'ZIF_SIMPLE_TEST';

/**
 * Pull test cases - each test pulls from a specific ref and verifies content
 */
const pullTestCases = [
  // Tag v0.1.0 - only get_message method
  {
    ref: 'v0.1.0',
    type: 'tag',
    pullTest: {
      name: 'pull from tag v0.1.0',
      args: ['--url', TEST_REPO_URL, '--branch', 'v0.1.0'],
      verify: (output) => {
        const hasPull = output.includes('Pull completed');
        const hasActivated = output.includes('Activated') || output.includes(TEST_OBJECT);
        const hasJobId = output.includes('Job ID:');
        return hasPull && hasActivated && hasJobId;
      }
    },
    verifyTest: {
      name: 'verify v0.1.0 - has only get_message',
      expectedMethods: ['get_message'],
      unexpectedMethods: ['validate_input', 'calculate_sum']
    }
  },

  // Tag v1.0.0 - get_message + validate_input methods
  {
    ref: 'v1.0.0',
    type: 'tag',
    pullTest: {
      name: 'pull from tag v1.0.0',
      args: ['--url', TEST_REPO_URL, '--branch', 'v1.0.0'],
      verify: (output) => {
        const hasPull = output.includes('Pull completed');
        const hasActivated = output.includes('Activated') || output.includes(TEST_OBJECT);
        const hasJobId = output.includes('Job ID:');
        return hasPull && hasActivated && hasJobId;
      }
    },
    verifyTest: {
      name: 'verify v1.0.0 - has get_message and validate_input',
      expectedMethods: ['get_message', 'validate_input'],
      unexpectedMethods: ['calculate_sum']
    }
  },

  // Branch feature/test-branch - get_message + calculate_sum methods
  {
    ref: 'feature/test-branch',
    type: 'branch',
    pullTest: {
      name: 'pull from branch feature/test-branch',
      args: ['--url', TEST_REPO_URL, '--branch', 'feature/test-branch'],
      verify: (output) => {
        const hasPull = output.includes('Pull completed');
        const hasActivated = output.includes('Activated') || output.includes(TEST_OBJECT);
        const hasJobId = output.includes('Job ID:');
        return hasPull && hasActivated && hasJobId;
      }
    },
    verifyTest: {
      name: 'verify feature/test-branch - has get_message and calculate_sum',
      expectedMethods: ['get_message', 'calculate_sum'],
      unexpectedMethods: ['validate_input']
    }
  },

  // Branch main - same as v1.0.0
  {
    ref: 'main',
    type: 'branch',
    pullTest: {
      name: 'pull from branch main',
      args: ['--url', TEST_REPO_URL, '--branch', 'main'],
      verify: (output) => {
        const hasPull = output.includes('Pull completed');
        const hasActivated = output.includes('Activated') || output.includes(TEST_OBJECT);
        const hasJobId = output.includes('Job ID:');
        return hasPull && hasActivated && hasJobId;
      }
    },
    verifyTest: {
      name: 'verify main - has get_message and validate_input',
      expectedMethods: ['get_message', 'validate_input'],
      unexpectedMethods: ['calculate_sum']
    }
  }
];

/**
 * Run pull tests with git ref switching and verification
 * @param {string} repoRoot - Repository root path
 * @param {Object} printFunctions - Print helper functions
 * @returns {Object} Test results with success, results, duration, passedCount, totalCount
 */
function runPullTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors }) {
  printSubHeader('Running Pull Command Tests (Git Refs with Verification)');

  const startTime = Date.now();
  const results = [];

  printInfo(`Test repository: ${TEST_REPO_URL}`);
  printInfo(`Test object: ${TEST_OBJECT}`);
  printInfo('');

  // Run each pull test case
  for (const testCase of pullTestCases) {
    const { ref, type, pullTest, verifyTest } = testCase;

    printInfo(colorize('cyan', `Testing ${type}: ${ref}`));

    // Step 1: Pull from git ref
    try {
      const pullCmd = `node ${path.join(repoRoot, 'bin', 'abapgit-agent')} pull ${pullTest.args.join(' ')}`;
      const pullOutput = execSync(pullCmd, {
        cwd: repoRoot,
        encoding: 'utf8',
        stdio: ['pipe', 'pipe', 'pipe']
      });

      const pullPassed = pullTest.verify(pullOutput);
      if (pullPassed) {
        printSuccess(`✓ ${pullTest.name}`);
        results.push({ name: pullTest.name, passed: true });
      } else {
        printError(`✗ ${pullTest.name}`);
        printError(`  Output did not match expected pattern`);
        results.push({ name: pullTest.name, passed: false, error: 'Verification failed' });
        continue; // Skip verify test if pull failed
      }
    } catch (error) {
      printError(`✗ ${pullTest.name}`);
      printError(`  ${error.message}`);
      results.push({ name: pullTest.name, passed: false, error: error.message });
      continue; // Skip verify test if pull failed
    }

    // Step 2: Verify activated content using view command
    try {
      const viewCmd = `node ${path.join(repoRoot, 'bin', 'abapgit-agent')} view --objects ${TEST_OBJECT}`;
      const viewOutput = execSync(viewCmd, {
        cwd: repoRoot,
        encoding: 'utf8',
        stdio: ['pipe', 'pipe', 'pipe']
      });

      // Check expected methods are present
      const hasExpected = verifyTest.expectedMethods.every(method => viewOutput.includes(method));
      // Check unexpected methods are NOT present
      const noUnexpected = verifyTest.unexpectedMethods.every(method => !viewOutput.includes(method));

      const verifyPassed = hasExpected && noUnexpected;
      if (verifyPassed) {
        printSuccess(`✓ ${verifyTest.name}`);
        results.push({ name: verifyTest.name, passed: true });
      } else {
        printError(`✗ ${verifyTest.name}`);
        if (!hasExpected) {
          printError(`  Missing expected methods: ${verifyTest.expectedMethods.filter(m => !viewOutput.includes(m)).join(', ')}`);
        }
        if (!noUnexpected) {
          printError(`  Found unexpected methods: ${verifyTest.unexpectedMethods.filter(m => viewOutput.includes(m)).join(', ')}`);
        }
        results.push({ name: verifyTest.name, passed: false, error: 'Content verification failed' });
      }
    } catch (error) {
      printError(`✗ ${verifyTest.name}`);
      printError(`  ${error.message}`);
      results.push({ name: verifyTest.name, passed: false, error: error.message });
    }

    printInfo('');
  }

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;
  const success = passedCount === totalCount;

  return { success, results, duration, passedCount, totalCount };
}

module.exports = {
  runPullTests,
  pullTestCases
};
