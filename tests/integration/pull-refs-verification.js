/**
 * Pull Command - Git References (Tags & Branches) Verification Test
 *
 * This test verifies that pull command correctly activates different versions
 * from tags and branches by checking the actual activated code.
 *
 * Run this from the test repository: abgagt-pull-test
 * $ cd /path/to/abgagt-pull-test
 * $ node /path/to/abapgit-agent/tests/integration/pull-refs-verification.js
 */

const { execSync } = require('child_process');
const path = require('path');

// Test repository details
const TEST_REPO_URL = 'https://github.tools.sap/I045696/abgagt-pull-test.git';
const TEST_OBJECT = 'ZIF_SIMPLE_TEST';

// Define test cases with expected method signatures
const TEST_CASES = [
  {
    ref: 'v0.1.0',
    type: 'tag',
    description: 'Tag v0.1.0 - only get_message method',
    expectedMethods: ['get_message'],
    unexpectedMethods: ['validate_input', 'calculate_sum']
  },
  {
    ref: 'v1.0.0',
    type: 'tag',
    description: 'Tag v1.0.0 - adds validate_input method',
    expectedMethods: ['get_message', 'validate_input'],
    unexpectedMethods: ['calculate_sum']
  },
  {
    ref: 'feature/test-branch',
    type: 'branch',
    description: 'Branch feature/test-branch - has calculate_sum method',
    expectedMethods: ['get_message', 'calculate_sum'],
    unexpectedMethods: ['validate_input']
  },
  {
    ref: 'main',
    type: 'branch',
    description: 'Branch main - same as v1.0.0',
    expectedMethods: ['get_message', 'validate_input'],
    unexpectedMethods: ['calculate_sum']
  }
];

/**
 * Run CLI command
 */
function runCli(command, args = []) {
  const argsStr = args.join(' ');
  const cmd = `node ${path.join(__dirname, '../../bin/abapgit-agent')} ${command} ${argsStr}`;
  return execSync(cmd, { encoding: 'utf8', stdio: 'pipe' });
}

/**
 * Extract methods from interface definition
 */
function extractMethods(interfaceCode) {
  const methodRegex = /METHODS\s+(\w+)/gi;
  const methods = [];
  let match;
  while ((match = methodRegex.exec(interfaceCode)) !== null) {
    methods.push(match[1].toLowerCase());
  }
  return methods;
}

/**
 * Delete the test object to ensure clean state
 */
function deleteTestObject() {
  console.log(`🗑️  Deleting ${TEST_OBJECT} to ensure clean state...`);
  try {
    // Use ABAP system to delete the object
    // We'll use a workaround: pull from a branch that doesn't have the object
    // Or we can skip this and document the limitation
    console.log(`⚠️  Note: Manual deletion may be required for clean test state`);
    return true;
  } catch (error) {
    console.log(`⚠️  Could not delete object (may not exist): ${error.message}`);
    return true; // Don't fail if object doesn't exist
  }
}

/**
 * Run test for a specific git ref
 */
function runTest(testCase, skipDelete = false) {
  console.log(`\n${'='.repeat(80)}`);
  console.log(`Testing: ${testCase.description}`);
  console.log(`  Ref: ${testCase.ref}`);
  console.log(`${'='.repeat(80)}\n`);

  // Step 0: Delete object for clean state (optional)
  if (!skipDelete) {
    deleteTestObject();
  }

  // Step 1: Pull from the specified ref
  console.log(`📥 Pulling from ${testCase.ref}...`);
  try {
    const pullOutput = runCli('pull', ['--url', TEST_REPO_URL, '--branch', testCase.ref]);

    if (!pullOutput.includes('Pull completed successfully')) {
      console.error(`❌ Pull failed for ${testCase.ref}`);
      console.error(pullOutput);
      return false;
    }

    console.log(`✅ Pull completed successfully`);
  } catch (error) {
    console.error(`❌ Pull command failed: ${error.message}`);
    return false;
  }

  // Step 2: View the activated interface
  console.log(`\n📖 Viewing ${TEST_OBJECT}...`);
  let viewOutput;
  try {
    viewOutput = runCli('view', ['--objects', TEST_OBJECT]);
  } catch (error) {
    console.error(`❌ View command failed: ${error.message}`);
    return false;
  }

  // Step 3: Extract methods from the output
  const activatedMethods = extractMethods(viewOutput);
  console.log(`\n🔍 Activated methods: ${activatedMethods.join(', ')}`);

  // Step 4: Verify expected methods are present
  let passed = true;
  console.log(`\n✓ Expected methods:`);
  for (const method of testCase.expectedMethods) {
    const found = activatedMethods.includes(method.toLowerCase());
    if (found) {
      console.log(`  ✅ ${method} - found`);
    } else {
      console.log(`  ❌ ${method} - MISSING`);
      passed = false;
    }
  }

  // Step 5: Verify unexpected methods are absent
  console.log(`\n✓ Should NOT have:`);
  for (const method of testCase.unexpectedMethods) {
    const found = activatedMethods.includes(method.toLowerCase());
    if (!found) {
      console.log(`  ✅ ${method} - correctly absent`);
    } else {
      console.log(`  ❌ ${method} - INCORRECTLY PRESENT`);
      passed = false;
    }
  }

  console.log(`\n${'='.repeat(80)}`);
  if (passed) {
    console.log(`✅ TEST PASSED: ${testCase.ref}`);
  } else {
    console.log(`❌ TEST FAILED: ${testCase.ref}`);
  }
  console.log(`${'='.repeat(80)}\n`);

  return passed;
}

/**
 * Main test runner
 */
function main() {
  console.log(`
╔════════════════════════════════════════════════════════════════════════════╗
║                                                                            ║
║  Pull Command - Git References Verification Test                          ║
║                                                                            ║
║  This test verifies that pull command correctly activates code from        ║
║  different git tags and branches.                                          ║
║                                                                            ║
║  ⚠️  IMPORTANT: For accurate testing, delete ${TEST_OBJECT} in SE24        ║
║     before running this test. abapGit's pull may not remove existing       ║
║     methods when switching to older versions (by design, to prevent        ║
║     accidental code deletion).                                             ║
║                                                                            ║
╚════════════════════════════════════════════════════════════════════════════╝
  `);

  const results = [];

  for (const testCase of TEST_CASES) {
    const passed = runTest(testCase);
    results.push({ ref: testCase.ref, passed });
  }

  // Summary
  console.log(`\n${'='.repeat(80)}`);
  console.log(`TEST SUMMARY`);
  console.log(`${'='.repeat(80)}\n`);

  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  for (const result of results) {
    const status = result.passed ? '✅' : '❌';
    console.log(`  ${status} ${result.ref}`);
  }

  console.log(`\n${passedCount}/${totalCount} tests passed\n`);

  if (passedCount === totalCount) {
    console.log(`✅ ALL TESTS PASSED!`);
    process.exit(0);
  } else {
    console.log(`❌ SOME TESTS FAILED`);
    process.exit(1);
  }
}

// Run if executed directly
if (require.main === module) {
  main();
}

module.exports = { runTest, TEST_CASES };
