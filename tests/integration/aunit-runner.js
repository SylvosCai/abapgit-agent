/**
 * AUnit Test Runner
 * Discovers and runs ABAP unit test classes
 */
const path = require('path');
const fs = require('fs');
const os = require('os');
const { execSync } = require('child_process');

const UNIT_TEST_CLONE_DIR = path.join(os.tmpdir(), 'abgagt-unit-test');

// Classes that intentionally fail — excluded from the "all must pass" aunit runner
// (they are covered by dedicated failure-scenario tests in junit-runner.js)
const EXPECTED_FAILING_CLASSES = new Set(['zcl_abgagt_unit_fail']);

// Auto-discover all ABAP test class files from the unit test repo clone
function discoverTestClasses() {
  const srcDir = path.join(UNIT_TEST_CLONE_DIR, 'src');
  if (!fs.existsSync(srcDir)) return [];
  return fs.readdirSync(srcDir)
    .filter(f => f.endsWith('.clas.testclasses.abap'))
    .map(f => f.replace('.clas.testclasses.abap', ''))
    .filter(name => !EXPECTED_FAILING_CLASSES.has(name))
    .sort();
}

/**
 * Run AUnit tests for ABAP test classes
 * @param {string} repoRoot - Repository root path
 * @param {Function} printSubHeader - Print subheader function
 * @param {Function} printSuccess - Print success function
 * @param {Function} printError - Print error function
 * @param {Function} printWarning - Print warning function
 * @returns {Object} Test results with success, results, duration, passedCount, totalCount
 */
function runAUnitTests(repoRoot, { printSubHeader, printSuccess, printError, printWarning }) {
  printSubHeader('Running AUnit Tests');

  const startTime = Date.now();
  const results = [];
  const testClasses = discoverTestClasses();

  if (testClasses.length === 0) {
    printWarning(`Unit test repo not cloned yet: ${UNIT_TEST_CLONE_DIR} — skipping AUnit tests`);
    return { success: true, results, duration: '0.0', passedCount: 0, totalCount: 0 };
  }

  for (const testClass of testClasses) {
    const fileName = `${testClass}.clas.testclasses.abap`;
    const filePath = path.join(UNIT_TEST_CLONE_DIR, 'src', fileName);

    if (!fs.existsSync(filePath)) {
      printWarning(`Test class not found: ${fileName}`);
      continue;
    }

    try {
      const output = execSync(
        `node bin/abapgit-agent unit --files ${filePath}`,
        { cwd: repoRoot, encoding: 'utf8' }
      );

      const passed = output.includes('All tests passed') || output.includes('passed');
      const className = testClass.toUpperCase();

      if (passed) {
        printSuccess(`${className}: All tests passed`);
        results.push({ className, passed: true, error: null });
      } else {
        printError(`${className}: Tests failed`);
        results.push({ className, passed: false, error: 'Tests failed' });
      }
    } catch (error) {
      const className = testClass.toUpperCase();
      printError(`${className}: ${error.message}`);
      results.push({ className, passed: false, error: error.message });
    }
  }

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  if (passedCount === totalCount) {
    printSuccess(`AUnit tests: ${passedCount}/${totalCount} classes passed (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`AUnit tests: ${passedCount}/${totalCount} classes passed (${duration}s)`);
    return { success: false, results, duration, passedCount, totalCount };
  }
}

module.exports = { runAUnitTests, discoverTestClasses };
