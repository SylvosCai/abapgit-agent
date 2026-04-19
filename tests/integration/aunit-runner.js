/**
 * AUnit Test Runner
 * Discovers and runs ABAP unit test classes
 */
const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');

// Auto-discover all ABAP test class files
function discoverTestClasses(repoRoot) {
  return fs.readdirSync(path.join(repoRoot, 'abap'))
    .filter(f => f.endsWith('.clas.testclasses.abap'))
    .map(f => f.replace('.clas.testclasses.abap', ''))
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
  const testClasses = discoverTestClasses(repoRoot);

  for (const testClass of testClasses) {
    const fileName = `${testClass}.clas.testclasses.abap`;
    const filePath = path.join(repoRoot, 'abap', fileName);

    if (!fs.existsSync(filePath)) {
      printWarning(`Test class not found: ${fileName}`);
      continue;
    }

    try {
      // Run AUnit test using the CLI (need to include abap/ prefix)
      const cliPath = `abap/${fileName}`;
      const output = execSync(
        `node bin/abapgit-agent unit --files ${cliPath}`,
        { cwd: repoRoot, encoding: 'utf8' }
      );

      // Parse output to determine pass/fail.
      // "No unit tests" is a failure — it means the file was not recognised
      // or has no FOR TESTING methods, which must be investigated.
      const passed = output.includes('All tests passed') ||
        output.includes('passed');
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
