/**
 * AUnit Test Runner
 * Discovers and runs ABAP unit test classes from the abapgit-agent abap/ directory
 */
const path = require('path');
const fs = require('fs');
const { execSync } = require('child_process');

// Auto-discover all ABAP test class files from the project's abap/ directory
function discoverTestClasses(repoRoot) {
  const abapDir = path.join(repoRoot, 'abap');
  if (!fs.existsSync(abapDir)) return [];
  return fs.readdirSync(abapDir)
    .filter(f => f.endsWith('.clas.testclasses.abap'))
    .map(f => ({ name: f.replace('.clas.testclasses.abap', ''), filePath: path.join(abapDir, f) }))
    .sort((a, b) => a.name.localeCompare(b.name));
}

/**
 * Run AUnit tests for ABAP test classes
 */
function runAUnitTests(repoRoot, { printSubHeader, printSuccess, printError, printWarning }) {
  printSubHeader('Running AUnit Tests');

  const startTime = Date.now();
  const results = [];
  const testClasses = discoverTestClasses(repoRoot);

  if (testClasses.length === 0) {
    printWarning(`No test classes found in abap/ — skipping AUnit tests`);
    return { success: true, results, duration: '0.0', passedCount: 0, totalCount: 0 };
  }

  for (const { name, filePath } of testClasses) {
    try {
      const output = execSync(
        `node bin/abapgit-agent unit --files ${filePath}`,
        { cwd: repoRoot, encoding: 'utf8' }
      );

      const passed = output.includes('All tests passed') || output.includes('passed');
      const className = name.toUpperCase();

      if (passed) {
        printSuccess(`${className}: All tests passed`);
        results.push({ className, passed: true, error: null });
      } else {
        printError(`${className}: Tests failed`);
        results.push({ className, passed: false, error: 'Tests failed' });
      }
    } catch (error) {
      const className = name.toUpperCase();
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
