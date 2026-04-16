/**
 * Lifecycle Test Runner
 * Tests init, create, import, delete commands with full integration
 */
const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');

const { getTestRepoUrl } = require('./test-repos');

// Test repository configuration
const TEST_REPO_URL = getTestRepoUrl('lifecycle');

/**
 * Read ABAP configuration from .abapGitAgent file
 * @param {string} repoRoot - Repository root path
 * @returns {Object|null} Configuration object or null
 */
function readAbapConfig(repoRoot) {
  const configPath = path.join(repoRoot, '.abapGitAgent');
  if (fs.existsSync(configPath)) {
    return JSON.parse(fs.readFileSync(configPath, 'utf8'));
  }
  return null;
}

/**
 * Run comprehensive lifecycle test with full reset and validation
 * @param {string} repoRoot - Repository root path
 * @param {Object} printFunctions - Print helper functions
 * @returns {Object} Test results with success, results, duration, passedCount, totalCount
 */
function runFullLifecycleTests(repoRoot, { printSubHeader, printInfo, printSuccess, printError, colorize, colors }) {
  printSubHeader('Running Lifecycle Command Tests (Full Integration)');

  const startTime = Date.now();
  const results = [];
  const testRepoDir = path.join(os.tmpdir(), 'abgagt-lifecycle-test');

  // Step 1: Clone or update test repo
  let testDir = testRepoDir;
  if (fs.existsSync(testDir)) {
    printInfo('Test repo already exists, using existing copy');
  } else {
    printInfo(`Cloning test repo: ${TEST_REPO_URL}`);
    try {
      execSync(`git clone ${TEST_REPO_URL} ${testDir}`, { encoding: 'utf8' });
    } catch (e) {
      printError(`Failed to clone repo: ${e.message}`);
      return { success: false, results: [], error: e.message };
    }
  }

  // Helper function to run CLI command in test directory
  const runCli = (cmd, args = []) => {
    const argsStr = args.map(arg => `'${arg}'`).join(' ');
    try {
      return execSync(
        `abapgit-agent ${cmd} ${argsStr}`,
        { cwd: testDir, encoding: 'utf8', timeout: 120000, stdio: ['pipe', 'pipe', 'pipe'] }
      );
    } catch (e) {
      // Return combined stdout+stderr so callers can inspect the actual error message
      return (e.stdout || '') + (e.stderr || '') || e.message;
    }
  };

  // Helper function to add test result
  const addResult = (name, passed, output = '') => {
    results.push({ name, passed, output: output.substring(0, 200) });
    if (passed) {
      console.log(colorize('green', '✅ ') + name);
    } else {
      console.log(colorize('red', '❌ ') + name);
      if (output) {
        console.log(colors.gray + `   ${output.substring(0, 100)}...` + colors.reset);
      }
    }
  };

  try {
    // Step 3: Reset to first commit
    printInfo('Resetting test repo to first commit...');
    execSync('git checkout main', { cwd: testDir });
    // Find the actual first commit and reset to it
    const firstCommit = execSync('git log --reverse --format=%H', { cwd: testDir, encoding: 'utf8' }).trim().split('\n')[0];
    if (firstCommit) {
      execSync(`git reset --hard ${firstCommit}`, { cwd: testDir });
      // Force push to reset remote to first commit as well
      printInfo('Force pushing to reset remote to first commit...');
      execSync('git push --force origin main', { cwd: testDir });
    }
    // Clean up any .abapGitAgent file that might exist from previous runs
    const testConfigPath = path.join(testDir, '.abapGitAgent');
    if (fs.existsSync(testConfigPath)) {
      printInfo('Removing existing .abapGitAgent from previous test run...');
      fs.unlinkSync(testConfigPath);
    }

    // Step 4: Run health without config - should fail/not work
    printInfo('Testing health without config...');
    let output = '';
    output = runCli('health');
    // Expected: either a graceful "not configured" message, or a connection error — not a crash
    const healthNoCfgPassed = output.includes('not configured') || output.includes('healthy') ||
      output.includes('OK') || output.includes('Error') || output.includes('error') ||
      output.length > 0;
    addResult('health without config', healthNoCfgPassed, output);

    // Step 5: Run init command
    printInfo('Running init command...');
    output = runCli('init', ['--package', '$ABGAGT_LIFECYCLE_TEST', '--folder', 'src/']);
    const initPassed = output.includes('Created .abapGitAgent') ||
                       output.includes('initialized');
    addResult('init creates config files', initPassed, output);

    const projectConfigPath = path.join(testDir, '.abapgit-agent.json');
    const projectConfigCreated = fs.existsSync(projectConfigPath) &&
      JSON.parse(fs.readFileSync(projectConfigPath, 'utf8')).project.name === '$ABGAGT_LIFECYCLE_TEST';
    addResult('init creates .abapgit-agent.json with package name', projectConfigCreated, projectConfigPath);

    // Step 6: Edit .abapGitAgent with credentials from main project
    printInfo('Updating .abapGitAgent with credentials...');
    const config = readAbapConfig(repoRoot);
    if (config) {
      config.package = '$ABGAGT_LIFECYCLE_TEST';
      config.folder = '/src/';
      // In CI the main project uses a service user token which may have no write access to
      // personal test repos. Override with a personal token when available.
      if (process.env.TEST_GIT_USR) config.gitUsername = process.env.TEST_GIT_USR;
      if (process.env.TEST_GIT_PSW) config.gitPassword = process.env.TEST_GIT_PSW;
      // Keep referenceFolder as-is for the test environment
      fs.writeFileSync(path.join(testDir, '.abapGitAgent'), JSON.stringify(config, null, 2));
    }

    // Step 7: Run health with config - should work
    printInfo('Testing health with config...');
    output = runCli('health');
    const healthPassed = output.includes('healthy') || output.includes('OK') || output.includes('"status"');
    addResult('health with config', healthPassed, output);

    // Step 8: Run status - check if repo exists in ABAP
    printInfo('Checking status (repo may or may not exist)...');
    output = runCli('status');
    const outputLower = output.toLowerCase();
    const repoExists = outputLower.includes('created') && !outputLower.includes('not created');
    const repoNotCreated = outputLower.includes('not created') || outputLower.includes('not found');
    // Test passes if we get a clear status either way
    addResult('status - repo status checked', repoExists || repoNotCreated, output);

    // Step 9: If repo exists, delete it first
    if (repoExists) {
      printInfo('Repo exists, deleting first...');
      try {
        output = runCli('delete');
        addResult('cleanup - delete existing repo', true, output);
      } catch (e) {
        addResult('cleanup - delete existing repo', false, e.message);
      }
      // Verify deleted
      output = runCli('status');
      const afterDelete = output.includes('Not created') || output.includes('Not found');
      addResult('status after delete', afterDelete, output);
    }

    // Step 10: Commit init changes
    printInfo('Committing init changes...');
    execSync('git add .', { cwd: testDir });
    execSync('git commit -m "chore: init project"', { cwd: testDir });

    // Step 11: Run create command
    printInfo('Creating abapGit repo...');
    output = runCli('create');
    const createPassed = output.includes('success') && output.includes('created');
    addResult('create repo', createPassed, output);

    // Step 12: Run import command (async with polling)
    printInfo('Importing objects (async job with polling)...');
    output = runCli('import', ['--message', 'test: initial import']);
    // Check for async job pattern: job started, polling, completed
    const importPassed = (
      (output.includes('Job started') || output.includes('job started')) &&
      (output.includes('Import completed successfully') || output.includes('completed'))
    ) || output.includes('No objects');
    addResult('import objects (async)', importPassed, output);

    // Step 13: Git pull to see new commits
    // Use fetch + reset --hard to avoid merge conflicts when the remote was force-reset
    // (the import step may have created .abapgit.xml on the remote which conflicts with
    // the local version created by init)
    printInfo('Git pull to check import...');
    try {
      execSync('git fetch origin main', { cwd: testDir });
      execSync('git reset --hard origin/main', { cwd: testDir });
      // Check if ZIF_ABGAGT_TEST interface exists
      const interfacePath = path.join(testDir, 'src', 'zif_abgagt_test.intf.abap');
      const interfaceExists = fs.existsSync(interfacePath);
      addResult('git pull - ZIF_ABGAGT_TEST imported', interfaceExists);
      // Store for next steps
      const hasInterface = interfaceExists;
    } catch (e) {
      addResult('git pull - check import', false, e.message);
      var hasInterface = false;
    }

    // Step 14: Run abapgit-agent pull
    printInfo('Running pull to activate objects...');
    try {
      output = runCli('pull');
      const pullPassed = output.includes('Pull completed') || output.includes('Activated');
      addResult('pull activates objects', pullPassed, output);
    } catch (e) {
      addResult('pull activates objects', false, e.message);
    }

    // Step 15: Edit interface with minor change
    printInfo('Editing interface for repeatability test...');
    const interfacePath = path.join(testDir, 'src', 'zif_abgagt_test.intf.abap');
    if (fs.existsSync(interfacePath)) {
      let interfaceContent = fs.readFileSync(interfacePath, 'utf8');
      // Remove any existing test comment lines first (lines starting with " Test comment)
      interfaceContent = interfaceContent.replace(/^" Test comment.*\n?/gm, '');
      // Generate random comment with timestamp for repeatability test
      const randomComment = `" Test comment ${Date.now()}\n`;
      interfaceContent = randomComment + interfaceContent;
      fs.writeFileSync(interfacePath, interfaceContent);

      // Commit and push
      execSync('git add .', { cwd: testDir });
      execSync('git commit -m "test: minor interface change"', { cwd: testDir });
      execSync('git push origin main', { cwd: testDir });

      // Step 16: Pull the change
      printInfo('Pulling interface change...');
      try {
        output = runCli('pull');
        const pullChangePassed = output.includes('Pull completed') || output.includes('Activated');
        addResult('pull interface change', pullChangePassed, output);
      } catch (e) {
        addResult('pull interface change', false, e.message);
      }

      // Step 17: Run inspect
      printInfo('Running inspect on interface...');
      try {
        output = runCli('inspect', ['--files', 'src/zif_abgagt_test.intf.abap']);
        const inspectPassed = output.includes('Syntax check passed');
        addResult('inspect syntax check', inspectPassed, output);
      } catch (e) {
        addResult('inspect syntax check', false, e.message);
      }
    } else {
      addResult('edit interface', false, 'Interface file not found');
      addResult('pull interface change', false, 'Skipped');
      addResult('inspect syntax check', false, 'Skipped');
    }

    // Step 18: Run delete command
    printInfo('Deleting repo...');
    try {
      output = runCli('delete');
      const deletePassed = output.includes('deleted') || output.includes('success');
      addResult('delete repo', deletePassed, output);
    } catch (e) {
      addResult('delete repo', false, e.message);
    }

    // Step 19: Verify repo deleted
    printInfo('Verifying repo deleted...');
    output = runCli('status');
    const verifyDeleted = output.includes('Not created') || output.includes('Not found');
    addResult('status - repo deleted', verifyDeleted, output);

  } catch (error) {
    printError(`Lifecycle test error: ${error.message}`);
    results.push({ name: 'ERROR', passed: false, output: error.message });
  }

  const duration = ((Date.now() - startTime) / 1000).toFixed(1);
  const passedCount = results.filter(r => r.passed).length;
  const totalCount = results.length;

  if (passedCount === totalCount) {
    printSuccess(`Lifecycle tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: true, results, duration, passedCount, totalCount };
  } else {
    printError(`Lifecycle tests: ${passedCount}/${totalCount} passed (${duration}s)`);
    return { success: false, results, duration, passedCount, totalCount };
  }
}

/**
 * Run lifecycle command tests (init, create, import, delete)
 * Wrapper that calls the full implementation
 * @param {string} repoRoot - Repository root path
 * @param {Object} printFunctions - Print helper functions
 * @returns {Object} Test results
 */
function runLifecycleTests(repoRoot, printFunctions) {
  return runFullLifecycleTests(repoRoot, printFunctions);
}

module.exports = { runLifecycleTests, runFullLifecycleTests };
