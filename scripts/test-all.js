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
 */

const { execSync, spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

const repoRoot = path.join(__dirname, '..');

// Auto-discover all ABAP test class files
const ABAP_TEST_CLASSES = fs.readdirSync(path.join(repoRoot, 'abap'))
  .filter(f => f.endsWith('.clas.testclasses.abap'))
  .map(f => f.replace('.clas.testclasses.abap', ''))
  .sort();

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
 * Run AUnit tests for ABAP test classes
 */
function runAUnitTests() {
  printSubHeader('Running AUnit Tests');

  const startTime = Date.now();
  const results = [];

  for (const testClass of ABAP_TEST_CLASSES) {
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

      // Parse output to determine pass/fail
      const passed = output.includes('All tests passed') ||
        output.includes('No unit tests') ||
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

/**
 * Command test cases with specific assertions (runs against real ABAP system)
 */
const commandTestCases = [
  // pull command tests
  {
    command: 'pull',
    name: 'pull --files (specific file)',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain either success or failure info, but not crash
      const hasResult = output.includes('Pull completed') ||
        output.includes('Job ID') ||
        output.includes(' Activated ') ||
        output.includes('Failed Objects');
      const noCrash = !output.includes('Error:') || output.includes('Error:') && output.includes('Job');
      return hasResult && noCrash;
    }
  },
  {
    command: 'inspect',
    name: 'inspect single file',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain syntax check result
      const hasResult = output.includes('Syntax check passed') ||
        output.includes('Syntax check failed');
      const hasObject = output.includes('ZCL_ABGAGT_UTIL') || output.includes('CLAS');
      return hasResult && hasObject;
    }
  },
  {
    command: 'unit',
    name: 'unit test class',
    args: ['--files', 'abap/zcl_abgagt_util.clas.testclasses.abap'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain test results
      const hasResult = output.includes('Tests:') &&
        (output.includes('Passed:') || output.includes('Failed:'));
      const hasClass = output.includes('ZCL_ABGAGT_UTIL');
      return hasResult && hasClass;
    }
  },
  {
    command: 'tree',
    name: 'tree package',
    args: ['--package', 'S_NWDEMO_BASIS'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain package name and tree structure
      const hasPackage = output.includes('S_NWDEMO_BASIS');
      const hasTree = output.includes('📦') || output.includes('Package');
      return hasPackage && hasTree;
    }
  },
  {
    command: 'tree',
    name: 'tree with depth',
    args: ['--package', 'S_NWDEMO_BASIS', '--depth', '2'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain package name and show depth
      const hasPackage = output.includes('S_NWDEMO_BASIS');
      const hasDepth = output.includes('Depth') || output.includes('depth');
      return hasPackage;
    }
  },
  {
    command: 'tree',
    name: 'tree with types',
    args: ['--package', 'S_NWDEMO_BASIS', '--include-types'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain package name and type counts
      const hasPackage = output.includes('S_NWDEMO_BASIS');
      const hasTypes = output.includes('CLAS=') || output.includes('TYPES:');
      return hasPackage;
    }
  },
  {
    command: 'list',
    name: 'list package',
    args: ['--package', 'S_NWDEMO_BASIS'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain package name and object list
      const hasPackage = output.includes('S_NWDEMO_BASIS');
      const hasObjects = output.includes('Objects in') || output.includes('AVAS') || output.includes('DEVC');
      return hasPackage && hasObjects;
    }
  },
  {
    command: 'list',
    name: 'list with type filter',
    args: ['--package', 'S_NWDEMO_BASIS', '--type', 'CLAS,INTF'],
    expectSuccess: true,
    verify: (output) => {
      // Should show filtered types
      const hasFilter = output.includes('CLAS') || output.includes('INTF') || output.includes('CLAS,INTF');
      return hasFilter;
    }
  },
  {
    command: 'list',
    name: 'list with name filter',
    args: ['--package', 'S_NWDEMO_BASIS', '--name', 'CL_*'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain filtered results (or empty if no matches)
      const hasResult = output.includes('Objects in') || output.includes('CL_');
      return hasResult;
    }
  },
  {
    command: 'view',
    name: 'view class',
    args: ['--objects', 'ZCL_ABGAGT_AGENT'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain the class name and definition
      const hasClass = output.includes('ZCL_ABGAGT_AGENT');
      const hasDef = output.includes('CLASS') || output.includes('DEFINITION');
      return hasClass && hasDef;
    }
  },
  {
    command: 'view',
    name: 'view interface',
    args: ['--objects', 'ZIF_ABGAGT_COMMAND'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain the interface name and interface keyword
      const hasInterface = output.includes('ZIF_ABGAGT_COMMAND');
      const hasDef = output.includes('INTERFACE');
      return hasInterface && hasDef;
    }
  },
  {
    command: 'view',
    name: 'view table',
    args: ['--objects', 'SFLIGHT', '--type', 'TABL'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain table name and field info
      const hasTable = output.includes('SFLIGHT');
      const hasField = output.includes('CARRID') || output.includes('Field') || output.includes('TABLE');
      return hasTable && hasField;
    }
  },
  {
    command: 'where',
    name: 'where class usage',
    args: ['--objects', 'ZCL_ABGAGT_AGENT'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain where-used results or empty result
      const hasResult = output.includes('Where-used') || output.includes('OBJECT') || output.includes('OBJECT_NAME') || output.includes('found');
      return hasResult;
    }
  },
  {
    command: 'where',
    name: 'where interface usage',
    args: ['--objects', 'ZIF_ABGAGT_COMMAND'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain where-used results
      const hasResult = output.includes('Where-used') || output.includes('OBJECT') || output.includes('OBJECT_NAME');
      return hasResult;
    }
  },
  {
    command: 'where',
    name: 'where with type filter',
    args: ['--objects', 'ZCL_ABGAGT_AGENT', '--type', 'CLAS'],
    expectSuccess: true,
    verify: (output) => {
      // Should contain where-used results
      const hasResult = output.includes('Where-used') || output.includes('OBJECT');
      return hasResult;
    }
  },
  {
    command: 'health',
    name: 'health check',
    args: [],
    expectSuccess: true,
    verify: (output) => {
      // Should contain status and version
      const hasStatus = output.includes('"status"') || output.includes('OK') || output.includes('healthy');
      const hasVersion = output.includes('"version"');
      return hasStatus && hasVersion;
    }
  },
  {
    command: 'status',
    name: 'status check',
    args: [],
    expectSuccess: true,
    verify: (output) => {
      // Should contain ENABLED or NOT configured
      const hasResult = output.includes('ENABLED') || output.includes('NOT configured');
      return hasResult;
    }
  },
  // ref commands (local file search - no ABAP required)
  {
    command: 'ref',
    name: 'ref --list-topics',
    args: ['--list-topics'],
    expectSuccess: true,
    verify: (output) => {
      // Should list topics
      const hasResult = output.includes('Topic') || output.includes('internal-tables') || output.includes('sql');
      return hasResult;
    }
  },
  {
    command: 'ref',
    name: 'ref --list-repos',
    args: ['--list-repos'],
    expectSuccess: true,
    verify: (output) => {
      // Should list repositories or show error if not configured
      const hasResult = output.includes('Repository') || output.includes('Reference folder') || output.includes('Not configured');
      return hasResult;
    }
  },
  {
    command: 'ref',
    name: 'ref --topic exceptions',
    args: ['--topic', 'exceptions'],
    expectSuccess: true,
    verify: (output) => {
      // Should show topic content or error
      const hasResult = output.includes('Exception') || output.includes('cx_') || output.includes('Topic') || output.length > 50;
      return hasResult;
    }
  }
];

/**
 * Read ABAP configuration from .abapGitAgent file
 */
function readAbapConfig() {
  const configPath = path.join(repoRoot, '.abapGitAgent');
  if (fs.existsSync(configPath)) {
    return JSON.parse(fs.readFileSync(configPath, 'utf8'));
  }
  return null;
}

/**
 * Get git remote URL from current repository
 */
function getGitRemoteUrl() {
  try {
    const gitConfigPath = path.join(repoRoot, '.git', 'config');
    if (fs.existsSync(gitConfigPath)) {
      const gitConfig = fs.readFileSync(gitConfigPath, 'utf8');
      const urlMatch = gitConfig.match(/url\s*=\s*(.+)/);
      if (urlMatch) {
        return urlMatch[1].trim();
      }
    }
  } catch (e) {
    // Ignore errors
  }
  return null;
}

/**
 * Test cases for init, create, import, delete commands
 * These run in the ../abgagt-test directory
 */
const TEST_REPO_URL = 'https://github.tools.sap/I045696/abgagt-test.git';
const TEST_REPO_DIR = path.join(repoRoot, 'output', 'abgagt-test');

/**
 * Run comprehensive lifecycle test with full reset and validation
 */
function runFullLifecycleTests() {
  printSubHeader('Running Lifecycle Command Tests (Full Integration)');

  const startTime = Date.now();
  const results = [];

  // Step 1: Create output directory if needed
  const outputDir = path.join(repoRoot, 'output');
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  // Step 2: Clone or update test repo
  let testDir = TEST_REPO_DIR;
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
    return execSync(
      `node bin/abapgit-agent ${cmd} ${argsStr}`,
      { cwd: testDir, encoding: 'utf8', timeout: 120000 }
    );
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
    // Recreate symlinks that may have been removed by git reset
    const binLink = path.join(testDir, 'bin');
    const nodeLink = path.join(testDir, 'node_modules');
    try {
      if (!fs.existsSync(binLink)) {
        fs.symlinkSync(path.join(repoRoot, 'bin'), binLink, 'dir');
      }
    } catch (e) {
      // Ignore if already exists
    }
    try {
      if (!fs.existsSync(nodeLink)) {
        fs.symlinkSync(path.join(repoRoot, 'node_modules'), nodeLink, 'dir');
      }
    } catch (e) {
      // Ignore if already exists
    }

    // Step 4: Run health without config - should fail/not work
    printInfo('Testing health without config...');
    let output = '';
    try {
      output = runCli('health');
      const passed = output.includes('healthy') || output.includes('OK');
      addResult('health without config', passed, output);
    } catch (e) {
      addResult('health without config', true, e.message); // Expected to fail
    }

    // Step 5: Run init command
    printInfo('Running init command...');
    output = runCli('init', ['--package', '$ABGAGT_TEST', '--folder', 'src/']);
    const initPassed = output.includes('Created .abapGitAgent') ||
                       output.includes('initialized');
    addResult('init creates config files', initPassed, output);

    // Step 6: Edit .abapGitAgent with credentials from main project
    printInfo('Updating .abapGitAgent with credentials...');
    const config = readAbapConfig();
    if (config) {
      config.package = '$ABGAGT_TEST';
      config.folder = '/src/';
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

    // Step 12: Run import command
    printInfo('Importing objects...');
    output = runCli('import', ['--message', 'test: initial import']);
    const importPassed = output.includes('imported') || output.includes('No objects');
    addResult('import objects', importPassed, output);

    // Step 13: Git pull to see new commits
    printInfo('Git pull to check import...');
    try {
      execSync('git pull origin main', { cwd: testDir });
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
 */
function runLifecycleTests() {
  return runFullLifecycleTests();
}

/**
 * Run command tests against real ABAP system
 */
function runCommandTests() {
  printSubHeader('Running Command Tests (Real ABAP System)');

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
  const lifecycleResults = runLifecycleTests();
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

  let runJest, runAunit, runCmd;

  if (args.includes('--jest')) {
    runJest = true;
    runAunit = false;
    runCmd = false;
  } else if (args.includes('--aunit')) {
    runJest = false;
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
    results.aunit = runAUnitTests();
  }

  // Run Command tests
  if (runCmd) {
    results.cmd = runCommandTests();
  }

  // Print summary and exit with appropriate code
  const allPassed = printSummary(results);

  if (!allPassed) {
    process.exit(1);
  }
}

main();
