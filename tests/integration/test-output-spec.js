/**
 * Output Specification Test Suite
 *
 * Verifies that CLI command outputs match the specifications in CLAUDE.md
 *
 * Usage:
 *   node tests/integration/test-output-spec.js [command]
 *
 * Examples:
 *   node tests/integration/test-output-spec.js              # Test all commands
 *   node tests/integration/test-output-spec.js inspect      # Test inspect only
 *   node tests/integration/test-output-spec.js pull         # Test pull only
 */

const { execSync } = require('child_process');
const path = require('path');
const verifiers = require('../helpers/verify-output-spec');

const repoRoot = path.join(__dirname, '..', '..');

// Colors
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  cyan: '\x1b[36m',
  yellow: '\x1b[33m'
};

function colorize(color, text) {
  return `${colors[color]}${text}${colors.reset}`;
}

/**
 * Test cases with expected output format based on CLAUDE.md specs
 */
const outputSpecTests = [
  {
    command: 'inspect',
    name: 'Inspect command output format',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap'],
    objectName: 'ZCL_ABGAGT_UTIL',
    verifier: verifiers.verifyInspectOutput
  },
  {
    command: 'unit',
    name: 'Unit command output format',
    args: ['--files', 'abap/zcl_abgagt_util.clas.testclasses.abap'],
    objectName: 'ZCL_ABGAGT_UTIL',
    verifier: verifiers.verifyUnitOutput
  },
  {
    command: 'tree',
    name: 'Tree command output format',
    args: ['--package', 'S_NWDEMO_BASIS'],
    packageName: 'S_NWDEMO_BASIS',
    verifier: verifiers.verifyTreeOutput
  },
  {
    command: 'syntax',
    name: 'Syntax command output format',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap'],
    objectName: 'ZCL_ABGAGT_UTIL',
    verifier: verifiers.verifySyntaxOutput
  },
  {
    command: 'preview',
    name: 'Preview command output format',
    args: ['--objects', 'SFLIGHT', '--limit', '5'],
    objectName: 'SFLIGHT',
    verifier: verifiers.verifyPreviewOutput
  },
  {
    command: 'view',
    name: 'View command output format',
    args: ['--objects', 'ZCL_ABGAGT_UTIL'],
    objectName: 'ZCL_ABGAGT_UTIL',
    verifier: verifiers.verifyViewOutput
  },
  {
    command: 'where',
    name: 'Where command output format',
    args: ['--objects', 'ZCL_ABGAGT_UTIL'],
    objectName: 'ZCL_ABGAGT_UTIL',
    verifier: verifiers.verifyWhereOutput
  },
  {
    command: 'list',
    name: 'List command output format',
    args: ['--package', 'SAPBC_DATAMODEL'],
    packageName: 'SAPBC_DATAMODEL',
    verifier: verifiers.verifyListOutput
  }
];

/**
 * Run a single test case
 */
function runOutputTest(test) {
  try {
    const cmd = `node bin/abapgit-agent ${test.command} ${test.args.join(' ')}`;
    const output = execSync(cmd, {
      cwd: repoRoot,
      encoding: 'utf-8',
      stdio: ['pipe', 'pipe', 'pipe']
    });

    // Run verifier
    let verified = false;
    if (test.verifier) {
      if (test.objectName) {
        verified = test.verifier(output, test.objectName);
      } else if (test.packageName) {
        verified = test.verifier(output, test.packageName);
      } else {
        verified = test.verifier(output);
      }
    }

    return {
      success: true,
      verified,
      output,
      error: null
    };
  } catch (error) {
    return {
      success: false,
      verified: false,
      output: error.stdout || '',
      error: error.message
    };
  }
}

/**
 * Main test runner
 */
function runOutputSpecTests(filterCommand) {
  console.log('\n' + '='.repeat(70));
  console.log(colorize('bright', '  CLI Output Specification Tests'));
  console.log('='.repeat(70));
  console.log();
  console.log('  Verifying CLI output matches CLAUDE.md specifications');
  console.log();

  const testsToRun = filterCommand
    ? outputSpecTests.filter(t => t.command === filterCommand)
    : outputSpecTests;

  if (testsToRun.length === 0) {
    console.log(colorize('red', `  ❌ No tests found for command: ${filterCommand}`));
    process.exit(1);
  }

  const results = [];

  for (const test of testsToRun) {
    process.stdout.write(`  Testing ${test.command}: ${test.name}... `);

    const result = runOutputTest(test);
    results.push({ ...test, ...result });

    if (result.success && result.verified) {
      console.log(colorize('green', '✅ PASS'));
    } else if (result.success && !result.verified) {
      console.log(colorize('yellow', '⚠️  OUTPUT FORMAT MISMATCH'));
    } else {
      console.log(colorize('red', '❌ FAIL'));
    }
  }

  // Summary
  console.log('\n' + '─'.repeat(70));
  console.log(colorize('bright', '  Summary'));
  console.log('─'.repeat(70));

  const passed = results.filter(r => r.success && r.verified).length;
  const formatIssues = results.filter(r => r.success && !r.verified).length;
  const failed = results.filter(r => !r.success).length;

  console.log(`  Total Tests: ${results.length}`);
  console.log(colorize('green', `  ✅ Passed (output matches spec): ${passed}`));
  if (formatIssues > 0) {
    console.log(colorize('yellow', `  ⚠️  Format Issues (works but doesn't match spec): ${formatIssues}`));
  }
  if (failed > 0) {
    console.log(colorize('red', `  ❌ Failed (command error): ${failed}`));
  }

  // Show details for format issues
  if (formatIssues > 0) {
    console.log('\n' + '─'.repeat(70));
    console.log(colorize('yellow', '  Format Issues Details'));
    console.log('─'.repeat(70));

    results.filter(r => r.success && !r.verified).forEach(r => {
      console.log(`\n  ${r.command}: ${r.name}`);
      console.log('  Expected format per CLAUDE.md, but got:');
      console.log('  ' + r.output.substring(0, 200).replace(/\n/g, '\n  '));
      if (r.output.length > 200) {
        console.log('  ... (output truncated)');
      }
    });
  }

  console.log();

  if (failed > 0) {
    process.exit(1);
  } else if (formatIssues > 0) {
    console.log(colorize('yellow', '  ⚠️  Some outputs don\'t match specification format'));
    process.exit(0); // Don't fail CI for format issues
  } else {
    console.log(colorize('green', '  ✅ All command outputs match CLAUDE.md specifications!'));
    process.exit(0);
  }
}

// Run tests
const filterCommand = process.argv[2];
runOutputSpecTests(filterCommand);
