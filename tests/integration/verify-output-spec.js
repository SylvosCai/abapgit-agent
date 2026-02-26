/**
 * Output Verification Helpers - Match CLI output to command specifications
 *
 * These helpers ensure CLI output matches the format specified in CLAUDE.md
 */

/**
 * Verify inspect command output format
 *
 * Expected format from CLAUDE.md:
 * ✅ CLAS ZCL_MY_CLASS - Syntax check passed
 * OR
 * ⚠️  CLAS ZCL_MY_CLASS - Syntax check passed with warnings (2):
 * OR
 * ❌ CLAS ZCL_MY_CLASS - Syntax check failed (1 error(s)):
 */
function verifyInspectOutput(output, objectName) {
  const checks = {
    hasStatusEmoji: /[✅⚠️❌]/.test(output),
    hasObjectType: /CLAS|INTF|PROG|FUGR|DDLS/.test(output),
    hasObjectName: output.includes(objectName),
    hasStatusText: output.includes('Syntax check passed') ||
      output.includes('Syntax check failed') ||
      output.includes('with warnings')
  };

  // If failed, should have error/warning sections with separator
  if (output.includes('failed') || output.includes('warnings')) {
    checks.hasSeparator = output.includes('─'.repeat(20));
    checks.hasSection = output.includes('Errors:') ||
      output.includes('Warnings:') ||
      output.includes('Info:');
  }

  return Object.values(checks).every(v => v === true);
}

/**
 * Verify pull command output format
 *
 * Expected format from CLAUDE.md:
 * ✅ Pull completed successfully!
 *    Job ID: CAIS20260208115649
 *    Message: Pull completed successfully
 *
 * 📋 Pull Log (N messages):
 * ───────────────────────────────────────────────────────────────────────────────
 * Icon │ Object                      │ Message
 *
 * 📦 Activated Objects (N):
 * ───────────────────────────────────────────────────────────────────────────────
 * ✅ CLAS ZCL_MY_CLASS
 */
function verifyPullOutput(output) {
  const checks = {
    hasStatusEmoji: /[✅❌]/.test(output),
    hasJobId: output.includes('Job ID:') || output.includes('CAIS'),
    hasMessage: output.includes('Pull completed') || output.includes('Message:'),
    hasPullLog: output.includes('Pull Log') || output.includes('messages'),
    hasActivatedSection: output.includes('Activated Objects') || output.includes('📦'),
    hasSeparator: output.includes('─'.repeat(20))
  };

  return Object.values(checks).every(v => v === true);
}

/**
 * Verify unit command output format
 *
 * Expected format from CLAUDE.md:
 * ✅ ZCL_MY_TEST - All tests passed
 *    Tests: 10 | Passed: 10 | Failed: 0
 * OR
 * ❌ ZCL_MY_TEST - Tests failed
 *    Tests: 10 | Passed: 8 | Failed: 2
 *    ✗ ZCL_MY_TEST=>TEST_METHOD_1: Error description
 */
function verifyUnitOutput(output, className) {
  const checks = {
    hasStatusEmoji: /[✅❌➖]/.test(output),
    hasClassName: output.includes(className),
    hasTestCounts: output.includes('Tests:') &&
      output.includes('Passed:') &&
      output.includes('Failed:'),
    hasStatusText: output.includes('All tests passed') ||
      output.includes('Tests failed') ||
      output.includes('No unit tests')
  };

  // If failed, should show failed test details
  if (output.includes('Tests failed')) {
    checks.hasFailedTests = output.includes('✗') && output.includes('=>');
  }

  return Object.values(checks).every(v => v === true);
}

/**
 * Verify tree command output format
 *
 * Expected format from CLAUDE.md:
 * Package Tree: $ZMAIN_PACKAGE
 * ⬆️  Parent: $ZSAP_BASE
 *
 * 📦 $ZMAIN_PACKAGE
 *    ├─ 📦 $ZMAIN_SUB1
 *    └─ 📦 $ZMAIN_SUB2
 *
 * Summary
 * PACKAGES: 4
 * OBJECTS: 127
 */
function verifyTreeOutput(output, packageName) {
  const checks = {
    hasPackageName: output.includes(packageName),
    hasTreeIcon: output.includes('📦'),
    hasTreeStructure: output.includes('├─') || output.includes('└─'),
    hasSummary: output.includes('Summary'),
    hasPackageCount: output.includes('PACKAGES:'),
    hasObjectCount: output.includes('OBJECTS:')
  };

  return Object.values(checks).every(v => v === true);
}

/**
 * Verify syntax command output format
 *
 * Expected format from CLAUDE.md:
 * Syntax check for 1 file(s)
 *
 * ✅ CLAS ZCL_MY_CLASS - Syntax check passed
 *
 * ✅ All 1 object(s) passed syntax check
 */
function verifySyntaxOutput(output, objectName) {
  const checks = {
    hasFileCount: output.includes('Syntax check for') && output.includes('file(s)'),
    hasStatusEmoji: /[✅❌]/.test(output),
    hasObjectType: /CLAS|INTF|PROG/.test(output),
    hasObjectName: output.includes(objectName),
    hasStatusText: output.includes('Syntax check passed') ||
      output.includes('Syntax check failed'),
    hasSummary: output.includes('All') && output.includes('object(s)')
  };

  // If failed, should have error section with line numbers
  if (output.includes('failed')) {
    checks.hasErrorSection = output.includes('Errors:');
    checks.hasLineNumbers = /Line \d+/.test(output);
  }

  return Object.values(checks).every(v => v === true);
}

/**
 * Verify preview command output format
 *
 * Expected format from CLAUDE.md:
 * 📊 Preview: SFLIGHT (Table)
 *
 * ┌──────────┬────────┬──────────┐
 * │ CARRID   │ CONNID │ FLDATE   │
 * ├──────────┼────────┼──────────┤
 * │ AA       │ 0017   │ 20240201 │
 * └──────────┴────────┴──────────┘
 *
 * Showing 2 of 10 rows
 */
function verifyPreviewOutput(output, objectName) {
  const checks = {
    hasPreviewIcon: output.includes('📊'),
    hasObjectName: output.includes(objectName),
    hasTableBorders: output.includes('┌') && output.includes('┐') &&
      output.includes('├') && output.includes('┤'),
    hasHeaders: /│\s+\w+\s+│/.test(output),
    hasRowCount: output.includes('of') && output.includes('row')
  };

  return Object.values(checks).every(v => v === true);
}

/**
 * Verify view command output format
 *
 * Expected format from CLAUDE.md:
 * 📖 ZCL_MY_CLASS (Class)
 *    Class ZCL_MY_CLASS in $PACKAGE
 *
 * CLASS zcl_my_class DEFINITION PUBLIC.
 * ...
 */
function verifyViewOutput(output, objectName) {
  const checks = {
    hasViewIcon: output.includes('📖'),
    hasObjectName: output.includes(objectName),
    hasObjectType: /\(Class\)|\(Interface\)|\(Table\)|\(CDS View\)/.test(output),
    hasDescription: output.includes('in') || output.includes('DEFINITION'),
    hasSource: output.length > 100 // Should have actual source code
  };

  return Object.values(checks).every(v => v === true);
}

/**
 * Verify where command output format
 *
 * Expected format from CLAUDE.md:
 * 🔍 ZCL_MY_CLASS (CLAS)
 *    Found 5 reference(s):
 *
 * 1. ZCL_OTHER_CLASS → METHOD_NAME (CLAS)
 * 2. ZIF_INTERFACE (INTF)
 */
function verifyWhereOutput(output, objectName) {
  const checks = {
    hasSearchIcon: output.includes('🔍') || output.includes('❌'),
    hasObjectName: output.includes(objectName),
    hasObjectType: /\(CLAS\)|\(INTF\)|\(PROG\)/.test(output),
    hasReferenceCount: output.includes('reference(s)') ||
      output.includes('No references') ||
      output.includes('Found')
  };

  // If references found, should show numbered list
  if (output.includes('Found') && !output.includes('No references')) {
    checks.hasNumberedList = /\d+\.\s+\w+/.test(output);
  }

  return Object.values(checks).every(v => v === true);
}

/**
 * Verify list command output format
 *
 * Actual format (grouped by type):
 * Objects in SAPBC_DATAMODEL (Total: 299)
 *
 *   AUTH (1)
 *     CARRID
 *
 *   DOMA (71)
 *     CITY
 *     POSTCODE
 */
function verifyListOutput(output, packageName) {
  const checks = {
    hasPackageName: output.includes(packageName),
    hasObjectsText: output.includes('Objects in'),
    // Total: is optional - only shown with pagination or filtering
    hasGroupedFormat: /[A-Z]{2,4}\s+\(\d+\)/.test(output), // e.g., "CLAS (5)" or "AUTH (1)"
    hasIndentation: /\n\s{2,}[A-Z0-9_$]/.test(output) // Indented object names
  };

  return Object.values(checks).every(v => v === true);
}

/**
 * Run all verification checks and report results
 */
function verifyAllOutputs(testResults) {
  const verificationResults = [];

  for (const test of testResults) {
    let verified = false;
    let verifier = null;

    // Select appropriate verifier based on command
    switch (test.command) {
      case 'inspect':
        verifier = 'verifyInspectOutput';
        verified = verifyInspectOutput(test.output, test.objectName);
        break;
      case 'pull':
        verifier = 'verifyPullOutput';
        verified = verifyPullOutput(test.output);
        break;
      case 'unit':
        verifier = 'verifyUnitOutput';
        verified = verifyUnitOutput(test.output, test.objectName);
        break;
      case 'tree':
        verifier = 'verifyTreeOutput';
        verified = verifyTreeOutput(test.output, test.packageName);
        break;
      case 'syntax':
        verifier = 'verifySyntaxOutput';
        verified = verifySyntaxOutput(test.output, test.objectName);
        break;
      case 'preview':
        verifier = 'verifyPreviewOutput';
        verified = verifyPreviewOutput(test.output, test.objectName);
        break;
      case 'view':
        verifier = 'verifyViewOutput';
        verified = verifyViewOutput(test.output, test.objectName);
        break;
      case 'where':
        verifier = 'verifyWhereOutput';
        verified = verifyWhereOutput(test.output, test.objectName);
        break;
      case 'list':
        verifier = 'verifyListOutput';
        verified = verifyListOutput(test.output, test.packageName);
        break;
      default:
        verifier = 'none';
        verified = true; // Skip verification for unknown commands
    }

    verificationResults.push({
      command: test.command,
      name: test.name,
      verifier,
      verified,
      output: test.output
    });
  }

  return verificationResults;
}

module.exports = {
  verifyInspectOutput,
  verifyPullOutput,
  verifyUnitOutput,
  verifyTreeOutput,
  verifySyntaxOutput,
  verifyPreviewOutput,
  verifyViewOutput,
  verifyWhereOutput,
  verifyListOutput,
  verifyAllOutputs
};
