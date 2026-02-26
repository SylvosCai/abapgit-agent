# How to Verify CLI Output Matches Command Spec

## TL;DR - Yes, Unit Tests with Mocks Are Easier! ✅

**Your instinct is correct** - unit tests with mocked HTTP requests/responses are **much better** for verifying CLI output format because:

1. **Fast** - No real ABAP system needed (2s vs 70s)
2. **Deterministic** - Same mock always produces same output
3. **Complete coverage** - Test all scenarios (success, errors, warnings, edge cases)
4. **Controlled** - Craft exact ABAP responses to test each output format

## Current Challenge

The commands have some **testability issues** that make pure unit testing difficult:

1. **File system dependencies** - Commands check if files exist on disk
2. **Direct process.exit() calls** - Can't be mocked easily in tests
3. **Console output** - Need to capture stdout/stderr

## Solution: Test at Two Levels

### Level 1: Integration Tests (Current Approach) ✅

**What we have now:**
```javascript
// tests/integration/test-all.js
{
  command: 'inspect',
  args: ['--files', 'abap/zcl_abgagt_util.clas.abap'],
  verify: (output) => {
    return output.includes('Syntax check passed') &&
           output.includes('ZCL_ABGAGT_UTIL');
  }
}
```

**Pros:**
- Tests real integration with ABAP system
- No code changes needed
- Validates end-to-end flow

**Cons:**
- Slow (70+ seconds for full suite)
- Hard to test error scenarios
- Verification is basic (only checks for presence of strings)

### Level 2: Output Format Unit Tests (Recommended Addition) ✅

Create dedicated unit tests that:
1. Mock the `AbapHttp` class
2. Provide crafted ABAP responses
3. Verify output format matches CLAUDE.md spec exactly

**Example:**
```javascript
test('inspect output should match spec for passed syntax check', async () => {
  // Mock ABAP response
  const mockAbapResponse = {
    SUCCESS: true,
    RESULTS: [{
      OBJECT_TYPE: 'CLAS',
      OBJECT_NAME: 'ZCL_MY_CLASS',
      SUCCESS: true,
      MESSAGE: 'Syntax check passed'
    }]
  };

  // Mock context
  const mockContext = {
    AbapHttp: jest.fn().mockImplementation(() => ({
      fetchCsrfToken: jest.fn().mockResolvedValue('token'),
      post: jest.fn().mockResolvedValue(mockAbapResponse)
    }))
  };

  // Execute command
  const inspectCommand = require('../../src/commands/inspect');
  await inspectCommand.execute(['--files', 'test.clas.abap'], mockContext);

  // Verify format
  expect(consoleOutput).toMatch(/✅.*CLAS.*ZCL_MY_CLASS.*Syntax check passed/);
});
```

## Quick Win: Enhanced Integration Test Verification

The **easiest immediate improvement** without code changes is to enhance the `verify` functions in `tests/integration/test-all.js`:

### Before (Basic):
```javascript
verify: (output) => {
  return output.includes('Syntax check passed');
}
```

### After (Spec-Compliant):
```javascript
verify: (output) => {
  // Per CLAUDE.md spec, inspect output should have:
  // 1. Status emoji (✅/⚠️/❌)
  // 2. Object type (CLAS/INTF/PROG/etc.)
  // 3. Object name
  // 4. Status text
  const hasStatusEmoji = /[✅⚠️❌]/.test(output);
  const hasObjectType = /CLAS|INTF|PROG|FUGR|DDLS/.test(output);
  const hasStatusText = output.includes('Syntax check passed') ||
                        output.includes('Syntax check failed');
  const hasSeparator = !output.includes('failed') ||
                       output.includes('─'.repeat(20)); // Error section

  return hasStatusEmoji && hasObjectType && hasStatusText && hasSeparator;
}
```

## Recommended Testing Strategy

| Test Type | Purpose | When | Speed | Coverage |
|-----------|---------|------|-------|----------|
| **Integration tests** | Verify commands work with real ABAP | Pre-merge, CI | Slow (70s) | E2E flow |
| **Enhanced verify()** | Basic format checks | Every test run | Slow (70s) | Format basics |
| **Unit tests** (future) | Detailed format verification | Every commit | Fast (2s) | Format details |

## Implementation Steps

### Step 1: Enhance Integration Test Verification (Quick Win - 30 min)

I've already created `tests/integration/verify-output-spec.js` with spec-compliant verifiers.

**Use them in test-all.js:**
```javascript
const verifiers = require('./verify-output-spec');

const commandTestCases = [
  {
    command: 'inspect',
    args: ['--files', 'abap/zcl_abgagt_util.clas.abap'],
    verify: (output) => verifiers.verifyInspectOutput(output, 'ZCL_ABGAGT_UTIL')
  },
  {
    command: 'unit',
    args: ['--files', 'abap/zcl_abgagt_util.clas.testclasses.abap'],
    verify: (output) => verifiers.verifyUnitOutput(output, 'ZCL_ABGAGT_UTIL')
  }
  // ... etc
];
```

### Step 2: Create Standalone Output Spec Test Suite (30 min)

Run `tests/integration/test-output-spec.js` (already created) to verify all command outputs:

```bash
# Test all commands
node tests/integration/test-output-spec.js

# Test specific command
node tests/integration/test-output-spec.js inspect
```

This gives you a **dedicated test suite** focused only on output format compliance.

### Step 3: Add Unit Tests with Mocks (Future - 2-3 hours)

For commands that don't require file system:
- `health.js` ✅ (already has unit tests)
- `status.js` ✅ (already has unit tests)
- `list.js`, `view.js`, `preview.js`, `where.js`, `tree.js` - Can add mocked tests

For commands that DO require files (harder):
- `inspect.js`, `syntax.js`, `unit.js`, `pull.js`
- Would need refactoring to inject file system dependency

## What I've Created for You

1. **`tests/integration/verify-output-spec.js`** - Spec-compliant verifiers for each command
2. **`tests/integration/test-output-spec.js`** - Standalone output format test runner
3. **`tests/unit/output-format.test.js`** - Example unit tests (needs command refactoring)

## Immediate Next Step

**Run the output spec test suite:**

```bash
node tests/integration/test-output-spec.js
```

This will show you which commands match the CLAUDE.md spec and which don't.

## Answer to Your Question

**"Is it easier to use unit tests because you can mock the HTTP request and response?"**

**YES!** For output format verification specifically, unit tests with mocks are:
- ✅ Faster
- ✅ More reliable
- ✅ Better coverage
- ✅ Easier to write comprehensive format tests

But you **still need integration tests** to verify the commands actually work with a real ABAP system.

**Best approach:** Use both - integration tests for "does it work?", unit tests for "is the output format correct?"
