# Pull Command - Tags & Branches Integration Tests

This directory contains integration tests for the `pull` command with different git references (tags and branches).

## Test Structure

### 1. Basic Pull Tests (`abap-commands.js`)

Located in `tests/integration/abap-commands.js`, these tests verify that:
- Pull succeeds from tag `v0.1.0`
- Pull succeeds from tag `v1.0.0`
- Pull succeeds from branch `feature/test-branch`
- Pull succeeds from branch `main`

**Run with:**
```bash
npm run test:cmd:pull
```

These tests verify the pull command executes successfully but don't verify the activated code content (because they run from the main repo directory).

### 2. Full Verification Tests (`pull-refs-verification.js`)

Located in `tests/integration/pull-refs-verification.js`, this comprehensive test:
- Pulls from each git ref (tag/branch)
- Uses `view` command to inspect the activated code
- Verifies the correct methods exist in the activated interface
- Confirms version-specific methods are present/absent

**IMPORTANT**: This test must be run from the test repository directory:

```bash
cd /Users/i045696/Documents/code/project/abgagt-pull-test
node /Users/i045696/Documents/code/project/abapgit-agent/tests/integration/pull-refs-verification.js
```

## Test Repository

The tests use the repository: `https://github.tools.sap/I045696/abgagt-pull-test.git`

**Repository structure:**

| Git Ref | Methods in ZIF_SIMPLE_TEST |
|---------|----------------------------|
| v0.1.0 (tag) | `get_message` only |
| v1.0.0 (tag) | `get_message`, `validate_input` |
| feature/test-branch | `get_message`, `calculate_sum` |
| main | `get_message`, `validate_input` (same as v1.0.0) |

**ABAP Package:** `$ABGABT_PULL_TEST`
**Test Object:** `ZIF_SIMPLE_TEST` (interface)

## Verification Strategy

Each version of the interface has distinct method signatures:

### v0.1.0
```abap
INTERFACE zif_simple_test.
  METHODS get_message
    RETURNING VALUE(rv_message) TYPE string.
ENDINTERFACE.
```

### v1.0.0 / main
```abap
INTERFACE zif_simple_test.
  METHODS get_message
    RETURNING VALUE(rv_message) TYPE string.
  METHODS validate_input
    IMPORTING iv_input TYPE string
    RETURNING VALUE(rv_is_valid) TYPE abap_bool.
ENDINTERFACE.
```

### feature/test-branch
```abap
INTERFACE zif_simple_test.
  METHODS get_message
    RETURNING VALUE(rv_message) TYPE string.
  METHODS calculate_sum
    IMPORTING iv_num1 TYPE i
              iv_num2 TYPE i
    RETURNING VALUE(rv_result) TYPE i.
ENDINTERFACE.
```

The test verifies the correct version by checking:
- ✅ Expected methods ARE present
- ✅ Unexpected methods are NOT present

## Running All Tests

```bash
# Basic pull tests (from main repo)
cd /Users/i045696/Documents/code/project/abapgit-agent
npm run test:cmd:pull

# Full verification tests (from test repo)
cd /Users/i045696/Documents/code/project/abgagt-pull-test
node /Users/i045696/Documents/code/project/abapgit-agent/tests/integration/pull-refs-verification.js

# Or create a symlink for convenience
cd /Users/i045696/Documents/code/project/abgagt-pull-test
ln -s /Users/i045696/Documents/code/project/abapgit-agent/tests/integration/pull-refs-verification.js test-pull-refs.js
node test-pull-refs.js
```

## Expected Output

### Successful Test Run

```
╔════════════════════════════════════════════════════════════════════════════╗
║                                                                            ║
║  Pull Command - Git References Verification Test                          ║
║                                                                            ║
╚════════════════════════════════════════════════════════════════════════════╝

================================================================================
Testing: Tag v0.1.0 - only get_message method
  Ref: v0.1.0
================================================================================

📥 Pulling from v0.1.0...
✅ Pull completed successfully

📖 Viewing ZIF_SIMPLE_TEST...

🔍 Activated methods: get_message

✓ Expected methods:
  ✅ get_message - found

✓ Should NOT have:
  ✅ validate_input - correctly absent
  ✅ calculate_sum - correctly absent

================================================================================
✅ TEST PASSED: v0.1.0
================================================================================

[... similar output for v1.0.0, feature/test-branch, main ...]

================================================================================
TEST SUMMARY
================================================================================

  ✅ v0.1.0
  ✅ v1.0.0
  ✅ feature/test-branch
  ✅ main

4/4 tests passed

✅ ALL TESTS PASSED!
```

## Troubleshooting

### Test Fails with "INCORRECTLY PRESENT"

This means the previous version's methods are still in the interface. Possible causes:
- abapGit doesn't remove methods (only adds) for safety
- Interface needs manual deletion before pull
- Package version mismatch

**Solution:** Manually delete `ZIF_SIMPLE_TEST` in SE24/SE80 and re-run the test.

### Test Fails with "MISSING"

This means the expected method is not in the activated interface. Possible causes:
- Pull didn't activate the correct version
- Git ref doesn't exist or is incorrect
- Network/connectivity issue

**Solution:** Check git ref exists and re-run pull command manually.

### Wrong Package/Repository

If tests show wrong package, ensure you're running from the correct directory and `.abapGitAgent` points to the test repository.

## CI/CD Integration

To run these tests in CI:

```bash
# In CI pipeline
cd test-repo-directory
npm run test:cmd:pull  # Basic tests
node full-verification-script.js  # Full verification
```

## Known Limitations

1. **abapGit Merge Behavior**: abapGit's pull operation updates repository metadata but may not remove existing methods/code from active objects. This is by design to prevent accidental data loss.
   - **Impact**: Pulling v0.1.0 after v1.0.0 will NOT remove the `validate_input` method
   - **Workaround**: Delete the interface manually in SE24 before running verification tests
   - **Why**: abapGit protects against accidental code deletion by not force-overwriting active objects

2. **Directory Context**: Full verification requires running from test repo directory
3. **Timing**: May need short delay between pull and view commands
4. **Cleanup**: Test doesn't auto-cleanup; interface persists between test runs

## Future Enhancements

- [ ] Add auto-cleanup (delete interface before each test)
- [ ] Support class verification (not just interfaces)
- [ ] Add transport request verification
- [ ] Test with multiple objects in one pull
- [ ] Add performance timing metrics
