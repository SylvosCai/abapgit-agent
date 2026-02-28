# Pull Command - Tags & Branches Integration Tests

This directory contains integration tests for the `pull` command with different git references (tags and branches).

## Test Structure

All tests are located in `tests/integration/abap-commands.js`. The tests verify:
1. Pull succeeds from different git refs (tags/branches)
2. The activated code matches the expected version (verified using `view` command)

**Test flow:**
```
pull from v0.1.0 → view ZIF_SIMPLE_TEST → verify only get_message exists
pull from v1.0.0 → view ZIF_SIMPLE_TEST → verify get_message + validate_input exist
pull from feature/test-branch → view ZIF_SIMPLE_TEST → verify get_message + calculate_sum exist
pull from main → view ZIF_SIMPLE_TEST → verify get_message + validate_input exist
```

**Run all tests:**
```bash
npm run test:cmd:pull
```

**Run all command tests:**
```bash
npm run test:cmd
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

## Running Tests

```bash
# All pull tests (including tag/branch tests)
npm run test:cmd:pull

# All command tests
npm run test:cmd

# Specific command tests
npm run test:cmd -- --command=pull
```

## Expected Output

### Successful Test Run

```
======================================================================
  UNIFIED TEST SUITE
======================================================================

----------------------------------------------------------------------
  Running Command Tests (Real ABAP System)
----------------------------------------------------------------------
    Filtering tests for command: pull
  Testing: pull pull --files (specific file)... ✅
  Testing: pull pull from tag v0.1.0... ✅
  Testing: view verify v0.1.0 - has only get_message... ✅
  Testing: pull pull from tag v1.0.0... ✅
  Testing: view verify v1.0.0 - has get_message and validate_input... ✅
  Testing: pull pull from branch feature/test-branch... ✅
  Testing: view verify feature/test-branch - has get_message and calculate_sum... ✅
  Testing: pull pull from branch main... ✅
  Testing: view verify main - has get_message and validate_input... ✅
  ✅ Command tests: 9/9 passed (XX.Xs)

======================================================================
  TEST SUITE SUMMARY
======================================================================
  ✅ Command Tests: 9/9 PASSED (XX.Xs)

======================================================================
  ✅ ALL TESTS PASSED (Total: XX.Xs)
======================================================================
```

## Troubleshooting

### Test Fails with "INCORRECTLY PRESENT"

This means a method from a previous version is still present. This shouldn't happen with the fixed branch switching implementation, but if it does:
- The branch switching may not have worked correctly
- Check the abapGit logs for errors

**Solution:** Manually delete `ZIF_SIMPLE_TEST` in SE24/SE80 and re-run the test.

### Test Fails with "MISSING"

This means the expected method is not in the activated interface. Possible causes:
- Pull didn't activate the correct version
- Git ref doesn't exist or is incorrect
- Network/connectivity issue

**Solution:** Check git ref exists and re-run pull command manually:
```bash
cd /Users/i045696/Documents/code/project/abgagt-pull-test
node ../abapgit-agent/bin/abapgit-agent pull --branch v0.1.0
node ../abapgit-agent/bin/abapgit-agent view --objects ZIF_SIMPLE_TEST
```

### Wrong Package/Repository

If tests show wrong package, ensure the test is using the correct repository URL and `.abapGitAgent` configuration.

## Implementation Details

### How Branch Switching Works

The pull command now properly switches git references before deserializing:

1. **Refresh with cache drop** - `refresh(iv_drop_cache = abap_true)` fetches latest remote branches/tags
2. **Convert to full ref** - Converts short names (e.g., `v0.1.0`, `master`) to full git refs (`refs/tags/v0.1.0`, `refs/heads/master`)
3. **Select branch** - Calls `select_branch()` on the online repository
4. **Deserialize** - abapGit deserializes and activates the code from the selected ref

### Git Reference Format

abapGit expects full git reference names:
- Branches: `refs/heads/master`, `refs/heads/feature/test-branch`
- Tags: `refs/tags/v0.1.0`, `refs/tags/v1.0.0`

The agent automatically converts short names to full refs.

## CI/CD Integration

To run these tests in CI:

```bash
# In CI pipeline
npm run test:cmd:pull
```

## Known Limitations

1. **Sequential Execution**: Tests run sequentially, each pull replaces the previous version
2. **Test Repository Dependency**: Tests depend on the abgagt-pull-test repository structure
3. **Network Dependency**: Requires connectivity to both GitHub and ABAP system

## Future Enhancements

- [ ] Add performance timing metrics
- [ ] Test with multiple objects in one pull
- [ ] Add transport request verification
- [ ] Support class verification (not just interfaces)
