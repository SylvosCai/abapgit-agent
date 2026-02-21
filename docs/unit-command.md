# unit Command Requirements

## Overview

Run AUnit tests for ABAP test classes and display detailed results.

## Command

```bash
# Single test class file
abapgit-agent unit --files zcl_my_test.clas.testclasses.abap

# Multiple test class files
abapgit-agent unit --files zcl_test1.clas.testclasses.abap,zcl_test2.clas.testclasses.abap

# With path
abapgit-agent unit --files abap/zcl_my_test.clas.testclasses.abap
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Files must be test class files (`.testclasses.abap`)

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--files` | Yes | Comma-separated list of test class files |
| `--coverage` | No | Enable code coverage measurement |

---

## Coverage Option

When `--coverage` is specified, the command runs AUnit tests with code coverage enabled and displays coverage statistics.

```bash
# Run tests with coverage
abapgit-agent unit --files zcl_my_test.clas.testclasses.abap --coverage
```

## Tasks

### 1. Validate Parameters

- `--files` must be specified
- Files must be `.testclasses.abap` format

### 2. Load Configuration

Read `.abapGitAgent` for credentials

### 3. Fetch CSRF Token

```bash
GET /health (with X-CSRF-Token: fetch)
```

### 4. Make Unit Request

**Endpoint:** `POST /unit`

**Request Body:**
```json
{
  "files": ["ZCL_MY_TEST.CLASS.TESTCLASSES.ABAP"],
  "coverage": true
}
```

### 5. Display Results

---

## Output

### All Tests Passed

```
  Running unit tests for 2 file(s)

  ✅ ZCL_MY_TEST - All tests passed
     Tests: 10 | Passed: 10 | Failed: 0

  ✅ ZCL_OTHER_TEST - All tests passed
     Tests: 5 | Passed: 5 | Failed: 0
```

### With Failures

```
  Running unit tests for 1 file(s)

  ❌ ZCL_MY_TEST - Tests failed
     Tests: 10 | Passed: 8 | Failed: 2
     ✗ ZCL_MY_TEST=>TEST_METHOD_1: Error description
     ✗ ZCL_MY_TEST=>TEST_METHOD_2: Another error

Failed Tests:
────────────────────────────────────────────────────────────────────────────────
   ✗ ZCL_MY_TEST=>TEST_METHOD_1
     Error: Expected X but got Y
```

### With Coverage

```
  Running unit tests for 1 file(s) (with coverage)

  ✅ ZCL_MY_TEST - All tests passed
     Tests: 10 | Passed: 10 | Failed: 0
     📊 Coverage: 65.9%
        Total Lines: 41
        Covered Lines: 27
```

---

## Response Structure

```json
{
  "success": "X",
  "message": "2 of 10 tests failed",
  "test_count": 10,
  "passed_count": 8,
  "failed_count": 2,
  "errors": [
    {
      "class_name": "ZCL_MY_TEST",
      "method_name": "TEST_METHOD_1",
      "error_kind": "ERROR",
      "error_text": "Expected X but got Y"
    }
  ],
  "coverage_stats": {
    "total_lines": 41,
    "covered_lines": 27,
    "coverage_rate": 65.9
  }
}
```

---

## Error Handling

| Error | Message |
|-------|---------|
| File not found | `File not found: <path>` |
| Invalid format | `Invalid file format: <file>` |
| No --files specified | `Error: --files parameter required` |
| No tests found | `➖ <class> - No unit tests` |

---

## File Format

Test class files must end with `.testclasses.abap`:

| File | Test Class |
|------|------------|
| `zcl_my_test.clas.testclasses.abap` | ZCL_MY_TEST |
| `src/tests/zcl_my_test.clas.testclasses.abap` | ZCL_MY_TEST |

---

## Error Details

When a test fails, output includes:
- **Class**: The test class name
- **Method**: Failed test method name (with `=>` notation)
- **Error Kind**: Type of error (ERROR, FAILURE, etc.)
- **Error Text**: Detailed error message from AUnit

---

## Example

```bash
# Run tests
abapgit-agent unit --files zcl_my_test.clas.testclasses.abap

# Multiple
abapgit-agent unit --files abap/zcl_test1.clas.testclasses.abap,abap/zcl_test2.clas.testclasses.abap

# With coverage
abapgit-agent unit --files zcl_my_test.clas.testclasses.abap --coverage
```

## Implementation

Uses `CL_SUT_AUNIT_RUNNER` to execute tests:
- `S_CREATE` - Create test runner
- `run()` - Execute tests
- `str_results` - Get test statistics
- `tab_objects` - Get detailed results
- `get_coverage_result_stats` - Get coverage statistics (when coverage enabled)
- `p_cov = 'X'` - Enable coverage
- `p_cvrau` - Set coverage scope to specified_range
- `so_cvprg` - Program range for coverage measurement
