---
layout: default
title: unit - Run Tests
nav_order: 5
parent: Development Commands
---

# unit Command Requirements

## Overview

Run AUnit tests for ABAP test classes and display detailed results.

## Command

```bash
# Single test class file
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap

# Multiple test class files
abapgit-agent unit --files src/zcl_test1.clas.testclasses.abap,src/zcl_test2.clas.testclasses.abap

# With path
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Files must be test class files (`.testclasses.abap`)

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--files` | Yes | Comma-separated list of `.testclasses.abap` files |
| `--coverage` | No | Enable code coverage measurement |
| `--coverage-threshold <N>` | No | Fail (or warn) when a class's coverage is below N percent (0–100). Default: `0` (off). Requires `--coverage`. |
| `--coverage-mode <warn\|fail>` | No | Action when a class is below threshold: `fail` = exit 1 (default), `warn` = print warning only |
| `--junit-output <file>` | No | Write results as JUnit XML to this file |
| `--json` | No | Output results as JSON |
| `--verbose` | No | Print raw response body on HTTP errors |

> **When using the CI pipeline**, coverage policy (`threshold`, `mode`, `excludes`) is read
> from `.abapgit-agent.json` instead of being passed as CLI flags — see
> [CI pipeline docs](../../abapgit-agent-ci/docs/jenkins-setup-guide.md).

---

## Coverage Option

When `--coverage` is specified, the command runs AUnit tests with code coverage enabled and displays per-class coverage statistics.

```bash
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap --coverage
```

### Coverage Threshold

Use `--coverage-threshold` to enforce a minimum coverage percentage per class. When a class falls below the threshold, a `coverage_threshold` failure is injected into that class's JUnit testsuite so the failure is clearly attributed.

```bash
# Fail the build if any class is below 80% coverage
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap \
  --coverage --coverage-threshold 80

# Warn instead of failing
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap \
  --coverage --coverage-threshold 80 --coverage-mode warn
```

**Output when below threshold (fail mode):**
```
  ✅ ZCL_MY_TEST - All tests passed
     Tests: 5 | Passed: 5 | Failed: 0
     📊 Coverage: 20%
❌ ZCL_MY_TEST: coverage 20% is below threshold 80%
```

The threshold is evaluated **per class** — a class that meets the threshold is unaffected even if another class fails the gate.

---

## JUnit Output

Use `--junit-output` to write results as JUnit XML, suitable for CI systems like Jenkins.

```bash
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap \
  --coverage --coverage-threshold 80 \
  --junit-output reports/unit-results.xml
```

When a class fails the coverage threshold in `fail` mode, a `coverage_threshold` `<failure>` element is injected into that class's `<testsuite>` in the XML — so the CI test report shows the failure alongside the class it belongs to, not as a separate unrelated node.

Coverage statistics are also emitted as `<properties>` on each testsuite:

```xml
<testsuite name="ZCL_MY_TEST" tests="6" failures="1" errors="0">
  <properties>
    <property name="coverage.rate"          value="20"/>
    <property name="coverage.lines.total"   value="10"/>
    <property name="coverage.lines.covered" value="2"/>
  </properties>
  <testcase name="(5 passing test(s))" classname="ZCL_MY_TEST"/>
  <testcase name="coverage_threshold" classname="ZCL_MY_TEST">
    <failure type="FAILURE" message="ZCL_MY_TEST: coverage 20% is below threshold 80%">...</failure>
  </testcase>
</testsuite>
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
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap

# Multiple files
abapgit-agent unit --files src/zcl_test1.clas.testclasses.abap,src/zcl_test2.clas.testclasses.abap

# With coverage
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap --coverage

# Fail build if coverage below 80%
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap \
  --coverage --coverage-threshold 80

# Write JUnit XML for CI
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap \
  --coverage --coverage-threshold 80 \
  --junit-output reports/unit-results.xml
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
