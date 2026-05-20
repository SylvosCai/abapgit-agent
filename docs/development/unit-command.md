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
| `--files` | Yes | Comma-separated list of `.testclasses.abap` files. Multiple files referring to the same parent class are de-duplicated automatically. |
| `--coverage` | No | Enable per-method statement coverage measurement |
| `--coverage-threshold <N>` | No | Fail (or warn) when a class's coverage is below N percent (0–100). Defaults to `coverage.threshold` from `.abapgit-agent.json`, or `0` (off). Requires `--coverage`. |
| `--coverage-mode <warn\|fail>` | No | Action when a class is below threshold: `fail` = exit 1 (default), `warn` = print warning only. Defaults to `coverage.mode` from `.abapgit-agent.json`. |
| `--junit-output <file>` | No | Write results as JUnit XML to this file |
| `--json` | No | Output results as JSON |
| `--verbose` | No | Show per-method execution times AND per-method coverage; also print raw HTTP error responses |

> **When using the CI pipeline**, coverage policy (`threshold`, `mode`, `excludes`) is read
> from `.abapgit-agent.json` instead of being passed as CLI flags — see
> [CI pipeline docs](../../abapgit-agent-ci/docs/jenkins-setup-guide.md) and
> [Project Configuration reference](../project-config.md#coverage).

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

Coverage statistics are emitted in two places:
- **Per-class totals** as `<properties>` on `<testsuite>`
- **Per-method totals** as `<properties>` on each `<testcase>` whose name maps
  to a covered production method (heuristic: strip leading `TEST_`, then look
  for an exact or interface-prefixed match in `coverage.methods`)

```xml
<testsuite name="ZCL_MY_TEST" tests="6" failures="1" errors="0">
  <properties>
    <property name="coverage.rate"          value="65.9"/>
    <property name="coverage.lines.total"   value="41"/>
    <property name="coverage.lines.covered" value="27"/>
  </properties>
  <testcase name="TEST_ADD" classname="ZCL_MY_TEST.LTCL_UNIT_TEST" time="0.001">
    <properties>
      <property name="coverage.method.lines.total"   value="3"/>
      <property name="coverage.method.lines.covered" value="3"/>
      <property name="coverage.method.rate"          value="100"/>
    </properties>
  </testcase>
  <testcase name="TEST_PARSE" classname="ZCL_MY_TEST.LTCL_UNIT_TEST" time="0.002">
    <properties>
      <property name="coverage.method.lines.total"   value="48"/>
      <property name="coverage.method.lines.covered" value="35"/>
      <property name="coverage.method.rate"          value="72.9"/>
    </properties>
  </testcase>
  <testcase name="TEST_NO_PROD_METHOD" classname="ZCL_MY_TEST.LTCL_UNIT_TEST" time="0.001"/>
  <testcase name="TEST_FAIL" classname="ZCL_MY_TEST.LTCL_UNIT_TEST" time="0.003">
    <failure type="failedAssertion" message="Assertion failed">Assertion failed&#10;Expected X but got Y</failure>
  </testcase>
  <testcase name="coverage_threshold" classname="ZCL_MY_TEST">
    <failure type="FAILURE" message="ZCL_MY_TEST: coverage 20% is below threshold 80%">...</failure>
  </testcase>
</testsuite>
```

> Test methods that don't map to a production method (e.g. helper-only tests)
> simply omit the `<properties>` block — there's no synthetic
> `coverage.method.rate=0` filler.

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

**Endpoint:** `POST /sap/bc/z_abapgit_agent/unit` (abapgit-agent backend)

**Content-Type:** `application/json`

**Request Body:**

```json
{
  "files":    ["src/zcl_my_test.clas.testclasses.abap"],
  "coverage": true
}
```

The backend (`ZCL_ABGAGT_COMMAND_UNIT`) drives `CL_AUCV_TASK` directly — see
[Implementation](#implementation) below for why this replaces the older ADT
`testruns` path.

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

Failed methods show the test class name (local class, e.g. `LTCL_...`), the assertion title, all detail lines, and a stack entry with line number when available. A "Failed Tests" summary is printed at the end.

```
  Running unit tests for 1 file(s)

  ❌ ZCL_MY_TEST - Tests failed
     Tests: 10 | Passed: 8 | Failed: 2

     ✗ LTCL_UNIT_TEST=>TEST_ADD_NEGATIVE
       Assertion failed
       Expected [-3] but got [-4]
         at ZCL_MY_TEST (line 28)

     ✗ LTCL_UNIT_TEST=>TEST_GREET_EMPTY_NAME
       Unexpected exception: CX_SY_ZERO_DIVIDE
       Divide by zero in method GREET
         at ZCL_MY_TEST (line 55)

Failed Tests:
────────────────────────────────────────────────────────────────────────────────
   ✗ LTCL_UNIT_TEST=>TEST_ADD_NEGATIVE
     Assertion failed
     Expected [-3] but got [-4]

   ✗ LTCL_UNIT_TEST=>TEST_GREET_EMPTY_NAME
     Unexpected exception: CX_SY_ZERO_DIVIDE
     Divide by zero in method GREET
```

### With Slow Tests

When any method takes longer than 0.1 seconds, a slowest-test line is shown automatically:

```
  ✅ ZCL_MY_TEST - All tests passed
     Tests: 5 | Passed: 5 | Failed: 0
     ⏱  Slowest: TEST_HEAVY_QUERY (1.230s), TEST_DB_READ (0.450s)
```

### With `--verbose`

Per-method execution time table is shown for each class:

```
  ✅ ZCL_MY_TEST - All tests passed
     Tests: 4 | Passed: 4 | Failed: 0
     Method times:
       ✓ TEST_ADD                                0.001s
       ✓ TEST_IS_EVEN                            0.000s
       ✓ TEST_GREET                              0.000s
       ✓ TEST_HEAVY_QUERY                        1.230s
```

### With Coverage

```
  Running unit tests for 1 file(s) (with coverage)

  ✅ ZCL_MY_TEST - All tests passed
     Tests: 10 | Passed: 10 | Failed: 0
     📊 Coverage: 65.9%   (27/41 statements)
```

### With Coverage + `--verbose`

`--verbose` adds the per-method coverage breakdown:

```
  ✅ ZCL_ABGAGT_UTIL - All tests passed
     Tests: 12 | Passed: 12 | Failed: 0
     📊 Coverage: 51.1%   (160/313 statements)
     Method coverage:
       GET_INSTANCE                                     3/3 (100.0%)
       ZIF_ABGAGT_UTIL~CONVERT_INDEX_TO_CM_SUFFIX       42/82 (51.2%)
       ZIF_ABGAGT_UTIL~DETECT_INCLUDE_INFO              36/47 (76.6%)
       ZIF_ABGAGT_UTIL~PARSE_FILE_TO_OBJECT             35/48 (72.9%)
       ZIF_ABGAGT_UTIL~GET_INCLUDE_DESCRIPTION           0/31 (0.0%)
       ...
```

---

## Response Structure

The CLI surfaces this shape internally (one entry per parent class) — used for
console output, JUnit emission, and `--json` mode.

```json
{
  "className": "ZCL_MY_TEST",
  "file":      "src/zcl_my_test.clas.testclasses.abap",
  "methods": [
    { "name": "TEST_METHOD_1", "testClassName": "LTCL_UNIT_TEST", "passed": true, "executionTime": 0.001 },
    {
      "name": "TEST_METHOD_FAIL",
      "testClassName": "LTCL_UNIT_TEST",
      "passed": false,
      "executionTime": 0.003,
      "kind": "failedAssertion",
      "title": "[FM02] Different values",
      "details": ["Expected [99] Actual [5]"],
      "stack": [{ "name": "ZCL_MY_TEST===========CCAU 42 TEST_METHOD_FAIL", "line": 42 }]
    }
  ],
  "coverage": {
    "totalStatements":    41,
    "executedStatements": 27,
    "statementRate":      65.9,
    "methods": [
      { "name": "ADD",        "totalStatements": 3,  "executedStatements": 3,  "rate": 100 },
      { "name": "PARSE",      "totalStatements": 48, "executedStatements": 35, "rate": 72.9 }
    ]
  }
}
```

If `--coverage` is not set, the `coverage` field is omitted. If a class has no
tests, `methods` is `[]` (the run still succeeds with exit code 0).

---

## Error Handling

| Error | Message |
|-------|---------|
| File not found | `File not found: <path>` |
| Invalid format | `Invalid file format: <file>` |
| No --files specified | `Error: --files parameter required` |
| No tests found | `➖ <class> - No unit tests` |
| SETUP/TEARDOWN failure | `❌ <class> - Tests failed` with synthetic `✗ SETUP` / `✗ TEARDOWN` entry |

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
- **Test class**: Local test class name (e.g. `LTCL_UNIT_TEST`) shown as `LTCL_...=>METHOD_NAME`
- **Title**: Assertion or exception title from ADT
- **Details**: All detail lines (may be multiple — e.g. expected vs actual, stack context)
- **Stack**: Class name and line number from the `<stackEntry>` element (when available)

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

The command calls the abapgit-agent ABAP backend, which runs AUnit and
queries SCV directly using SAP_BASIS-shipped APIs:

- **CLI** (`src/commands/unit.js`): single `POST /sap/bc/z_abapgit_agent/unit`
  with `{ files, coverage }`. Files referring to the same parent class are
  forwarded as-is — the backend de-duplicates them internally.
- **ABAP backend** (`ZCL_ABGAGT_COMMAND_UNIT`):
  - Resolves files to class names (`abap/zcl_foo.clas.testclasses.abap` →
    `ZCL_FOO`); de-duplicates.
  - Builds an `IF_AUNIT_LISTENER` instance to capture per-method outcomes
    (`method_start` / `method_end` / `assert_failure` / `cx_failure` / `rt_failure`).
  - Runs `CL_AUCV_TASK=>create( i_measure_coverage = abap_true )` with
    `add_associated_unit_tests( ... )` for the resolved classes, then
    `task->run( c_run_mode-external )`.
  - When `--coverage` is set, calls `task->get_coverage_measurement( )` →
    `measurement->build_selection_result( i_selection )` and walks the
    `IF_SCV_RESULT_NODE` tree. Each `block` node carries per-method statement
    totals via `node->get_coverage( ce_scv_coverage_type=>statement )`.
  - Returns the structured response shown in [Response Structure](#response-structure)
    serialised with `/ui2/cl_json` (`pretty_mode-low_case`).

> **Why a backend** rather than the older direct-ADT path? The ADT
> `cov:query` endpoint returns `total=0` on some systems even when coverage
> data is recorded in `COVRES` (kernel-level configuration drift). Calling
> `IF_SCV_MEASUREMENT.build_selection_result` directly bypasses that layer
> and consistently produces method-level results. See `memory/project_coverage_disabled_gze.md`
> for the trace that motivated this design.
