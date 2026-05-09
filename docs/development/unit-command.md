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
| `--verbose` | No | Show per-method execution times; also print raw HTTP error responses |

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

Coverage statistics are also emitted as `<properties>` on each testsuite:

```xml
<testsuite name="ZCL_MY_TEST" tests="6" failures="1" errors="0">
  <properties>
    <property name="coverage.rate"          value="20"/>
    <property name="coverage.lines.total"   value="10"/>
    <property name="coverage.lines.covered" value="2"/>
  </properties>
  <testcase name="TEST_METHOD_1" classname="ZCL_MY_TEST.LTCL_UNIT_TEST" time="0.001"/>
  <testcase name="TEST_METHOD_2" classname="ZCL_MY_TEST.LTCL_UNIT_TEST" time="0.002"/>
  <testcase name="TEST_METHOD_FAIL" classname="ZCL_MY_TEST.LTCL_UNIT_TEST" time="0.003">
    <failure type="failedAssertion" message="Assertion failed">Assertion failed&#10;Expected X but got Y</failure>
  </testcase>
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

**Endpoint:** `POST /sap/bc/adt/abapunit/testruns` (standard SAP ADT endpoint)

**Content-Type:** `application/*`
**Accept:** `application/*`

**Request Body (XML):**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<aunit:runConfiguration xmlns:aunit="http://www.sap.com/adt/aunit">
  <external>
    <coverage active="false"/>
  </external>
  <options>
    <uriType value="semantic"/>
    <testDeterminationStrategy sameProgram="true" assignedTests="false"/>
    <testRiskLevels harmless="true" dangerous="false" critical="false"/>
    <testDurations short="true" medium="false" long="false"/>
    <withNavigationUri enabled="true"/>
  </options>
  <adtcore:objectSets xmlns:adtcore="http://www.sap.com/adt/core">
    <objectSet kind="inclusive">
      <adtcore:objectReferences>
        <adtcore:objectReference adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_test"/>
      </adtcore:objectReferences>
    </objectSet>
  </adtcore:objectSets>
</aunit:runConfiguration>
```

ADT URI is derived from the file name: `zcl_my_test.clas.testclasses.abap` → `/sap/bc/adt/oo/classes/zcl_my_test`.

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
     📊 Coverage: 65.9%
        Total Lines: 41
        Covered Lines: 27
```

---

## Response Structure

```json
{
  "className": "ZCL_MY_TEST",
  "methods": [
    { "name": "TEST_METHOD_1", "testClassName": "LTCL_UNIT_TEST", "passed": true, "executionTime": 0.001 },
    {
      "name": "TEST_METHOD_FAIL",
      "testClassName": "LTCL_UNIT_TEST",
      "passed": false,
      "executionTime": 0.003,
      "kind": "failedAssertion",
      "title": "Assertion failed",
      "details": ["Expected X but got Y", "at ZCL_MY_TEST line 42"],
      "stack": [{ "name": "ZCL_MY_TEST", "line": 42 }]
    }
  ],
  "coverageStats": {
    "totalLines": 41,
    "coveredLines": 27,
    "coverageRate": 65.9
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

Calls the standard SAP ADT endpoint directly — no custom ABAP backend required:
- `POST /sap/bc/adt/abapunit/testruns` with `application/*` content-type
- One request per test class file
- ADT URI derived from file name: `zcl_my_test.clas.testclasses.abap` → `/sap/bc/adt/oo/classes/zcl_my_test`
- Response is XML (`<aunit:runResult>`); each `<testMethod>` element is present for both passing and failing tests
- Test class name (`LTCL_...`) comes from the parent `<testClass adtcore:name>` element
- Execution time (`executionTime` attribute in seconds) is parsed per method; slow tests (> 0.1s) are surfaced automatically
- Failure detail includes all `<detail text="...">` elements and `<stackEntry>` line numbers
- Coverage stats (`lines_total`, `lines_covered`, `coverage_rate`) are embedded in the same response when `<coverage active="true"/>`
