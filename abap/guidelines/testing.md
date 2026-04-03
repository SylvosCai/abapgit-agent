---
layout: default
title: Unit Testing
nav_order: 7
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Unit Testing

**Searchable keywords**: unit test, AUnit, test class, cl_abap_unit_assert, FOR TESTING, setup, teardown, RISK LEVEL, DURATION, WITH_UNIT_TESTS, testclasses

## TOPICS IN THIS FILE
1. Local Test Classes - line 22
2. File Structure - line 24
3. Required Elements - line 35
4. Naming Conventions - line 67
5. Common Mistake: DDLS Testing - line 133

## Unit Testing with Local Test Classes

### File Structure

For ABAP local unit tests, use a **separate file** with `.testclasses.abap` extension:

```
abap/
  zcl_my_class.clas.abap          <- Main class (no test code)
  zcl_my_class.clas.testclasses.abap  <- Local test class
  zcl_my_class.clas.xml           <- XML with WITH_UNIT_TESTS = X
```

### Required Elements

1. **Test class file** (`zcl_my_class.clas.testclasses.abap`):
   ```abap
   *"* use this source file for your test class implementation
   *"* local test class
   CLASS ltcl_zcl_my_class DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
     PRIVATE SECTION.
       DATA mo_cut TYPE REF TO zcl_my_class.
       METHODS setup.
       METHODS test_method1 FOR TESTING.
       METHODS test_method2 FOR TESTING.
   ENDCLASS.

   CLASS ltcl_zcl_my_class IMPLEMENTATION.
     METHOD setup.
       CREATE OBJECT mo_cut.
     ENDMETHOD.
     METHOD test_method1.
       " Test code using cl_abap_unit_assert
     ENDMETHOD.
   ENDCLASS.
   ```

2. **XML metadata** (`zcl_my_class.clas.xml`):
   ```xml
   <VSEOCLASS>
     ...
     <WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>
   </VSEOCLASS>
   ```

### Naming Conventions

- Test class name: `LTCL_ZCL_<CLASSNAME>` (e.g., `LTCL_ZCL_COMMAND_PULL`)
- Test methods: `TEST_<methodname> FOR TESTING` or simply `test_method FOR TESTING`
- Test file: `<classname>.clas.testclasses.abap`

### CRITICAL: Method Name Length Limit

**Test method names MUST NOT exceed 30 characters!**

```abap
" WRONG - 34 characters (syntax error)
METHODS test_execute_with_minimal_params FOR TESTING.

" CORRECT - 18 characters
METHODS test_exec_minimal FOR TESTING.
```

Examples of compliant names:
- `test_get_name` (13 chars)
- `test_exec_minimal` (18 chars)
- `test_exec_files` (16 chars)
- `test_interface` (15 chars)

### Test Methods and RAISING Clause

If a test method calls methods that raise exceptions, add `RAISING` to the method definition:

```abap
" CORRECT - declare that method can raise exceptions
METHODS test_validate_ddls FOR TESTING RAISING cx_static_check.
METHODS test_read_data FOR TESTING RAISING cx_dd_ddl_check.

" Then implement with TRY-CATCH if needed
METHOD test_validate_ddls.
  TRY.
      mo_cut->some_method( ).
    CATCH cx_static_check.
      " Handle exception
  ENDTRY.
ENDMETHOD.
```

### Common Assertions

```abap
cl_abap_unit_assert=>assert_equals( act = lv_actual exp = lv_expected msg = 'Error message' ).
cl_abap_unit_assert=>assert_not_initial( act = lv_data msg = 'Should not be initial' ).
cl_abap_unit_assert=>assert_bound( act = lo_ref msg = 'Should be bound' ).
cl_abap_unit_assert=>assert_true( act = lv_bool msg = 'Should be true' ).
```

### What NOT To Do

- ❌ Don't add test methods directly in the main `.clas.abap` file
- ❌ Don't use `CLASS ... DEFINITION ...` without the special comment header
- ❌ Don't reference `<TESTCLASS>` in XML - abapGit auto-detects `.testclasses.abap`
- ❌ Don't use nested local classes inside the main class definition

---

### ⚠️ Common Mistake: CDS Views Don't Have `.testclasses.abap` Files

**WRONG - Creating test file for DDLS**:
```
zc_my_view.ddls.asddls
zc_my_view.ddls.testclasses.abap  ❌ This doesn't work!
zc_my_view.ddls.xml
```

**Error you'll see**:
```
The REPORT/PROGRAM statement is missing, or the program type is INCLUDE.
```

**CORRECT - Test CDS views using separate CLAS test classes**:
```
zc_flight_revenue.ddls.asddls               ← CDS view definition
zc_flight_revenue.ddls.xml                  ← CDS metadata

zcl_test_flight_revenue.clas.abap           ← Test class definition
zcl_test_flight_revenue.clas.testclasses.abap  ← Test implementation
zcl_test_flight_revenue.clas.xml            ← Class metadata (WITH_UNIT_TESTS=X)
```

**Why**: Each ABAP object type has its own testing pattern:
- **CLAS** (classes): Use `.clas.testclasses.abap` for the same class
- **DDLS** (CDS views): Use separate CLAS test class with CDS Test Double Framework
- **FUGR** (function groups): Use `.fugr.testclasses.abap`
- **PROG** (programs): Use `.prog.testclasses.abap`

**Don't assume patterns from one object type apply to another!**

See "Unit Testing CDS Views" section below for the correct CDS testing approach.

---

### Running Tests

In ABAP: SE24 → Test → Execute Unit Tests

Or via abapGit: Pull the files and run tests in the ABAP system.

## Unit Testing CDS Views

For full CDS test double patterns (basic setup, aggregations, CDS-on-CDS), see:

```bash
abapgit-agent ref --topic cds-testing
```

---

## See Also
- **CDS Views** (cds.md) - for CDS view definitions and syntax
- **abapGit** (abapgit.md) - for WITH_UNIT_TESTS in XML metadata
- **ABAP SQL** (sql.md) - for SELECT statements in tests
