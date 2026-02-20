# Unit Testing

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

### Running Tests

In ABAP: SE24 → Test → Execute Unit Tests

Or via abapGit: Pull the files and run tests in the ABAP system.

## Unit Testing CDS Views

When testing code that uses CDS view entities, you can use the **CDS Test Double Framework** (`CL_CDS_TEST_ENVIRONMENT`) to create test doubles for CDS views. This allows you to inject test data without affecting production data.

### When to Use CDS Test Doubles

- Testing code that reads from CDS views
- Need controlled test data (not production data)
- Testing CDS view logic with specific scenarios

### CDS Test Double Framework

Use `CL_CDS_TEST_ENVIRONMENT` for controlled test data:

```abap
"-------------------------
" CLASS DEFINITION
"-------------------------
CLASS ltcl_cds_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cds_env TYPE REF TO cl_cds_test_environment.

    METHODS setup.
    METHODS class_setup.
    METHODS class_teardown.
    METHODS test_cds_with_doubles FOR TESTING.

ENDCLASS.

"-------------------------
" CLASS IMPLEMENTATION
"-------------------------
CLASS ltcl_cds_test IMPLEMENTATION.

  METHOD class_setup.
    " Create CDS test environment for the CDS view entity
    mo_cds_env = cl_cds_test_environment=>create(
      i_for_entity = 'ZC_MY_CDS_VIEW'
      test_associations = 'X' ).  " Enable if testing associations
  ENDMETHOD.

  METHOD class_teardown.
    " Clean up test environment
    mo_cds_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " Clear test doubles before each test
    mo_cds_env->clear_doubles( ).
  ENDMETHOD.

  METHOD test_cds_with_doubles.
    " Prepare test data
    DATA(lt_test_data) = VALUE zc_my_cds_view(
      ( field1 = 'A' field2 = 100 )
      ( field1 = 'B' field2 = 200 ) ).

    " Insert test data into the CDS view test double
    mo_cds_env->insert_test_data( lt_test_data ).

    " Call the method under test (which reads from CDS view)
    DATA(lt_result) = mo_cut->get_data_from_cds( ).

    " Verify results
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_result
      msg = 'Result should not be empty' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 2
      msg = 'Expected 2 rows' ).
  ENDMETHOD.

ENDCLASS.
```

### Key Classes for CDS Testing

| Class | Purpose |
|-------|---------|
| `CL_CDS_TEST_ENVIRONMENT` | Create test doubles for CDS view entities |
| `CL_CDS_GET_DATA_SET_ENVIRONMENT` | Alternative for CDS views as DOC (dependent-on components) |
| `CL_OSQL_TEST_ENVIRONMENT` | Test doubles for database tables |
| `CL_ABAP_UNIT_ASSERT` | Assertions |

### Key Methods

| Method | Purpose |
|--------|---------|
| `CL_CDS_TEST_ENVIRONMENT=>CREATE` | Create test environment for a CDS view |
| `INSERT_TEST_DATA` | Insert test data into the test double |
| `CLEAR_DOUBLES` | Clear test data before each test method |
| `DESTROY` | Clean up after test class |

### Important **Use test doubles Notes

1. when**: You need controlled data, not production data
2. **Enable associations**: Set `test_associations = 'X'` if testing CDS associations
3. **Always clear doubles**: Call `clear_doubles` in `setup` method

### Search Reference for More Details

```bash
abapgit-agent ref "cl_cds_test_environment"
abapgit-agent ref --topic unit-tests
```
