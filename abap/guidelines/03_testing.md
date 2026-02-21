# Unit Testing

**Searchable keywords**: unit test, AUnit, test class, cl_abap_unit_assert, FOR TESTING, setup, teardown, RISK LEVEL, DURATION, CDS test double, CL_CDS_TEST_ENVIRONMENT

## TOPICS IN THIS FILE
1. Local Test Classes - line 3
2. File Structure - line 5
3. Required Elements - line 16
4. Naming Conventions - line 48
5. CDS Test Doubles - line 94
6. CDS with Aggregations - line 178

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
    " IMPORTANT: Use interface type, not class type!
    DATA mo_cds_env TYPE REF TO if_cds_test_environment.

    " IMPORTANT: class_setup/teardown must be CLASS-METHODS (static)!
    CLASS-DATA mo_cds_env_static TYPE REF TO if_cds_test_environment.

    METHODS setup.
    METHODS test_cds_with_doubles FOR TESTING.

    CLASS-METHODS: class_setup,
                   class_teardown.

ENDCLASS.

"-------------------------
" CLASS IMPLEMENTATION
"-------------------------
CLASS ltcl_cds_test IMPLEMENTATION.

  METHOD class_setup.
    " Create CDS test environment - framework auto-creates doubles for dependencies
    mo_cds_env_static = cl_cds_test_environment=>create(
      i_for_entity = 'ZC_MY_CDS_VIEW' ).
  ENDMETHOD.

  METHOD class_teardown.
    " Clean up test environment
    mo_cds_env_static->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " IMPORTANT: Assign static env to instance and clear doubles
    mo_cds_env = mo_cds_env_static.
    mo_cds_env->clear_doubles( ).
  ENDMETHOD.

  METHOD test_cds_with_doubles.
    " IMPORTANT: Must declare table type first, cannot inline in VALUE!
    DATA lt_test_data TYPE TABLE OF zc_my_cds_view WITH EMPTY KEY.
    lt_test_data = VALUE #(
      ( field1 = 'A' field2 = 100 )
      ( field1 = 'B' field2 = 200 ) ).

    " Insert test data using named parameter
    mo_cds_env->insert_test_data( i_data = lt_test_data ).

    " Select from CDS view
    SELECT * FROM zc_my_cds_view INTO TABLE @DATA(lt_result).

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

### Testing CDS Views with Aggregations (SUM, COUNT, GROUP BY)

For CDS views with aggregations, insert test data into the **base tables** (SFLIGHT, SCARR, SBOOK), not directly into the CDS view:

```abap
METHOD test_aggregation.
  " Insert data into base tables via CDS test doubles
  DATA lt_scarr TYPE TABLE OF scarr WITH EMPTY KEY.
  lt_scarr = VALUE #( ( carrid = 'LH' carrname = 'Lufthansa' currcode = 'EUR' ) ).
  mo_cds_env->insert_test_data( i_data = lt_scarr ).

  DATA lt_sflight TYPE TABLE OF sflight WITH EMPTY KEY.
  lt_sflight = VALUE #( ( carrid = 'LH' connid = '0400' fldate = '20240115'
                          seatsmax = 200 seatsocc = 100 ) ).
  mo_cds_env->insert_test_data( i_data = lt_sflight ).

  DATA lt_sbook TYPE TABLE OF sbook WITH EMPTY KEY.
  lt_sbook = VALUE #(
    ( carrid = 'LH' connid = '0400' fldate = '20240115' bookid = '0001' forcuram = 1000 )
    ( carrid = 'LH' connid = '0400' fldate = '20240115' bookid = '0002' forcuram = 2000 )
    ( carrid = 'LH' connid = '0400' fldate = '20240115' bookid = '0003' forcuram = 3000 ) ).
  mo_cds_env->insert_test_data( i_data = lt_sbook ).

  " Select from CDS view - aggregations will use test double data
  SELECT * FROM zc_flight_revenue INTO TABLE @DATA(lt_result).

  " Verify aggregations
  cl_abap_unit_assert=>assert_equals(
    exp = 3
    act = lt_result[ 1 ]-numberofbookings
    msg = 'Should have 3 bookings' ).

  cl_abap_unit_assert=>assert_equals(
    exp = '6000.00'
    act = lt_result[ 1 ]-totalrevenue
    msg = 'Total revenue should be 6000.00' ).
ENDMETHOD.
```

### Key Classes for CDS Testing

| Item | Type/Usage |
|------|------------|
| `CL_CDS_TEST_ENVIRONMENT` | Class with `CREATE` method |
| `IF_CDS_TEST_ENVIRONMENT` | Interface (CREATE returns this type) |
| `CLASS-METHODS` | `class_setup` and `class_teardown` must be static methods |
| `CL_OSQL_TEST_ENVIRONMENT` | Test doubles for database tables (use for aggregations) |
| `CL_ABAP_UNIT_ASSERT` | Assertions |

### Key Methods

| Method | Purpose |
|--------|---------|
| `CL_CDS_TEST_ENVIRONMENT=>create( i_for_entity = ... )` | Create test environment (returns `if_cds_test_environment`) |
| `insert_test_data( i_data = ... )` | Insert test data into test doubles |
| `clear_doubles` | Clear test data before each test method |
| `destroy` | Clean up after test class |

### Important Usage Notes

1. **Use interface type**: `DATA mo_cds_env TYPE REF TO if_cds_test_environment` - the CREATE method returns an interface reference
2. **CLASS-METHODS required**: `class_setup` and `class_teardown` must be declared with `CLASS-METHODS` (not `METHODS`)
3. **Table type declaration**: Must declare `DATA lt_tab TYPE TABLE OF <type> WITH EMPTY KEY` before using `VALUE #()`
4. **Auto-created dependencies**: CDS framework auto-creates test doubles for base tables - do not specify `i_dependency_list`
5. **Aggregations**: For CDS views with SUM/COUNT/GROUP BY, insert test data into base tables (SFLIGHT, SCARR, etc.)
6. **Clear doubles**: Call `clear_doubles` in `setup` method before each test
7. **Enable associations**: Set `test_associations = 'X'` only if testing CDS associations
8. **Exception handling**: Declare test methods with `RAISING cx_static_check` for proper exception handling

### Search Reference for More Details

```bash
abapgit-agent ref "cl_cds_test_environment"
abapgit-agent ref --topic unit-tests
```

---

## See Also
- **CDS Views** (04_cds.md) - for CDS view definitions and syntax
- **abapGit** (08_abapgit.md) - for WITH_UNIT_TESTS in XML metadata
- **ABAP SQL** (01_sql.md) - for SELECT statements in tests
