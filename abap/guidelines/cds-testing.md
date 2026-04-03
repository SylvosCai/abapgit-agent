---
layout: default
title: CDS Testing
nav_order: 10
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# CDS Testing

**Searchable keywords**: CDS test double, CL_CDS_TEST_ENVIRONMENT, IF_CDS_TEST_ENVIRONMENT, insert_test_data, clear_doubles, create_for_multiple_cds, class_setup, class_teardown, aggregation test, CDS unit test

## TOPICS IN THIS FILE
1. When to Use CDS Test Doubles - line 22
2. Basic Setup Pattern - line 30
3. Testing CDS Views with Aggregations - line 100
4. Testing CDS Views that Select from Another CDS View - line 130
5. Key Classes and Methods - line 170
6. Important Usage Notes - line 190

---

## 1. When to Use CDS Test Doubles

```
❌ WRONG: Use regular AUnit test class without test doubles
✅ CORRECT: Use CL_CDS_TEST_ENVIRONMENT to create test doubles for CDS views
```

Use the CDS Test Double Framework (`CL_CDS_TEST_ENVIRONMENT`) whenever:
- A class under test calls `SELECT FROM <cds_view>` directly or via a helper
- You need controlled test data (not production data)
- You need to test CDS view logic with specific scenarios

**Why**: CDS views read from database tables. Using test doubles allows injecting test data
without affecting production data, and keeps tests fast and isolated.

---

## 2. Basic Setup Pattern

The test class lives in `<classname>.clas.testclasses.abap`. The CDS view itself has no
`.testclasses` file — test it through a regular ABAP class that reads it.

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
    " Create CDS test environment — framework auto-creates doubles for dependencies
    mo_cds_env_static = cl_cds_test_environment=>create(
      i_for_entity = 'ZC_MY_CDS_VIEW' ).
  ENDMETHOD.

  METHOD class_teardown.
    " Clean up test environment
    mo_cds_env_static->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " IMPORTANT: Assign static env to instance and clear doubles before each test
    mo_cds_env = mo_cds_env_static.
    mo_cds_env->clear_doubles( ).
  ENDMETHOD.

  METHOD test_cds_with_doubles.
    " IMPORTANT: Must declare table type first — cannot inline in VALUE #()!
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

---

## 3. Testing CDS Views with Aggregations (SUM, COUNT, GROUP BY)

For CDS views with aggregations, insert test data into the **base tables** (SFLIGHT, SCARR,
SBOOK, etc.), not directly into the CDS view. The framework routes the inserts through the
aggregation pipeline.

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

  " Select from CDS view — aggregations will use test double data
  SELECT * FROM zc_flight_revenue INTO TABLE @DATA(lt_result).

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

---

## 4. Testing CDS Views that Select from Another CDS View

> **Note:** This pattern applies when your design **already has** a CDS view that selects from
> another CDS view. It does NOT mean you should split a single view into two — use a single
> CDS view with GROUP BY / JOIN when the business logic fits.

When your CDS view selects from **another CDS view** (not a base table), `create` raises
`CX_CDS_FAILURE`. Use `create_for_multiple_cds` instead and list all CDS entities in the
dependency chain.

```abap
METHOD class_setup.
  " ZC_TopView selects from ZC_IntermediateView (another CDS view entity)
  " → must use create_for_multiple_cds and list all CDS entities
  mo_cds_env_static = cl_cds_test_environment=>create_for_multiple_cds(
    i_for_entities = VALUE #(
      ( 'ZC_TOPVIEW' )           " the view under test
      ( 'ZC_INTERMEDIATEVIEW' )  " the CDS view it selects from
    ) ).
ENDMETHOD.
```

Insert test data into the **intermediate CDS view** (not the base tables), because that is
what the top-level view reads:

```abap
METHOD test_read.
  DATA lt_source TYPE TABLE OF zc_intermediateview WITH EMPTY KEY.
  lt_source = VALUE #(
    ( field1 = 'A' field2 = 100 )
    ( field1 = 'B' field2 = 200 ) ).
  mo_cds_env->insert_test_data( i_data = lt_source ).

  SELECT * FROM zc_topview INTO TABLE @DATA(lt_result).

  cl_abap_unit_assert=>assert_equals(
    exp = 2  act = lines( lt_result )  msg = 'Expected 2 rows' ).
ENDMETHOD.
```

**Rules:**
- List the view under test **and all CDS views it depends on** in `i_for_entities`
- Insert data into the **direct source** of the top-level view (the intermediate CDS view)
- Order in `i_for_entities` does not matter
- If `create` raises `CX_CDS_FAILURE`, switch to `create_for_multiple_cds`

---

## 5. Key Classes and Methods

| Item | Type / Usage |
|------|-------------|
| `CL_CDS_TEST_ENVIRONMENT` | Class — entry point, use its `CREATE` / `CREATE_FOR_MULTIPLE_CDS` methods |
| `IF_CDS_TEST_ENVIRONMENT` | Interface — declare your variable with this type |
| `CLASS-METHODS class_setup` | Must be static (`CLASS-METHODS`, not `METHODS`) |
| `CL_ABAP_UNIT_ASSERT` | Standard AUnit assertion class |

| Method | Purpose |
|--------|---------|
| `CL_CDS_TEST_ENVIRONMENT=>create( i_for_entity = ... )` | Environment for a CDS view over base tables |
| `CL_CDS_TEST_ENVIRONMENT=>create_for_multiple_cds( i_for_entities = ... )` | Environment when CDS view selects from another CDS view |
| `insert_test_data( i_data = ... )` | Inject test rows into a test double |
| `clear_doubles( )` | Remove all test rows — call in `setup` before each test |
| `destroy( )` | Tear down the environment — call in `class_teardown` |

---

## 6. Important Usage Notes

1. **Declare with interface type**: `DATA mo_cds_env TYPE REF TO if_cds_test_environment` —
   `create` returns an interface reference, not a class reference.

2. **Static lifecycle methods**: `class_setup` and `class_teardown` must be `CLASS-METHODS`
   (not `METHODS`). The environment is expensive to create — build it once per class run.

3. **Table type before VALUE #()**: Must declare `DATA lt_tab TYPE TABLE OF <type> WITH EMPTY KEY`
   before using `VALUE #()` — inline declaration inside `VALUE #()` is not supported here.

4. **Auto-created dependencies**: When the CDS view selects only from base tables, the framework
   auto-creates test doubles — do not specify an `i_dependency_list`. When the CDS view selects
   from another CDS view, use `create_for_multiple_cds` (see section 4).

5. **Aggregations**: For CDS views with SUM/COUNT/GROUP BY, insert test data into the base tables
   (SFLIGHT, SCARR, etc.), not the CDS view itself.

6. **Clear doubles**: Always call `clear_doubles` in `setup` to ensure test isolation.

7. **Enable associations**: Set `test_associations = 'X'` in `create` only when explicitly
   testing CDS association navigation.

8. **Exception handling**: Declare test methods with `RAISING cx_static_check` if the code
   under test raises checked exceptions.
