# ABAP Unit Testable Code Guidelines

This document provides guidelines for creating ABAP OO classes/interfaces that can be easily unit tested with test doubles. These guidelines help AI coding tools understand how to design classes that are testable without requiring real dependencies.

## The Problem

When ABAP classes are not designed for testability, unit tests cannot mock dependencies. This leads to:

- Tests that depend on real external systems (databases, APIs, file systems)
- Tests that fail in different environments
- Tests that are slow and unreliable
- Impossible to test error conditions

**Example of untestable code:**

```abap
" BAD - Hardcoded dependency, cannot be replaced in tests
CLASS zcl_abgagt_command_pull DEFINITION PUBLIC.
  METHOD execute.
    lo_agent = NEW zcl_abgagt_agent( ).  " Hardcoded!
    ls_result = lo_agent->pull( ... ).   " Calls real system
  ENDMETHOD.
ENDCLASS.
```

The unit test will instantiate the REAL `zcl_abgagt_agent` which tries to connect to abapGit and a real git repository, causing test failures.

---

## Core Principles

### 1. Dependency Inversion (Dependency Injection)

**Pass dependencies through constructor instead of creating them internally.**

```abap
" GOOD - Dependency injected via constructor
CLASS zcl_abgagt_command_pull DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    " Constructor injection
    METHODS constructor
      IMPORTING
        io_agent TYPE REF TO zif_abgagt_agent.

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zif_abgagt_agent.

ENDCLASS.

CLASS zcl_abgagt_command_pull IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_agent = io_agent.
  ENDMETHOD.

  METHOD execute.
    " Use injected dependency
    ls_result = mo_agent->pull( ... ).
  ENDMETHOD.

ENDCLASS.
```

**In production code:**
```abap
DATA(lo_command) = NEW zcl_abgagt_command_pull(
  io_agent = NEW zcl_abgagt_agent( ) ).
```

**In test code:**
```abap
" Create test double
CLASS ltd_mock_agent DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_agent PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_mock_agent IMPLEMENTATION.
  METHOD zif_abgagt_agent~pull.
    " Return test data instead of calling real system
    rs_result-success = abap_true.
    rs_result-message = 'Test success'.
  ENDMETHOD.
ENDCLASS.

" Test uses test double
CLASS ltcl_test DEFINITION FOR TESTING.
  METHOD test_execute.
    DATA(lo_mock) = NEW ltd_mock_agent( ).
    DATA(lo_cut) = NEW zcl_abgagt_command_pull( io_agent = lo_mock ).

    DATA(lv_result) = lo_cut->execute( ... ).

    " Assert expected results
  ENDMETHOD.
ENDCLASS.
```

### 2. Always Use Interfaces for Dependencies

**Never depend on concrete classes - depend on interfaces.**

```abap
" GOOD - Depend on interface
DATA mo_agent TYPE REF TO zif_abgagt_agent.  " Interface!

" BAD - Depends on concrete class
DATA mo_agent TYPE REF TO zcl_abgagt_agent.   " Concrete class!
```

This allows you to replace the implementation with test doubles.

### 3. Make Dependencies Injectable via Constructor

**Use constructor injection, not setter injection.**

```abap
" GOOD - Constructor injection (required dependency)
METHODS constructor
  IMPORTING
    io_agent TYPE REF TO zif_abgagt_agent.

" BAD - Setter injection (optional, can be forgotten)
METHODS set_agent
  IMPORTING
    io_agent TYPE REF TO zif_abgagt_agent.
```

Constructor injection:
- Makes dependency explicit
- Ensures object is always in valid state
- Cannot forget to inject

### 4. Avoid Static Calls

**Static method calls cannot be mocked/test-doubled.**

```abap
" BAD - Static call cannot be replaced
DATA(li_repo) = zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url( ... ).

" GOOD - Instance method via injected dependency
DATA(li_repo) = mo_repo_srv->get_repo_from_url( ... ).
```

If you must call static methods, wrap them in an instance method of an injected class.

### 5. Keep Constructor Simple

**Constructor should only assign dependencies, not perform complex logic.**

```abap
" GOOD - Simple constructor
METHOD constructor.
  mo_agent = io_agent.
  mo_logger = io_logger.
ENDMETHOD.

" BAD - Complex logic in constructor
METHOD constructor.
  mo_agent = io_agent.
  " Don't do this here:
  mo_agent->connect( ).  " Network call in constructor!
  DATA(ls_config) = read_config( ).  " File I/O in constructor!
ENDMETHOD.
```

---

## Injection Techniques

### Constructor Injection (Recommended)

```abap
CLASS zcl_my_class DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_dependency TYPE REF TO zif_my_interface.
  PRIVATE SECTION.
    DATA mo_dependency TYPE REF TO zif_my_interface.
ENDCLASS.
```

### Back Door Injection (for existing code)

When you cannot modify the constructor, use friendship:

```abap
" In test class
CLASS zcl_my_class DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_with_mock.
    " Directly set private attribute via friendship
    CREATE OBJECT mo_cut.
    mo_cut->mo_dependency = lo_mock.  " Access private attribute
  ENDMETHOD.
ENDCLASS.
```

### Test Seams (last resort)

For legacy code that cannot be refactored:

```abap
" In production code
METHOD get_data.
  TEST-SEAM db_select.
    SELECT * FROM dbtab INTO TABLE @DATA(lt_data).
  END-TEST-SEAM.
ENDMETHOD.

" In test class
METHOD test_get_data.
  TEST-INJECTION db_select.
    lt_data = VALUE #( ( id = '1' ) ( id = '2' ) ).
  END-TEST-INJECTION.

  DATA(lt_result) = mo_cut->get_data( ).
ENDMETHOD.
```

---

## Test Double Patterns

### Manual Test Double (Local Class)

```abap
" Create test double class
CLASS ltd_mock_reader DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_data_reader PARTIALLY IMPLEMENTED.
    METHODS set_result_data
      IMPORTING it_data TYPE ANY TABLE.
  PRIVATE SECTION.
    DATA mt_data TYPE ANY TABLE.
ENDCLASS.

CLASS ltd_mock_reader IMPLEMENTATION.
  METHOD set_result_data.
    mt_data = it_data.
  ENDMETHOD.

  METHOD zif_data_reader~read_all.
    rt_data = mt_data.
  ENDMETHOD.
ENDCLASS.
```

### Using ABAP Test Double Framework

```abap
" Create test double for class
DATA(lo_double) = cl_abap_testdouble=>create( 'zcl_real_class' ).

" Configure method to return test data
cl_abap_testdouble=>configure_call( lo_double )->get_data(
  EXPORTING iv_id = '123'
  IMPORTING ev_result = DATA(lv_result) ).

" Raise exception
cl_abap_testdouble=>configure_call( lo_double )->raise( 'cx_static_check' ).
```

---

## Guidelines for AI Coding Tools

When creating new ABAP classes, ALWAYS follow these rules:

### DO:

1. **Use interfaces for ALL external dependencies**
   - Database access → interface
   - External API calls → interface
   - File I/O → interface
   - Other services → interface

2. **Pass dependencies via constructor**
   ```abap
   METHODS constructor
     IMPORTING
       io_agent TYPE REF TO zif_abgagt_agent
       io_logger TYPE REF TO zif_logger.
   ```

3. **Define interfaces for all service classes**
   ```abap
   " Instead of using class directly
   DATA mo_agent TYPE REF TO zcl_abgagt_agent.   " BAD

   " Use interface
   DATA mo_agent TYPE REF TO zif_abgagt_agent.    " GOOD
   ```

4. **Keep classes FINAL if they don't need mocking**
   - If a class has no dependencies and doesn't need test doubles, make it FINAL
   - If a class needs to be mocked in tests, don't make it FINAL

5. **Use dependency injection in command classes**
   ```abap
   " Good pattern for command classes
   CLASS zcl_abgagt_command_pull DEFINITION PUBLIC.
     PUBLIC SECTION.
       INTERFACES zif_abgagt_command.
       METHODS constructor
         IMPORTING io_agent TYPE REF TO zif_abgagt_agent.
   ENDCLASS.
   ```

### DON'T:

1. **Never create dependencies inside methods**
   ```abap
   " BAD
   METHOD execute.
     lo_agent = NEW zcl_abgagt_agent( ).  " Hardcoded!
   ENDMETHOD.

   " GOOD
   METHOD execute.
     ls_result = mo_agent->pull( ... ).   " Use injected
   ENDMETHOD.
   ```

2. **Don't use static method calls for testable code**
   ```abap
   " BAD
   DATA(lo_srv) = zcl_some_srv=>get_instance( ).

   " GOOD - inject the service
   DATA(lo_srv) = mo_service_provider.
   ```

3. **Don't make classes FINAL if they need test doubles**
   - If you need to mock a class in tests, don't declare it FINAL

---

## Example: Refactoring for Testability

### Before (Not Testable)

```abap
CLASS zcl_abgagt_command_pull DEFINITION PUBLIC.
  METHOD execute.
    DATA lo_agent TYPE REF TO zcl_abgagt_agent.
    lo_agent = NEW zcl_abgagt_agent( ).  " Hardcoded!

    ls_result = lo_agent->pull(
      iv_url = ls_params-url
      iv_branch = ls_params-branch ).
  ENDMETHOD.
ENDCLASS.
```

### After (Testable)

```abap
" Interface for agent
INTERFACE zif_abgagt_agent PUBLIC.
  METHODS pull ... RAISING zcx_abapgit_exception.
ENDINTERFACE.

" Command class with constructor injection
CLASS zcl_abgagt_command_pull DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    METHODS constructor
      IMPORTING
        io_agent TYPE REF TO zif_abgagt_agent OPTIONAL.  " Optional for backward compat

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zif_abgagt_agent.

    METHODS get_agent
      RETURNING VALUE(ro_agent) TYPE REF TO zif_abgagt_agent.
ENDCLASS.

CLASS zcl_abgagt_command_pull IMPLEMENTATION.

  METHOD constructor.
    mo_agent = io_agent.
  ENDMETHOD.

  METHOD get_agent.
    " Lazy creation if not injected (for production)
    IF mo_agent IS NOT BOUND.
      mo_agent = NEW zcl_abgagt_agent( ).
    ENDIF.
    ro_agent = mo_agent.
  ENDMETHOD.

  METHOD execute.
    DATA(lo_agent) = get_agent( ).
    ls_result = lo_agent->pull( ... ).
  ENDMETHOD.

ENDCLASS.
```

**Production usage:**
```abap
DATA(lo_command) = NEW zcl_abgagt_command_pull(
  io_agent = NEW zcl_abgagt_agent( ) ).
```

**Test usage:**
```abap
CLASS ltd_mock_agent DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_agent PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_mock_agent IMPLEMENTATION.
  METHOD zif_abgagt_agent~pull.
    rs_result-success = abap_true.
    rs_result-message = 'Mocked success'.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test DEFINITION FOR TESTING.
  METHOD test_pull_success.
    DATA(lo_mock) = NEW ltd_mock_agent( ).
    DATA(lo_cut) = NEW zcl_abgagt_command_pull( io_agent = lo_mock ).

    DATA(lv_result) = lo_cut->execute( ... ).

    " Assert mocked behavior
  ENDMETHOD.
ENDCLASS.
```

---

## Key Takeaways

1. **Always use interfaces** for dependencies
2. **Use constructor injection** to pass dependencies
3. **Never hardcode `NEW` for dependencies** - pass them in
4. **Avoid static calls** - use instance methods with injected dependencies
5. **Keep constructors simple** - only assign dependencies

Following these guidelines ensures that:
- Unit tests can mock all dependencies
- Tests run fast without external systems
- Tests are reliable and repeatable
- Error conditions can be tested easily
- Code is modular and loosely coupled
