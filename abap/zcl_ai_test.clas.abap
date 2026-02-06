" Test class with intentional errors for iteration testing
CLASS zcl_ai_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: do_something.

  PRIVATE SECTION.
    DATA: mv_counter TYPE i,
          " Intentional error: unknown type
          mv_unknown TYPE ztype_unknown.
ENDCLASS.

CLASS zcl_ai_test IMPLEMENTATION.
  METHOD do_something.
    WRITE: / 'Test method executed'.
  ENDMETHOD.
ENDCLASS.
