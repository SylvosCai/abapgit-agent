" Test class with intentional errors for iteration testing
CLASS zcl_ai_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: do_something.

  PRIVATE SECTION.
    DATA: mv_counter TYPE i.
ENDCLASS.

CLASS zcl_ai_test IMPLEMENTATION.
  METHOD do_something.
    " Intentional error: undefined variable
    WRITE: / mv_countr.

    " Intentional error: missing period
    IF mv_counter > 0
      WRITE: / 'Counter is positive'.
    ENDIF

    " Intentional error: syntax error
    DATA lv_text(10 TYPE c.
  ENDMETHOD.
ENDCLASS.
