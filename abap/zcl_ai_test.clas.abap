" Test class with intentional errors for iteration testing
CLASS zcl_ai_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: do_something
      RETURNING VALUE(rv_result) TYPE string.

  PRIVATE SECTION.
    DATA: mv_counter TYPE i.
ENDCLASS.

CLASS zcl_ai_test IMPLEMENTATION.
  METHOD do_something.
    " Intentional syntax error: missing period after IF
    IF mv_counter > 0
      rv_result = 'Positive'.
    ENDIF

    " Intentional error: undefined variable
    DATA lv_undefined TYPE string.
    lv_undefined = mv_not_defined.
  ENDMETHOD.
ENDCLASS.
