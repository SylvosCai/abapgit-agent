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
    " Fixed: Added period after IF statement
    IF mv_counter > 0.
      rv_result = 'Positive'.
    ENDIF.

    " Fixed: Use defined variable
    DATA lv_result TYPE string.
    lv_result = |Counter: { mv_counter }|.
  ENDMETHOD.
ENDCLASS.
