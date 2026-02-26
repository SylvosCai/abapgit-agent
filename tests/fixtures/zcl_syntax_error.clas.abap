CLASS zcl_syntax_error DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS test_method.
ENDCLASS.

CLASS zcl_syntax_error IMPLEMENTATION.
  METHOD test_method.
    " Intentional syntax error - unknown statement
    UNKNOWN_STATEMENT this_will_fail.

    " Missing period
    DATA lv_test TYPE string

    " Another error - undefined variable
    lv_undefined = 'test'.
  ENDMETHOD.
ENDCLASS.
