" Simple test class
CLASS zcl_ai_syntax_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: test_method.
ENDCLASS.

CLASS zcl_ai_syntax_test IMPLEMENTATION.
  METHOD test_method.
    " Intentional syntax error: missing period after DATA
    DATA lv_test TYPE string
    WRITE lv_test.
  ENDMETHOD.
ENDCLASS.
