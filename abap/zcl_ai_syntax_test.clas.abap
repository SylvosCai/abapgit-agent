" Simple test class
CLASS zcl_ai_syntax_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
ENDCLASS.

CLASS zcl_ai_syntax_test IMPLEMENTATION.
ENDCLASS.

" Local test class
CLASS ltcl_test DEFINITION FOR TESTING.
  PRIVATE SECTION.
    METHODS: test_syntax_ok FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_syntax_ok.
    " Simple test that always passes
    DATA: lv_result TYPE string.
    lv_result = 'Test passed'.
    cl_abap_unit_assert=>assert_not_initial( lv_result ).
  ENDMETHOD.
ENDCLASS.
