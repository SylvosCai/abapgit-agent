" Test class with syntax errors for iteration workflow
CLASS zcl_ai_syntax_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: constructor RETURNING VALUE(rv_result) TYPE string.

  PRIVATE SECTION.
    DATA: mv_value TYPE i.
ENDCLASS.

CLASS zcl_ai_syntax_test IMPLEMENTATION.
  METHOD constructor.
    rv_result = 'Hello'.
  ENDMETHOD.
ENDCLASS.
