" Test class with syntax errors for iteration workflow
CLASS zcl_ai_syntax_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: constructor.

  PRIVATE SECTION.
    DATA: mv_value TYPE i.
ENDCLASS.

CLASS zcl_ai_syntax_test IMPLEMENTATION.
  METHOD constructor.
    " Intentional error: calling a variable as if it were a class
    DATA(lo_obj) = mv_undefined_var( ).
  ENDMETHOD.
ENDCLASS.
