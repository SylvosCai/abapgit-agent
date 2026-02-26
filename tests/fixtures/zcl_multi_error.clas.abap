CLASS zcl_multi_error DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS method1.
    METHODS method2.
    METHODS method3.
ENDCLASS.

CLASS zcl_multi_error IMPLEMENTATION.
  METHOD method1.
    " Valid code
    DATA lv_ok TYPE string.
    lv_ok = 'test'.
  ENDMETHOD.

  METHOD method2.
    " Error: Unknown statement
    UNKNOWN_STATEMENT error_here.
  ENDMETHOD.

  METHOD method3.
    " This won't be checked - SYNTAX-CHECK stops at first error
    DATA lv_test TYPE string.
  ENDMETHOD.
ENDCLASS.
