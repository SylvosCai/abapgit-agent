CLASS zcl_warning_test DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS test_method.
ENDCLASS.

CLASS zcl_warning_test IMPLEMENTATION.
  METHOD test_method.
    " This should produce warnings in Code Inspector:
    " 1. Unused variable
    DATA lv_unused TYPE string.

    " 2. WRITE without data (potential info)
    WRITE / 'Hello'.

    " 3. Actually use something to make it valid
    DATA(lv_result) = 1 + 1.
  ENDMETHOD.
ENDCLASS.
