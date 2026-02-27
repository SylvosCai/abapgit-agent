CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_test_auto_detect.

    METHODS setup.
    METHODS test_add FOR TESTING.
    METHODS test_multiply FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW zcl_test_auto_detect( ).
  ENDMETHOD.

  METHOD test_add.
    DATA(lv_result) = mo_cut->add(
      iv_a = 5
      iv_b = 3 ).
    cl_abap_unit_assert=>assert_equals(
      exp = 8
      act = lv_result ).
  ENDMETHOD.

  METHOD test_multiply.
    DATA(lv_result) = lcl_helper=>multiply(
      iv_a = 4
      iv_b = UNDEFINED_VARIABLE ).
    cl_abap_unit_assert=>assert_equals(
      exp = 12
      act = lv_result ).
  ENDMETHOD.
ENDCLASS.
