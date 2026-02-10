*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_util DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_util TYPE REF TO zcl_abgagt_util.

    METHODS setup.
    METHODS test_parse_clas FOR TESTING.
    METHODS test_parse_intf FOR TESTING.
    METHODS test_parse_with_path FOR TESTING.
    METHODS test_parse_invalid FOR TESTING.
    METHODS test_intentional_fail FOR TESTING.
ENDCLASS.

CLASS ltcl_zcl_abgagt_util IMPLEMENTATION.

  METHOD setup.
    mo_util = zcl_abgagt_util=>get_instance( ).
  ENDMETHOD.

  METHOD test_parse_clas.
    " Test parsing CLAS file
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = 'zcl_my_class.clas.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type exp = 'CLAS' msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name exp = 'ZCL_MY_CLASS' msg = 'Object name mismatch' ).
  ENDMETHOD.

  METHOD test_parse_intf.
    " Test parsing INTF file
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = 'zif_my_intf.intf.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type exp = 'INTF' msg = 'Object type should be INTF' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name exp = 'ZIF_MY_INTF' msg = 'Object name mismatch' ).
  ENDMETHOD.

  METHOD test_parse_with_path.
    " Test parsing file with path
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = 'src/zcl_my_class.clas.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type exp = 'CLAS' msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name exp = 'ZCL_MY_CLASS' msg = 'Path should be stripped' ).
  ENDMETHOD.

  METHOD test_parse_invalid.
    " Test parsing invalid file (should return empty)
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = 'invalid_file.txt'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_type msg = 'Invalid file should return empty type' ).
    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_name msg = 'Invalid file should return empty name' ).
  ENDMETHOD.

  METHOD test_intentional_fail.
    " This test demonstrates failure - parse CLAS but expect INTF
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = 'zcl_my_class.clas.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    " This will fail because CLAS != INTF
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type
      exp = 'INTF'
      msg = 'CLAS file should be parsed as INTF (intentional fail)' ).
  ENDMETHOD.

ENDCLASS.
