*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_util DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_util TYPE REF TO zif_abgagt_util.

    METHODS setup.
    METHODS test_parse_clas FOR TESTING.
    METHODS test_parse_intf FOR TESTING.
    METHODS test_parse_with_path FOR TESTING.
    METHODS test_parse_invalid FOR TESTING.
    METHODS test_detect_include_method FOR TESTING.
    METHODS test_detect_intf_section FOR TESTING.
    METHODS test_detect_public_section FOR TESTING.
    METHODS test_convert_method_index FOR TESTING.
    METHODS test_convert_index_to_cm_suffix FOR TESTING.
ENDCLASS.

CLASS ltcl_zcl_abgagt_util IMPLEMENTATION.

  METHOD setup.
    mo_util = zcl_abgagt_util=>get_instance( ).
  ENDMETHOD.

  METHOD test_parse_clas.
    " Test parsing CLAS file
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->parse_file_to_object(
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

    mo_util->parse_file_to_object(
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

    mo_util->parse_file_to_object(
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

    mo_util->parse_file_to_object(
      EXPORTING iv_file = 'invalid_file.txt'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_type msg = 'Invalid file should return empty type' ).
    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_name msg = 'Invalid file should return empty name' ).
  ENDMETHOD.

  METHOD test_detect_include_method.
    " Test detecting method include - obj_name should NOT have trailing equals
    DATA ls_info TYPE zif_abgagt_util=>ty_include_info.

    ls_info = mo_util->detect_include_info( 'ZCL_ABGAGT_COMMAND_PULL=======CM001' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_info-is_source_include exp = abap_true
      msg = 'Should be detected as source include' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_type exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_name exp = 'ZCL_ABGAGT_COMMAND_PULL'
      msg = 'Object name should NOT have trailing equals' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-include_type exp = 'CM001'
      msg = 'Include type should be CM001' ).
  ENDMETHOD.

  METHOD test_detect_intf_section.
    " Test detecting interface section
    DATA ls_info TYPE zif_abgagt_util=>ty_include_info.

    ls_info = mo_util->detect_include_info( 'ZIF_ABGAGT_CMD_FACTORY========IU' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_info-is_source_include exp = abap_true
      msg = 'Should be detected as source include' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_type exp = 'INTF'
      msg = 'Object type should be INTF' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_name exp = 'ZIF_ABGAGT_CMD_FACTORY'
      msg = 'Object name should NOT have trailing equals' ).
  ENDMETHOD.

  METHOD test_detect_public_section.
    " Test detecting public section
    DATA ls_info TYPE zif_abgagt_util=>ty_include_info.

    ls_info = mo_util->detect_include_info( 'ZCL_ABGAGT_COMMAND_VIEW=======CU' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_info-is_source_include exp = abap_true
      msg = 'Should be detected as source include' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_type exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_name exp = 'ZCL_ABGAGT_COMMAND_VIEW'
      msg = 'Object name should NOT have trailing equals' ).
  ENDMETHOD.

  METHOD test_convert_method_index.
    \" Test base-36 decoding: CM suffix -> integer index
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_method_index( 'CM001' ) exp = 1
      msg = 'CM001 should be 1' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_method_index( 'CM00A' ) exp = 10
      msg = 'CM00A should be 10' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_method_index( 'CM00Z' ) exp = 35
      msg = 'CM00Z should be 35' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_method_index( 'CM010' ) exp = 36
      msg = 'CM010 should be 36' ).
  ENDMETHOD.

  METHOD test_convert_index_to_cm_suffix.
    \" Test base-36 encoding: integer index -> CM suffix
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_index_to_cm_suffix( 1 ) exp = 'CM001'
      msg = '1 should be CM001' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_index_to_cm_suffix( 10 ) exp = 'CM00A'
      msg = '10 should be CM00A' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_index_to_cm_suffix( 35 ) exp = 'CM00Z'
      msg = '35 should be CM00Z' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_index_to_cm_suffix( 36 ) exp = 'CM010'
      msg = '36 should be CM010' ).
  ENDMETHOD.

ENDCLASS.
