*"* use this source file for your test class implementation
*"* local test class

"--------------------------------------------------------------------
" Test double: agent stub — returns configurable drop result
"--------------------------------------------------------------------
CLASS ltcl_agent_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_agent.
    DATA ms_drop_result TYPE zif_abgagt_agent=>ty_drop_result.
ENDCLASS.

CLASS ltcl_agent_double IMPLEMENTATION.
  METHOD zif_abgagt_agent~drop.
    rs_result = ms_drop_result.
  ENDMETHOD.
  METHOD zif_abgagt_agent~pull.            " not used
  ENDMETHOD.
  METHOD zif_abgagt_agent~get_repo_status. " not used
  ENDMETHOD.
ENDCLASS.

"--------------------------------------------------------------------
" Test double: util stub — returns configurable parse result
"--------------------------------------------------------------------
CLASS ltcl_util_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_util.
    DATA mv_obj_type TYPE string.
    DATA mv_obj_name TYPE string.
ENDCLASS.

CLASS ltcl_util_double IMPLEMENTATION.
  METHOD zif_abgagt_util~parse_file_to_object.
    ev_obj_type = mv_obj_type.
    ev_obj_name = mv_obj_name.
  ENDMETHOD.
  METHOD zif_abgagt_util~detect_include_info.     ENDMETHOD.
  METHOD zif_abgagt_util~get_object_info_from_tadir. ENDMETHOD.
  METHOD zif_abgagt_util~check_log_for_errors.    ENDMETHOD.
  METHOD zif_abgagt_util~get_log_detail.          ENDMETHOD.
  METHOD zif_abgagt_util~convert_method_index.    ENDMETHOD.
  METHOD zif_abgagt_util~convert_index_to_cm_suffix. ENDMETHOD.
  METHOD zif_abgagt_util~get_method_name.         ENDMETHOD.
  METHOD zif_abgagt_util~get_include_description. ENDMETHOD.
ENDCLASS.

"--------------------------------------------------------------------
" Main test class
"--------------------------------------------------------------------
CLASS ltcl_command_drop DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_agent_dbl TYPE REF TO ltcl_agent_double.
    DATA mo_cut       TYPE REF TO zcl_abgagt_command_drop.

    METHODS setup.
    METHODS test_get_name             FOR TESTING.
    METHODS test_missing_file         FOR TESTING.
    METHODS test_drop_success         FOR TESTING.
    METHODS test_drop_error           FOR TESTING.
    METHODS test_dtel_rejected        FOR TESTING.
    METHODS test_unrecognized_file    FOR TESTING.
    METHODS test_object_not_in_tadir  FOR TESTING.
ENDCLASS.

CLASS ltcl_command_drop IMPLEMENTATION.

  METHOD setup.
    mo_agent_dbl = NEW #( ).
    mo_cut = NEW zcl_abgagt_command_drop( io_agent = mo_agent_dbl ).
  ENDMETHOD.

  METHOD test_get_name.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->zif_abgagt_command~get_name( )
      exp = 'DROP'
      msg = 'Command name should be DROP' ).
  ENDMETHOD.

  METHOD test_missing_file.
    " Agent returns 'file is required' error for empty file
    mo_agent_dbl->ms_drop_result-error = 'file is required'.

    DATA: BEGIN OF ls_param,
            file TYPE string VALUE '',
          END OF ls_param.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*"success":""*'
      msg = 'Should return error when file is empty' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*file is required*'
      msg = 'Should contain file-is-required message' ).
  ENDMETHOD.

  METHOD test_drop_success.
    mo_agent_dbl->ms_drop_result = VALUE #(
      success  = abap_true
      obj_type = 'CLAS'
      obj_name = 'ZCL_FOO'
      message  = 'Object deleted successfully' ).

    DATA: BEGIN OF ls_param,
            file TYPE string VALUE 'abap/zcl_foo.clas.abap',
          END OF ls_param.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*"success":"X"*'
      msg = 'Should return success X' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*ZCL_FOO*'
      msg = 'Should contain object name' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*Object deleted successfully*'
      msg = 'Should contain success message' ).
  ENDMETHOD.

  METHOD test_drop_error.
    mo_agent_dbl->ms_drop_result-error = 'Object ZCL_FOO (CLAS) not found in TADIR'.

    DATA: BEGIN OF ls_param,
            file TYPE string VALUE 'abap/zcl_foo.clas.abap',
          END OF ls_param.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*"success":""*'
      msg = 'Should return empty success on error' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*not found in TADIR*'
      msg = 'Should propagate error message' ).
  ENDMETHOD.

  METHOD test_dtel_rejected.
    mo_agent_dbl->ms_drop_result-error =
      'drop does not support DTEL objects. Edit the XML and use pull instead.'.

    DATA: BEGIN OF ls_param,
            file TYPE string VALUE 'abap/zmy_elem.dtel.xml',
          END OF ls_param.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*"success":""*'
      msg = 'DTEL should return error' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*does not support DTEL*'
      msg = 'Should mention DTEL unsupported' ).
  ENDMETHOD.

  METHOD test_unrecognized_file.
    mo_agent_dbl->ms_drop_result-error =
      'Cannot derive object type/name from file: abap/some_config.json'.

    DATA: BEGIN OF ls_param,
            file TYPE string VALUE 'abap/some_config.json',
          END OF ls_param.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*"success":""*'
      msg = 'Should return error for unrecognized file' ).
  ENDMETHOD.

  METHOD test_object_not_in_tadir.
    mo_agent_dbl->ms_drop_result-error =
      'Object ZCL_ABGAGT_NONEXISTENT (CLAS) not found in TADIR'.

    DATA: BEGIN OF ls_param,
            file TYPE string VALUE 'abap/zcl_abgagt_nonexistent.clas.abap',
          END OF ls_param.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*"success":""*'
      msg = 'Should return error when object not in TADIR' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result  exp = '*not found in TADIR*'
      msg = 'Should mention TADIR in error' ).
  ENDMETHOD.

ENDCLASS.
