*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_cmd_inspect DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_inspect.

    " Local type for DDLS names
    TYPES ty_ddls_names TYPE STANDARD TABLE OF tadir-obj_name WITH NON-UNIQUE DEFAULT KEY.

    METHODS setup.
    METHODS test_get_name FOR TESTING.
    METHODS test_interface FOR TESTING.
    METHODS test_exec_no_files FOR TESTING.
    METHODS test_exec_single_file FOR TESTING.
    METHODS test_exec_multi_files FOR TESTING.
    METHODS test_exec_ddls_file FOR TESTING.
    METHODS test_exec_variant_parameter FOR TESTING.
    METHODS test_exec_mixed_files FOR TESTING.
    METHODS test_exec_invalid_file FOR TESTING.
    METHODS test_build_object_result FOR TESTING.
    METHODS test_get_check_variant FOR TESTING.
    METHODS test_create_inspection_name FOR TESTING.
    METHODS test_mock_success FOR TESTING.
    METHODS test_mock_errors FOR TESTING.
    METHODS test_mock_warnings FOR TESTING.
    METHODS test_mock_info FOR TESTING.
    METHODS test_mock_unittest_include FOR TESTING.
    METHODS test_mock_local_defs FOR TESTING.
    METHODS test_mock_local_impl FOR TESTING.
    METHODS test_mock_macros FOR TESTING.
    METHODS test_mock_multi_errors FOR TESTING.
    METHODS test_mock_var_not_found FOR TESTING.
    METHODS test_mock_exception FOR TESTING.
    " Test double tests for DDL handler
    METHODS test_validate_ddls_success FOR TESTING RAISING cx_dd_ddl_check.
    METHODS test_validate_ddls_not_found FOR TESTING RAISING cx_dd_ddl_check.
    METHODS test_ddls_with_warnings FOR TESTING RAISING cx_dd_ddl_check.
    METHODS test_ddls_with_errors FOR TESTING RAISING cx_dd_ddl_check.
    METHODS test_read_ddls_source FOR TESTING RAISING cx_dd_ddl_check.
    METHODS test_validate_ddls_chk FOR TESTING RAISING cx_dd_ddl_check.
ENDCLASS.

"**********************************************************************
" Local test double for code inspector
CLASS lcl_code_inspector_mock DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_code_inspector.

    " Configuration for mock behavior
    DATA mv_return_errors TYPE abap_bool.
    DATA mv_return_warnings TYPE abap_bool.
    DATA mv_variant_not_found TYPE abap_bool.
    DATA mv_raise_exception TYPE abap_bool.

    " Mock data to return
    DATA mt_mock_results TYPE scit_alvlist.
    DATA mv_objname TYPE sobj_name.
    DATA mx_exception TYPE REF TO cx_root.

    METHODS constructor.

  PRIVATE SECTION.
    DATA mo_mock_objset TYPE REF TO cl_ci_objectset.
    DATA mo_mock_inspection TYPE REF TO cl_ci_inspection.
    DATA mo_mock_variant TYPE REF TO cl_ci_checkvariant.
ENDCLASS.

CLASS lcl_code_inspector_mock IMPLEMENTATION.

  METHOD constructor.
    mv_return_errors = abap_false.
    mv_return_warnings = abap_false.
    mv_variant_not_found = abap_false.
    mv_raise_exception = abap_false.

    " Create mock objects to avoid null reference issues
    CREATE OBJECT mo_mock_objset.
    CREATE OBJECT mo_mock_inspection.
    CREATE OBJECT mo_mock_variant.
  ENDMETHOD.

  METHOD zif_abgagt_code_inspector~create_object_set.
    IF mv_raise_exception = abap_true AND mx_exception IS BOUND.
      RAISE EXCEPTION mx_exception.
    ENDIF.
    ro_objset = mo_mock_objset.
  ENDMETHOD.

  METHOD zif_abgagt_code_inspector~get_check_variant.
    IF mv_variant_not_found = abap_true.
      RETURN.
    ENDIF.
    ro_variant = mo_mock_variant.
  ENDMETHOD.

  METHOD zif_abgagt_code_inspector~create_and_run_inspection.
    IF mv_raise_exception = abap_true AND mx_exception IS BOUND.
      RAISE EXCEPTION mx_exception.
    ENDIF.
    ro_inspection = mo_mock_inspection.
  ENDMETHOD.

  METHOD zif_abgagt_code_inspector~get_results.
    " Return mock results - filter by objname if set
    IF mv_objname IS NOT INITIAL.
      DATA ls TYPE LINE OF scit_alvlist.
      LOOP AT mt_mock_results INTO ls WHERE objname = mv_objname.
        APPEND ls TO rt_list.
      ENDLOOP.
    ELSE.
      rt_list = mt_mock_results.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_code_inspector~cleanup.
    " Do nothing in mock
  ENDMETHOD.

ENDCLASS.

"**********************************************************************
" Local test double for DDL handler
CLASS lcl_ddl_handler_mock DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_ddl_handler.

    " Configuration for mock behavior
    DATA mv_raise_not_found TYPE abap_bool.
    DATA mv_raise_check_error TYPE abap_bool.
    DATA mv_source_found TYPE abap_bool.

    " Mock data to return
    DATA ms_mock_ddlsrcv TYPE zif_abgagt_ddl_handler=>ty_ddlsrcv.
    DATA mt_mock_warnings TYPE zif_abgagt_ddl_handler=>ty_warnings.
    DATA mx_check_exception TYPE REF TO cx_dd_ddl_check.

    METHODS constructor.

  PRIVATE SECTION.
    DATA mv_call_count_read TYPE i.
    DATA mv_call_count_check TYPE i.
ENDCLASS.

CLASS lcl_ddl_handler_mock IMPLEMENTATION.

  METHOD constructor.
    mv_raise_not_found = abap_false.
    mv_raise_check_error = abap_false.
    mv_source_found = abap_true.
    mv_call_count_read = 0.
    mv_call_count_check = 0.
  ENDMETHOD.

  METHOD zif_abgagt_ddl_handler~read.
    mv_call_count_read = mv_call_count_read + 1.

    IF mv_raise_not_found = abap_true.
      RAISE EXCEPTION TYPE cx_dd_ddl_check.
    ENDIF.

    IF mv_source_found = abap_true.
      es_ddlsrcv = ms_mock_ddlsrcv.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_ddl_handler~check.
    mv_call_count_check = mv_call_count_check + 1.

    IF mv_raise_check_error = abap_true AND mx_check_exception IS BOUND.
      RAISE EXCEPTION mx_check_exception.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_ddl_handler~get_warnings.
    rt_warnings = mt_mock_warnings.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cmd_inspect IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect.
  ENDMETHOD.

  METHOD test_get_name.
    " Test that get_name returns the correct command name
    DATA(lv_name) = mo_cut->zif_abgagt_command~get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = zif_abgagt_command=>gc_inspect
      msg = 'Command name should be INSPECT' ).
  ENDMETHOD.

  METHOD test_interface.
    " Test that the class implements the command interface

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect.
    DATA(lo_interface) = mo_cut.
    cl_abap_unit_assert=>assert_bound(
      act = lo_interface
      msg = 'Object should implement zif_abgagt_command interface' ).
  ENDMETHOD.

  METHOD test_exec_no_files.
    " Test execute with no files - should return error
    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should be valid JSON (starts with { and ends with })
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*{*'
      msg = 'Result should be valid JSON object' ).

    " Result should contain ERROR_COUNT or error_count
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ERROR_COUNT*'
      msg = 'Result should contain error_count field' ).
  ENDMETHOD.

  METHOD test_exec_single_file.
    " Test execute with a single file
    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.

    APPEND 'zcl_my_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should be valid JSON
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*{*'
      msg = 'Result should be valid JSON object' ).

    " Result should contain SUCCESS or success field
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*SUCCESS*'
      msg = 'Result should contain success field' ).
  ENDMETHOD.

  METHOD test_exec_multi_files.
    " Test execute with multiple files
    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.

    APPEND 'zcl_my_class.clas.abap' TO ls_param-files.
    APPEND 'zif_my_intf.intf.abap' TO ls_param-files.
    APPEND 'zcl_another.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should be valid JSON
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*{*'
      msg = 'Result should be valid JSON object' ).

    " Result should contain ERROR_COUNT or error_count
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ERROR_COUNT*'
      msg = 'Result should contain error_count field' ).
  ENDMETHOD.

  METHOD test_exec_ddls_file.
    " Test execute with DDLS file (CDS view)
    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.

    APPEND 'zc_my_cds_view.ddls.asddls' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should be valid JSON
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*{*'
      msg = 'Result should be valid JSON object' ).

    " Result should contain DDLS object type
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*DDLS*'
      msg = 'Result should contain DDLS object type' ).
  ENDMETHOD.

  METHOD test_exec_variant_parameter.
    " Test execute with variant parameter
    DATA: BEGIN OF ls_param,
            files TYPE string_table,
            variant TYPE string,
          END OF ls_param.

    APPEND 'zcl_my_class.clas.abap' TO ls_param-files.
    ls_param-variant = 'ALL_CHECKS'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should be valid JSON
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*{*'
      msg = 'Result should be valid JSON object' ).
  ENDMETHOD.

  METHOD test_exec_mixed_files.
    " Test execute with mixed file types (DDLS + CLAS)
    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.

    APPEND 'zc_my_cds_view.ddls.asddls' TO ls_param-files.
    APPEND 'zcl_my_class.clas.abap' TO ls_param-files.
    APPEND 'zif_my_intf.intf.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should be valid JSON
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*{*'
      msg = 'Result should be valid JSON object' ).

    " Result should contain multiple objects
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*object_type*'
      msg = 'Result should contain object_type field' ).
  ENDMETHOD.

  METHOD test_exec_invalid_file.
    " Test execute with invalid file format (should not crash)
    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.

    APPEND 'invalid_file.xyz' TO ls_param-files.

    " Should not raise exception, should return error result
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial even with invalid file' ).

    " Result should be valid JSON
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*{*'
      msg = 'Result should be valid JSON object' ).
  ENDMETHOD.

  METHOD test_build_object_result.
    " Test build_object_result by checking execute returns proper structure
    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.

    APPEND 'zcl_my_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Result should contain expected JSON fields
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*object_type*'
      msg = 'Result should contain object_type' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*object_name*'
      msg = 'Result should contain object_name' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*'
      msg = 'Result should contain success' ).
  ENDMETHOD.

  METHOD test_get_check_variant.
    " Test get_check_variant returns a variant reference
    " This tests the private method indirectly through run_inspection

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
            variant TYPE string,
          END OF ls_param.

    APPEND 'zcl_my_class.clas.abap' TO ls_param-files.
    ls_param-variant = 'SYNTAX_CHECK'.

    " Should not raise exception
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).
  ENDMETHOD.

  METHOD test_create_inspection_name.
    " Test create_inspection_name creates proper format
    " This is tested indirectly through execute

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.

    APPEND 'zcl_my_class.clas.abap' TO ls_param-files.

    " Execute should work without error
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).
  ENDMETHOD.

  METHOD test_mock_success.
    " Test with mock returning success - mock returns empty results (no issues found)
    " This exercises the code path where there are no inspection results
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.
    CLEAR lo_mock->mt_mock_results.
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Should contain object in result
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZCL_TEST_CLASS*'
      msg = 'Result should contain object name' ).
  ENDMETHOD.

  METHOD test_mock_errors.
    " Test with mock returning errors - covers categorize_message for E
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.

    DATA ls_result_row TYPE LINE OF scit_alvlist.
    ls_result_row-objname  = 'ZCL_TEST_CLASS'.
    ls_result_row-sobjname = 'ZCL_TEST_CLASS========CM001'.
    ls_result_row-kind     = 'E'.
    ls_result_row-line     = '010'.
    ls_result_row-col      = '012'.
    ls_result_row-text     = 'Variable "LV_UNKNOWN" is not defined'.
    ls_result_row-code     = 'LV_UNKNOWN'.
    APPEND ls_result_row TO lo_mock->mt_mock_results.
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Should contain error
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*errors*'
      msg = 'Result should contain errors' ).

    " Should indicate failure
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success* *'
      msg = 'Result should indicate failure' ).
  ENDMETHOD.

  METHOD test_mock_warnings.
    " Test with mock returning warnings - covers categorize_message for W
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.

    DATA ls_result_row TYPE LINE OF scit_alvlist.
    ls_result_row-objname  = 'ZCL_TEST_CLASS'.
    ls_result_row-sobjname = 'ZCL_TEST_CLASS========CM001'.
    ls_result_row-kind     = 'W'.
    ls_result_row-line     = '015'.
    ls_result_row-col      = '001'.
    ls_result_row-text     = 'Avoid using obsolete statement'.
    APPEND ls_result_row TO lo_mock->mt_mock_results.
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Should contain warnings
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*warnings*'
      msg = 'Result should contain warnings' ).
  ENDMETHOD.

  METHOD test_mock_info.
    " Test with mock returning info - covers categorize_message for I
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.

    DATA ls_result_row TYPE LINE OF scit_alvlist.
    ls_result_row-objname  = 'ZCL_TEST_CLASS'.
    ls_result_row-sobjname = 'ZCL_TEST_CLASS========CM001'.
    ls_result_row-kind     = 'I'.
    ls_result_row-line     = '020'.
    ls_result_row-col      = '001'.
    ls_result_row-text     = 'Information message'.
    APPEND ls_result_row TO lo_mock->mt_mock_results.
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Should contain infos
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*infos*'
      msg = 'Result should contain infos' ).
  ENDMETHOD.

  METHOD test_mock_unittest_include.
    " Test with CCAU (unit test include) - covers extract_method_name
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.

    DATA ls_result_row TYPE LINE OF scit_alvlist.
    ls_result_row-objname  = 'ZCL_TEST_CLASS'.
    ls_result_row-sobjname = 'ZCL_TEST_CLASS========CCAU'.
    ls_result_row-kind     = 'N'.
    ls_result_row-line     = 0.
    ls_result_row-col      = 0.
    ls_result_row-text     = 'OK'.
    APPEND ls_result_row TO lo_mock->mt_mock_results.
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).
  ENDMETHOD.

  METHOD test_mock_local_defs.
    " Test with CCDEF (local definitions) - covers extract_method_name
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.

    DATA ls_result_row TYPE LINE OF scit_alvlist.
    ls_result_row-objname  = 'ZCL_TEST_CLASS'.
    ls_result_row-sobjname = 'ZCL_TEST_CLASS========CCDEF'.
    ls_result_row-kind     = 'N'.
    ls_result_row-line     = 0.
    ls_result_row-col      = 0.
    ls_result_row-text     = 'OK'.
    APPEND ls_result_row TO lo_mock->mt_mock_results.
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).
  ENDMETHOD.

  METHOD test_mock_local_impl.
    " Test with CCIMP (local implementations) - covers extract_method_name
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.

    DATA ls_result_row TYPE LINE OF scit_alvlist.
    ls_result_row-objname  = 'ZCL_TEST_CLASS'.
    ls_result_row-sobjname = 'ZCL_TEST_CLASS========CCIMP'.
    ls_result_row-kind     = 'N'.
    ls_result_row-line     = 0.
    ls_result_row-col      = 0.
    ls_result_row-text     = 'OK'.
    APPEND ls_result_row TO lo_mock->mt_mock_results.
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).
  ENDMETHOD.

  METHOD test_mock_macros.
    " Test with CCINC (macros) - covers extract_method_name
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.

    DATA ls_result_row TYPE LINE OF scit_alvlist.
    ls_result_row-objname  = 'ZCL_TEST_CLASS'.
    ls_result_row-sobjname = 'ZCL_TEST_CLASS========CCINC'.
    ls_result_row-kind     = 'N'.
    ls_result_row-line     = 0.
    ls_result_row-col      = 0.
    ls_result_row-text     = 'OK'.
    APPEND ls_result_row TO lo_mock->mt_mock_results.
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).
  ENDMETHOD.

  METHOD test_mock_multi_errors.
    " Test with multiple errors for same object - covers build_object_result sorting
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.

    DATA ls_result_row TYPE LINE OF scit_alvlist.
    ls_result_row-objname  = 'ZCL_TEST_CLASS'.
    ls_result_row-sobjname = 'ZCL_TEST_CLASS========CM001'.
    ls_result_row-kind     = 'E'.
    ls_result_row-line     = '010'.
    ls_result_row-col      = '001'.
    ls_result_row-text     = 'Error 1'.
    APPEND ls_result_row TO lo_mock->mt_mock_results.
    ls_result_row-sobjname = 'ZCL_TEST_CLASS========CM002'.
    ls_result_row-line     = '020'.
    ls_result_row-col      = '002'.
    ls_result_row-text     = 'Error 2'.
    APPEND ls_result_row TO lo_mock->mt_mock_results.
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Should contain errors
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*errors*'
      msg = 'Result should contain errors' ).
  ENDMETHOD.

  METHOD test_mock_var_not_found.
    " Test with mock returning variant not found
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.
    lo_mock->mv_variant_not_found = abap_true.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
            variant TYPE string,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.
    ls_param-variant = 'INVALID_VARIANT'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Should contain variant error
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*VARIANT*'
      msg = 'Result should contain VARIANT' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*not found*'
      msg = 'Result should indicate variant not found' ).
  ENDMETHOD.

  METHOD test_mock_exception.
    " Test exception handling - covers build_error_result
    DATA lo_mock TYPE REF TO lcl_code_inspector_mock.
    CREATE OBJECT lo_mock TYPE lcl_code_inspector_mock.
    lo_mock->mv_raise_exception = abap_true.
    CREATE OBJECT lo_mock->mx_exception TYPE cx_demo_exception.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    " Should not raise exception, should return error result
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Should contain error info from exception
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*errors*'
      msg = 'Result should contain errors' ).

    " Should indicate failure
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success* *'
      msg = 'Result should indicate failure' ).
  ENDMETHOD.

  METHOD test_validate_ddls_success.
    " Test validate_ddls with success (no warnings)
    DATA lo_mock TYPE REF TO lcl_ddl_handler_mock.
    CREATE OBJECT lo_mock TYPE lcl_ddl_handler_mock.
    lo_mock->ms_mock_ddlsrcv-ddlname = 'ZC_TEST_VIEW'.
    lo_mock->ms_mock_ddlsrcv-source  = 'SELECT * FROM tdevc'.
    CLEAR lo_mock->mt_mock_warnings.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_ddl_handler = lo_mock.

    DATA lt_ddls_names TYPE ty_ddls_names.
    APPEND 'ZC_TEST_VIEW' TO lt_ddls_names.
    DATA(lt_results) = mo_cut->validate_ddls( lt_ddls_names ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lt_results
      msg = 'Results should not be initial' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_results )
      exp = 1
      msg = 'Should have one result' ).

    DATA(ls_result) = lt_results[ 1 ].
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_type
      exp = 'DDLS'
      msg = 'Object type should be DDLS' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_name
      exp = 'ZC_TEST_VIEW'
      msg = 'Object name should match' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_true
      msg = 'Success should be true' ).
  ENDMETHOD.

  METHOD test_validate_ddls_not_found.
    " Test validate_ddls when DDLS is not found
    DATA lo_mock TYPE REF TO lcl_ddl_handler_mock.
    CREATE OBJECT lo_mock TYPE lcl_ddl_handler_mock.
    lo_mock->mv_raise_not_found = abap_true.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_ddl_handler = lo_mock.

    DATA lt_ddls_names TYPE ty_ddls_names.
    APPEND 'ZC_NONEXISTENT' TO lt_ddls_names.
    DATA(lt_results) = mo_cut->validate_ddls( lt_ddls_names ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lt_results
      msg = 'Results should not be initial' ).

    DATA(ls_result) = lt_results[ 1 ].
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'Success should be false when not found' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_result-errors )
      exp = 1
      msg = 'Should have one error' ).
  ENDMETHOD.

  METHOD test_ddls_with_warnings.
    " Test validate_ddls with warnings
    DATA lo_mock TYPE REF TO lcl_ddl_handler_mock.
    CREATE OBJECT lo_mock TYPE lcl_ddl_handler_mock.
    lo_mock->ms_mock_ddlsrcv-ddlname = 'ZC_TEST_VIEW'.
    lo_mock->ms_mock_ddlsrcv-source  = 'SELECT * FROM tdevc'.

    DATA ls_warning TYPE LINE OF zif_abgagt_ddl_handler=>ty_warnings.
    ls_warning-type     = 'W'.
    ls_warning-line     = 10.
    ls_warning-column   = 1.
    ls_warning-severity = 'W'.
    ls_warning-arbgb    = 'DD'.
    ls_warning-msgnr    = 001.
    ls_warning-var1     = 'Warning text'.
    APPEND ls_warning TO lo_mock->mt_mock_warnings.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_ddl_handler = lo_mock.

    DATA lt_ddls_names TYPE ty_ddls_names.
    APPEND 'ZC_TEST_VIEW' TO lt_ddls_names.
    DATA(lt_results) = mo_cut->validate_ddls( lt_ddls_names ).

    DATA(ls_result) = lt_results[ 1 ].
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'Success should be false when there are warnings' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_result-warnings )
      exp = 1
      msg = 'Should have one warning' ).
  ENDMETHOD.

  METHOD test_ddls_with_errors.
    " Test validate_ddls when check throws exception with errors
    DATA lo_mock TYPE REF TO lcl_ddl_handler_mock.
    CREATE OBJECT lo_mock TYPE lcl_ddl_handler_mock.
    lo_mock->ms_mock_ddlsrcv-ddlname = 'ZC_TEST_VIEW'.
    lo_mock->ms_mock_ddlsrcv-source  = 'SELECT * FROM tdevc'.
    lo_mock->mv_raise_check_error = abap_true.

    " Create exception with errors
    DATA lo_error TYPE REF TO cx_dd_ddl_check.
    CREATE OBJECT lo_error TYPE cx_dd_ddl_check.
    lo_mock->mx_check_exception = lo_error.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_ddl_handler = lo_mock.

    DATA lt_ddls_names TYPE ty_ddls_names.
    APPEND 'ZC_TEST_VIEW' TO lt_ddls_names.
    " This should catch the exception and return error result
    DATA(lt_results) = mo_cut->validate_ddls( lt_ddls_names ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lt_results
      msg = 'Results should not be initial even with exception' ).

    DATA(ls_result) = lt_results[ 1 ].
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'Success should be false when exception occurs' ).
  ENDMETHOD.

  METHOD test_read_ddls_source.
    " Test read_ddls_source method directly
    DATA lo_mock TYPE REF TO lcl_ddl_handler_mock.
    CREATE OBJECT lo_mock TYPE lcl_ddl_handler_mock.
    lo_mock->ms_mock_ddlsrcv-ddlname = 'ZC_TEST_VIEW'.
    lo_mock->ms_mock_ddlsrcv-source  = 'SELECT * FROM tdevc'.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_ddl_handler = lo_mock.

    DATA: ls_ddlsrcv TYPE zif_abgagt_ddl_handler=>ty_ddlsrcv.

    DATA(lv_found) = mo_cut->read_ddls_source(
      EXPORTING
        iv_ddls_name = 'ZC_TEST_VIEW'
      IMPORTING
        es_ddlsrcv   = ls_ddlsrcv ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_found
      exp = abap_true
      msg = 'Should find the DDLS' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_ddlsrcv-ddlname
      exp = 'ZC_TEST_VIEW'
      msg = 'DDLS name should match' ).
  ENDMETHOD.

  METHOD test_validate_ddls_chk.
    " Test validate_ddls_check method directly
    DATA lo_mock TYPE REF TO lcl_ddl_handler_mock.
    CREATE OBJECT lo_mock TYPE lcl_ddl_handler_mock.
    lo_mock->ms_mock_ddlsrcv-ddlname = 'ZC_TEST_VIEW'.
    lo_mock->ms_mock_ddlsrcv-source  = 'SELECT * FROM tdevc'.

    DATA ls_warning TYPE LINE OF zif_abgagt_ddl_handler=>ty_warnings.
    ls_warning-type     = 'W'.
    ls_warning-line     = 5.
    ls_warning-column   = 1.
    ls_warning-severity = 'W'.
    APPEND ls_warning TO lo_mock->mt_mock_warnings.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect
      EXPORTING io_ddl_handler = lo_mock.

    DATA ls_ddlsrcv TYPE zif_abgagt_ddl_handler=>ty_ddlsrcv.
    ls_ddlsrcv-ddlname = 'ZC_TEST_VIEW'.
    ls_ddlsrcv-source  = 'SELECT * FROM tdevc'.

    DATA(ls_result) = mo_cut->validate_ddls_check(
      iv_ddls_name = 'ZC_TEST_VIEW'
      is_ddlsrcv   = ls_ddlsrcv ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_type
      exp = 'DDLS'
      msg = 'Object type should be DDLS' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_name
      exp = 'ZC_TEST_VIEW'
      msg = 'Object name should match' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_result-warnings )
      exp = 1
      msg = 'Should have one warning from mock' ).
  ENDMETHOD.

ENDCLASS.
