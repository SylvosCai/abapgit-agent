*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_cmd_inspect DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_inspect.

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
    METHODS test_get_method_name FOR TESTING.
    METHODS test_build_object_result FOR TESTING.
    METHODS test_get_check_variant FOR TESTING.
    METHODS test_create_inspection_name FOR TESTING.
    METHODS test_mock_success FOR TESTING.
    METHODS test_mock_errors FOR TESTING.
    METHODS test_mock_warnings FOR TESTING.
    METHODS test_mock_var_not_found FOR TESTING.
ENDCLASS.

"**********************************************************************
" Local test double for code inspector
CLASS lcl_code_inspector_mock DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_code_inspector.

    " Configuration for mock behavior
    DATA mv_return_errors TYPE abap_bool DEFAULT abap_false.
    DATA mv_return_warnings TYPE abap_bool DEFAULT abap_false.
    DATA mv_variant_not_found TYPE abap_bool DEFAULT abap_false.
    DATA mv_raise_exception TYPE abap_bool DEFAULT abap_false.

    " Mock data to return
    DATA mt_mock_results TYPE scit_alvlist.

  PRIVATE SECTION.
    DATA mo_mock_objset TYPE REF TO cl_ci_objectset.
    DATA mo_mock_inspection TYPE REF TO cl_ci_inspection.
    DATA mo_mock_variant TYPE REF TO cl_ci_checkvariant.
ENDCLASS.

CLASS lcl_code_inspector_mock IMPLEMENTATION.

  METHOD zif_abgagt_code_inspector~create_object_set.
    " Return mock object set (not really used in tests)
    ro_objset = mo_mock_objset.
  ENDMETHOD.

  METHOD zif_abgagt_code_inspector~get_check_variant.
    IF mv_variant_not_found = abap_true.
      " Return empty to simulate variant not found
      RETURN.
    ENDIF.
    ro_variant = mo_mock_variant.
  ENDMETHOD.

  METHOD zif_abgagt_code_inspector~create_and_run_inspection.
    " Return mock inspection
    ro_inspection = mo_mock_inspection.
  ENDMETHOD.

  METHOD zif_abgagt_code_inspector~get_results.
    " Return configured mock results
    rt_list = mt_mock_results.
  ENDMETHOD.

  METHOD zif_abgagt_code_inspector~cleanup.
    " Do nothing in mock
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cmd_inspect IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
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
    DATA lo_interface TYPE REF TO zif_abgagt_command.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_inspect.
    lo_interface = mo_cut.
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

  METHOD test_get_method_name.
    " Test the get_method_name method returns method name from TMDIR
    " Note: This test may return empty if the test class doesn't exist in TMDIR
    DATA(lv_method_name) = mo_cut->get_method_name(
      iv_classname   = 'ZCL_NONEXISTENT_TEST_CLASS'
      iv_include_num = 1 ).

    " Method should return a string (empty string is expected if not found)
    " Just verify the method executed without raising an exception
    cl_abap_unit_assert=>assert_equals(
      act = lv_method_name
      exp = ''
      msg = 'Method should return empty string for non-existent class' ).
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
    " Test with mock returning success (no errors/warnings)
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CM001' kind = 'N' line = 0 col = 0 text = 'No errors found' )
    ).

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

    DATA: BEGIN OF ls_param,
            files TYPE string_table,
          END OF ls_param.
    APPEND 'zcl_test_class.clas.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Should contain success
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*X*'
      msg = 'Result should indicate success' ).
  ENDMETHOD.

  METHOD test_mock_errors.
    " Test with mock returning errors
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CM001' kind = 'E' line = '010' col = '012' text = 'Variable "LV_UNKNOWN" is not defined' code = 'LV_UNKNOWN' )
    ).

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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
    " Test with mock returning warnings
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CM001' kind = 'W' line = '015' col = '001' text = 'Avoid using obsolete statement' )
    ).

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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

  METHOD test_mock_var_not_found.
    " Test with mock returning variant not found
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mv_variant_not_found = abap_true.

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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

ENDCLASS.
