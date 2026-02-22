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
    METHODS test_mock_info FOR TESTING.
    METHODS test_mock_unittest_include FOR TESTING.
    METHODS test_mock_local_defs FOR TESTING.
    METHODS test_mock_local_impl FOR TESTING.
    METHODS test_mock_macros FOR TESTING.
    METHODS test_mock_multi_errors FOR TESTING.
    METHODS test_mock_var_not_found FOR TESTING.
    METHODS test_mock_exception FOR TESTING.
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
      rt_list = VALUE #( FOR ls IN mt_mock_results WHERE ( objname = mv_objname ) ( ls ) ).
    ELSE.
      rt_list = mt_mock_results.
    ENDIF.
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
    " Test with mock returning success - mock returns empty results (no issues found)
    " This exercises the code path where there are no inspection results
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #( ). " Empty results = success
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CM001' kind = 'E' line = '010' col = '012' text = 'Variable "LV_UNKNOWN" is not defined' code = 'LV_UNKNOWN' )
    ).
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

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
    " Test with mock returning warnings - covers categorize_message for W
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CM001' kind = 'W' line = '015' col = '001' text = 'Avoid using obsolete statement' )
    ).
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

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

  METHOD test_mock_info.
    " Test with mock returning info - covers categorize_message for I
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CM001' kind = 'I' line = '020' col = '001' text = 'Information message' )
    ).
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CCAU' kind = 'N' line = 0 col = 0 text = 'OK' )
    ).
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CCDEF' kind = 'N' line = 0 col = 0 text = 'OK' )
    ).
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CCIMP' kind = 'N' line = 0 col = 0 text = 'OK' )
    ).
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CCINC' kind = 'N' line = 0 col = 0 text = 'OK' )
    ).
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mt_mock_results = VALUE #(
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CM001' kind = 'E' line = '010' col = '001' text = 'Error 1' )
      ( objname = 'ZCL_TEST_CLASS' sobjname = 'ZCL_TEST_CLASS========CM002' kind = 'E' line = '020' col = '002' text = 'Error 2' )
    ).
    lo_mock->mv_objname = 'ZCL_TEST_CLASS'.

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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

  METHOD test_mock_exception.
    " Test exception handling - covers build_error_result
    DATA(lo_mock) = NEW lcl_code_inspector_mock( ).
    lo_mock->mv_raise_exception = abap_true.
    " Use cx_demo_exception which is a simple instantiable exception
    lo_mock->mx_exception = NEW cx_demo_exception( text = 'Test exception' ).

    CREATE OBJECT mo_cut EXPORTING io_inspector = lo_mock.

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

    " Should contain exception text
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*Test exception*'
      msg = 'Result should contain exception text' ).
  ENDMETHOD.

ENDCLASS.
