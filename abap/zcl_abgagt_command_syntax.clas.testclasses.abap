*"* use this source file for your test class implementation
*"* local test class

"**********************************************************************
" Test double for syntax checker interface
CLASS lcl_syntax_checker_mock DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

    " Configuration for mock behavior
    DATA mv_object_type TYPE string.
    DATA ms_mock_result TYPE zif_abgagt_syntax_checker=>ty_result.

    METHODS constructor
      IMPORTING iv_object_type TYPE string.

ENDCLASS.

CLASS lcl_syntax_checker_mock IMPLEMENTATION.

  METHOD constructor.
    mv_object_type = iv_object_type.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = mv_object_type.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~check.
    " Return configured mock result
    rs_result = ms_mock_result.
    " Fill object info if not set
    IF rs_result-object_type IS INITIAL.
      rs_result-object_type = mv_object_type.
    ENDIF.
    IF rs_result-object_name IS INITIAL.
      rs_result-object_name = iv_name.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~set_fixpt.
    " Mock implementation - do nothing
  ENDMETHOD.

ENDCLASS.

"**********************************************************************
" Test class for syntax command
CLASS ltcl_cmd_syntax DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_syntax.

    METHODS setup.
    METHODS test_get_name FOR TESTING.
    METHODS test_interface FOR TESTING.
    METHODS test_exec_no_objects FOR TESTING.
    METHODS test_exec_single_class FOR TESTING.
    METHODS test_exec_single_interface FOR TESTING.
    METHODS test_exec_single_program FOR TESTING.
    METHODS test_exec_multi_objects FOR TESTING.
    METHODS test_exec_unsupported_type FOR TESTING.
    METHODS test_exec_class_with_locals FOR TESTING.
    METHODS test_exec_clas_locals_def FOR TESTING.
    METHODS test_exec_clas_locals_imp FOR TESTING.
    METHODS test_exec_prog_with_uccheck FOR TESTING.
    METHODS test_exec_invalid_uccheck FOR TESTING.
    METHODS test_exec_mixed_success_fail FOR TESTING.
ENDCLASS.

CLASS ltcl_cmd_syntax IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_name.
    " Test that get_name returns the correct command name
    DATA(lv_name) = mo_cut->zif_abgagt_command~get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'SYNTAX'
      msg = 'Command name should be SYNTAX' ).
  ENDMETHOD.

  METHOD test_interface.
    " Test that the class implements the command interface
    DATA lo_interface TYPE REF TO zif_abgagt_command.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_syntax.
    lo_interface = mo_cut.
    cl_abap_unit_assert=>assert_bound(
      act = lo_interface
      msg = 'Object should implement zif_abgagt_command interface' ).
  ENDMETHOD.

  METHOD test_exec_no_objects.
    " Test execute with no objects - should return error
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain error message
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*No objects provided*'
      msg = 'Result should indicate no objects provided' ).

    " Success should be false
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"SUCCESS":false*'
      msg = 'Success should be false' ).
  ENDMETHOD.

  METHOD test_exec_single_class.
    " Test execute with a single class
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-objects = VALUE #( (
      type   = 'CLAS'
      name   = 'ZCL_TEST_CLASS'
      source = 'CLASS zcl_test_class DEFINITION PUBLIC.' && cl_abap_char_utilities=>newline &&
               '  PUBLIC SECTION.' && cl_abap_char_utilities=>newline &&
               'ENDCLASS.' && cl_abap_char_utilities=>newline &&
               'CLASS zcl_test_class IMPLEMENTATION.' && cl_abap_char_utilities=>newline &&
               'ENDCLASS.'
    ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should be valid JSON
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*{*'
      msg = 'Result should be valid JSON object' ).

    " Result should contain CLAS
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*CLAS*'
      msg = 'Result should contain CLAS object type' ).

    " Result should contain the class name
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZCL_TEST_CLASS*'
      msg = 'Result should contain class name' ).
  ENDMETHOD.

  METHOD test_exec_single_interface.
    " Test execute with a single interface
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-objects = VALUE #( (
      type   = 'INTF'
      name   = 'ZIF_TEST_INTF'
      source = 'INTERFACE zif_test_intf PUBLIC.' && cl_abap_char_utilities=>newline &&
               '  METHODS test_method.' && cl_abap_char_utilities=>newline &&
               'ENDINTERFACE.'
    ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain INTF
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*INTF*'
      msg = 'Result should contain INTF object type' ).

    " Result should contain the interface name
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZIF_TEST_INTF*'
      msg = 'Result should contain interface name' ).
  ENDMETHOD.

  METHOD test_exec_single_program.
    " Test execute with a single program
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-objects = VALUE #( (
      type   = 'PROG'
      name   = 'ZTEST_PROGRAM'
      source = 'REPORT ztest_program.' && cl_abap_char_utilities=>newline &&
               'DATA lv_test TYPE string.' && cl_abap_char_utilities=>newline &&
               'lv_test = ''Hello''.'
    ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain PROG
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*PROG*'
      msg = 'Result should contain PROG object type' ).

    " Result should contain the program name
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZTEST_PROGRAM*'
      msg = 'Result should contain program name' ).
  ENDMETHOD.

  METHOD test_exec_multi_objects.
    " Test execute with multiple objects
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-objects = VALUE #(
      ( type = 'CLAS' name = 'ZCL_CLASS1'
        source = 'CLASS zcl_class1 DEFINITION PUBLIC.' && cl_abap_char_utilities=>newline &&
                 'ENDCLASS.' && cl_abap_char_utilities=>newline &&
                 'CLASS zcl_class1 IMPLEMENTATION.' && cl_abap_char_utilities=>newline &&
                 'ENDCLASS.' )
      ( type = 'INTF' name = 'ZIF_INTF1'
        source = 'INTERFACE zif_intf1 PUBLIC.' && cl_abap_char_utilities=>newline &&
                 'ENDINTERFACE.' )
      ( type = 'PROG' name = 'ZPROG1'
        source = 'REPORT zprog1.' )
    ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain all object types
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*CLAS*'
      msg = 'Result should contain CLAS' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*INTF*'
      msg = 'Result should contain INTF' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*PROG*'
      msg = 'Result should contain PROG' ).

    " Result should contain RESULTS array
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*RESULTS*'
      msg = 'Result should contain RESULTS array' ).
  ENDMETHOD.

  METHOD test_exec_unsupported_type.
    " Test execute with unsupported object type
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-objects = VALUE #( (
      type   = 'FUGR'
      name   = 'ZTEST_FUGR'
      source = 'FUNCTION z_test_func.' && cl_abap_char_utilities=>newline &&
               'ENDFUNCTION.'
    ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should indicate unsupported type
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*Unsupported object type*'
      msg = 'Result should indicate unsupported type' ).

    " Result should suggest using pull command
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*pull*'
      msg = 'Result should suggest using pull command' ).

    " Success should be false
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"SUCCESS":false*'
      msg = 'Success should be false' ).
  ENDMETHOD.

  METHOD test_exec_class_with_locals.
    " Test execute with class that has local classes
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-objects = VALUE #( (
      type       = 'CLAS'
      name       = 'ZCL_TEST_CLASS'
      source     = 'CLASS zcl_test_class DEFINITION PUBLIC.' && cl_abap_char_utilities=>newline &&
                   '  PUBLIC SECTION.' && cl_abap_char_utilities=>newline &&
                   '    METHODS test_method.' && cl_abap_char_utilities=>newline &&
                   'ENDCLASS.' && cl_abap_char_utilities=>newline &&
                   'CLASS zcl_test_class IMPLEMENTATION.' && cl_abap_char_utilities=>newline &&
                   '  METHOD test_method.' && cl_abap_char_utilities=>newline &&
                   '    DATA(lo_helper) = NEW lcl_helper( ).' && cl_abap_char_utilities=>newline &&
                   '  ENDMETHOD.' && cl_abap_char_utilities=>newline &&
                   'ENDCLASS.'
      locals_def = 'CLASS lcl_helper DEFINITION.' && cl_abap_char_utilities=>newline &&
                   'ENDCLASS.'
      locals_imp = 'CLASS lcl_helper IMPLEMENTATION.' && cl_abap_char_utilities=>newline &&
                   'ENDCLASS.'
    ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain the class name
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZCL_TEST_CLASS*'
      msg = 'Result should contain class name' ).
  ENDMETHOD.

  METHOD test_exec_clas_locals_def.
    " Test execute with class that has only local definitions (no implementations)
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-objects = VALUE #( (
      type       = 'CLAS'
      name       = 'ZCL_TEST_DEF_ONLY'
      source     = 'CLASS zcl_test_def_only DEFINITION PUBLIC.' && cl_abap_char_utilities=>newline &&
                   '  PUBLIC SECTION.' && cl_abap_char_utilities=>newline &&
                   '    TYPES ty_test TYPE i.' && cl_abap_char_utilities=>newline &&
                   'ENDCLASS.' && cl_abap_char_utilities=>newline &&
                   'CLASS zcl_test_def_only IMPLEMENTATION.' && cl_abap_char_utilities=>newline &&
                   'ENDCLASS.'
      locals_def = 'TYPES ty_local TYPE string.'
    ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain the class name
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZCL_TEST_DEF_ONLY*'
      msg = 'Result should contain class name' ).
  ENDMETHOD.

  METHOD test_exec_clas_locals_imp.
    " Test execute with class that has only local implementations (no definitions)
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-objects = VALUE #( (
      type       = 'CLAS'
      name       = 'ZCL_TEST_IMP_ONLY'
      source     = 'CLASS zcl_test_imp_only DEFINITION PUBLIC.' && cl_abap_char_utilities=>newline &&
                   '  PUBLIC SECTION.' && cl_abap_char_utilities=>newline &&
                   'ENDCLASS.' && cl_abap_char_utilities=>newline &&
                   'CLASS zcl_test_imp_only IMPLEMENTATION.' && cl_abap_char_utilities=>newline &&
                   'ENDCLASS.'
      locals_imp = 'DATA gv_local TYPE string.'
    ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain the class name
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZCL_TEST_IMP_ONLY*'
      msg = 'Result should contain class name' ).
  ENDMETHOD.

  METHOD test_exec_prog_with_uccheck.
    " Test execute with program and uccheck parameter
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-uccheck = '5'. " ABAP Cloud mode
    ls_param-objects = VALUE #( (
      type   = 'PROG'
      name   = 'ZTEST_CLOUD_PROG'
      source = 'REPORT ztest_cloud_prog.' && cl_abap_char_utilities=>newline &&
               'DATA lv_test TYPE string.'
    ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain the program name
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZTEST_CLOUD_PROG*'
      msg = 'Result should contain program name' ).
  ENDMETHOD.

  METHOD test_exec_invalid_uccheck.
    " Test execute with invalid uccheck value - should default to 'X'
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-uccheck = 'Z'. " Invalid value - should fallback to 'X'
    ls_param-objects = VALUE #( (
      type   = 'PROG'
      name   = 'ZTEST_INVALID_UC'
      source = 'REPORT ztest_invalid_uc.' && cl_abap_char_utilities=>newline &&
               'DATA lv_test TYPE string.'
    ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain the program name (should work with default uccheck)
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZTEST_INVALID_UC*'
      msg = 'Result should contain program name' ).
  ENDMETHOD.

  METHOD test_exec_mixed_success_fail.
    " Test execute with mixed success and failure
    DATA ls_param TYPE zcl_abgagt_command_syntax=>ty_syntax_params.

    ls_param-objects = VALUE #(
      " Valid class
      ( type = 'CLAS' name = 'ZCL_VALID'
        source = 'CLASS zcl_valid DEFINITION PUBLIC.' && cl_abap_char_utilities=>newline &&
                 'ENDCLASS.' && cl_abap_char_utilities=>newline &&
                 'CLASS zcl_valid IMPLEMENTATION.' && cl_abap_char_utilities=>newline &&
                 'ENDCLASS.' )
      " Invalid (unsupported type)
      ( type = 'TABL' name = 'ZTABLE'
        source = 'some source' )
    ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should contain both objects
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZCL_VALID*'
      msg = 'Result should contain valid class' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZTABLE*'
      msg = 'Result should contain unsupported table' ).

    " Overall success should be false due to unsupported type
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"SUCCESS":false*'
      msg = 'Overall success should be false' ).
  ENDMETHOD.

ENDCLASS.

"**********************************************************************
" Test class for syntax checker factory
CLASS ltcl_syntax_factory DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test_create_clas FOR TESTING.
    METHODS test_create_intf FOR TESTING.
    METHODS test_create_prog FOR TESTING.
    METHODS test_create_unsupported FOR TESTING.
    METHODS test_create_lowercase FOR TESTING.
    METHODS test_is_supported_clas FOR TESTING.
    METHODS test_is_supported_unsupported FOR TESTING.
ENDCLASS.

CLASS ltcl_syntax_factory IMPLEMENTATION.

  METHOD test_create_clas.
    " Test factory creates CLAS checker
    DATA(lo_checker) = zcl_abgagt_syntax_chk_factory=>create( 'CLAS' ).

    cl_abap_unit_assert=>assert_bound(
      act = lo_checker
      msg = 'Should create CLAS checker' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_checker->get_object_type( )
      exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
  ENDMETHOD.

  METHOD test_create_intf.
    " Test factory creates INTF checker
    DATA(lo_checker) = zcl_abgagt_syntax_chk_factory=>create( 'INTF' ).

    cl_abap_unit_assert=>assert_bound(
      act = lo_checker
      msg = 'Should create INTF checker' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_checker->get_object_type( )
      exp = 'INTF'
      msg = 'Object type should be INTF' ).
  ENDMETHOD.

  METHOD test_create_prog.
    " Test factory creates PROG checker
    DATA(lo_checker) = zcl_abgagt_syntax_chk_factory=>create( 'PROG' ).

    cl_abap_unit_assert=>assert_bound(
      act = lo_checker
      msg = 'Should create PROG checker' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_checker->get_object_type( )
      exp = 'PROG'
      msg = 'Object type should be PROG' ).
  ENDMETHOD.

  METHOD test_create_unsupported.
    " Test factory returns empty for unsupported type
    DATA(lo_checker) = zcl_abgagt_syntax_chk_factory=>create( 'FUGR' ).

    cl_abap_unit_assert=>assert_not_bound(
      act = lo_checker
      msg = 'Should not create checker for unsupported type' ).
  ENDMETHOD.

  METHOD test_create_lowercase.
    " Test factory handles lowercase input
    DATA(lo_checker) = zcl_abgagt_syntax_chk_factory=>create( 'clas' ).

    cl_abap_unit_assert=>assert_bound(
      act = lo_checker
      msg = 'Should create checker for lowercase input' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_checker->get_object_type( )
      exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
  ENDMETHOD.

  METHOD test_is_supported_clas.
    " Test is_supported returns true for CLAS
    DATA(lv_supported) = zcl_abgagt_syntax_chk_factory=>is_supported( 'CLAS' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_supported
      exp = abap_true
      msg = 'CLAS should be supported' ).
  ENDMETHOD.

  METHOD test_is_supported_unsupported.
    " Test is_supported returns false for unsupported type
    DATA(lv_supported) = zcl_abgagt_syntax_chk_factory=>is_supported( 'FUGR' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_supported
      exp = abap_false
      msg = 'FUGR should not be supported' ).
  ENDMETHOD.

ENDCLASS.

"**********************************************************************
" Test class for class syntax checker
CLASS ltcl_syntax_chk_clas DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_syntax_chk_clas.

    METHODS setup.
    METHODS test_get_object_type FOR TESTING.
    METHODS test_check_valid_class FOR TESTING.
    METHODS test_check_empty_source FOR TESTING.
    METHODS test_check_with_locals FOR TESTING.
    METHODS test_clear_locals FOR TESTING.
ENDCLASS.

CLASS ltcl_syntax_chk_clas IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_object_type.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->zif_abgagt_syntax_checker~get_object_type( )
      exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
  ENDMETHOD.

  METHOD test_check_valid_class.
    " Test checking valid class source
    DATA lt_source TYPE string_table.
    APPEND 'CLASS zcl_test DEFINITION PUBLIC.' TO lt_source.
    APPEND '  PUBLIC SECTION.' TO lt_source.
    APPEND 'ENDCLASS.' TO lt_source.
    APPEND 'CLASS zcl_test IMPLEMENTATION.' TO lt_source.
    APPEND 'ENDCLASS.' TO lt_source.

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZCL_TEST'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_type
      exp = 'CLAS'
      msg = 'Object type should be CLAS' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_name
      exp = 'ZCL_TEST'
      msg = 'Object name should match' ).
  ENDMETHOD.

  METHOD test_check_empty_source.
    " Test checking empty source
    DATA lt_source TYPE string_table.

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZCL_EMPTY'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'Success should be false for empty source' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-error_count
      exp = 1
      msg = 'Should have 1 error' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_result-message
      exp = '*No source*'
      msg = 'Message should indicate no source' ).
  ENDMETHOD.

  METHOD test_check_with_locals.
    " Test checking class with local classes
    DATA lt_source TYPE string_table.
    APPEND 'CLASS zcl_test DEFINITION PUBLIC.' TO lt_source.
    APPEND '  PUBLIC SECTION.' TO lt_source.
    APPEND '    METHODS test_method.' TO lt_source.
    APPEND 'ENDCLASS.' TO lt_source.
    APPEND 'CLASS zcl_test IMPLEMENTATION.' TO lt_source.
    APPEND '  METHOD test_method.' TO lt_source.
    APPEND '    DATA(lo_helper) = NEW lcl_helper( ).' TO lt_source.
    APPEND '  ENDMETHOD.' TO lt_source.
    APPEND 'ENDCLASS.' TO lt_source.

    DATA lt_locals_def TYPE string_table.
    APPEND 'CLASS lcl_helper DEFINITION.' TO lt_locals_def.
    APPEND 'ENDCLASS.' TO lt_locals_def.

    DATA lt_locals_imp TYPE string_table.
    APPEND 'CLASS lcl_helper IMPLEMENTATION.' TO lt_locals_imp.
    APPEND 'ENDCLASS.' TO lt_locals_imp.

    mo_cut->set_locals_def( lt_locals_def ).
    mo_cut->set_locals_imp( lt_locals_imp ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZCL_TEST'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_name
      exp = 'ZCL_TEST'
      msg = 'Object name should match' ).
  ENDMETHOD.

  METHOD test_clear_locals.
    " Test clearing local classes
    DATA lt_locals_def TYPE string_table.
    APPEND 'CLASS lcl_test DEFINITION.' TO lt_locals_def.
    mo_cut->set_locals_def( lt_locals_def ).

    DATA lt_locals_imp TYPE string_table.
    APPEND 'CLASS lcl_test IMPLEMENTATION.' TO lt_locals_imp.
    mo_cut->set_locals_imp( lt_locals_imp ).

    " Clear locals
    mo_cut->clear_locals( ).

    " Now check - should work without local classes
    DATA lt_source TYPE string_table.
    APPEND 'CLASS zcl_test DEFINITION PUBLIC.' TO lt_source.
    APPEND 'ENDCLASS.' TO lt_source.
    APPEND 'CLASS zcl_test IMPLEMENTATION.' TO lt_source.
    APPEND 'ENDCLASS.' TO lt_source.

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZCL_TEST'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_name
      exp = 'ZCL_TEST'
      msg = 'Check should work after clearing locals' ).
  ENDMETHOD.

ENDCLASS.

"**********************************************************************
" Test class for interface syntax checker
CLASS ltcl_syntax_chk_intf DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_syntax_chk_intf.

    METHODS setup.
    METHODS test_get_object_type FOR TESTING.
    METHODS test_check_valid_intf FOR TESTING.
    METHODS test_check_empty_source FOR TESTING.
ENDCLASS.

CLASS ltcl_syntax_chk_intf IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_object_type.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->zif_abgagt_syntax_checker~get_object_type( )
      exp = 'INTF'
      msg = 'Object type should be INTF' ).
  ENDMETHOD.

  METHOD test_check_valid_intf.
    " Test checking valid interface source
    DATA lt_source TYPE string_table.
    APPEND 'INTERFACE zif_test PUBLIC.' TO lt_source.
    APPEND '  METHODS test_method.' TO lt_source.
    APPEND 'ENDINTERFACE.' TO lt_source.

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZIF_TEST'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_type
      exp = 'INTF'
      msg = 'Object type should be INTF' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_name
      exp = 'ZIF_TEST'
      msg = 'Object name should match' ).
  ENDMETHOD.

  METHOD test_check_empty_source.
    " Test checking empty source
    DATA lt_source TYPE string_table.

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZIF_EMPTY'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'Success should be false for empty source' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_result-message
      exp = '*No source*'
      msg = 'Message should indicate no source' ).
  ENDMETHOD.

ENDCLASS.

"**********************************************************************
" Test class for program syntax checker
CLASS ltcl_syntax_chk_prog DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_syntax_chk_prog.

    METHODS setup.
    METHODS test_get_object_type FOR TESTING.
    METHODS test_check_valid_prog FOR TESTING.
    METHODS test_check_empty_source FOR TESTING.
    METHODS test_set_uccheck FOR TESTING.
ENDCLASS.

CLASS ltcl_syntax_chk_prog IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_object_type.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->zif_abgagt_syntax_checker~get_object_type( )
      exp = 'PROG'
      msg = 'Object type should be PROG' ).
  ENDMETHOD.

  METHOD test_check_valid_prog.
    " Test checking valid program source
    DATA lt_source TYPE string_table.
    APPEND 'REPORT ztest_prog.' TO lt_source.
    APPEND 'DATA lv_test TYPE string.' TO lt_source.

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZTEST_PROG'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_type
      exp = 'PROG'
      msg = 'Object type should be PROG' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_name
      exp = 'ZTEST_PROG'
      msg = 'Object name should match' ).
  ENDMETHOD.

  METHOD test_check_empty_source.
    " Test checking empty source
    DATA lt_source TYPE string_table.

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZEMPTY_PROG'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'Success should be false for empty source' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_result-message
      exp = '*No source*'
      msg = 'Message should indicate no source' ).
  ENDMETHOD.

  METHOD test_set_uccheck.
    " Test setting uccheck mode
    mo_cut->set_uccheck( '5' ). " ABAP Cloud mode

    " Check valid source with Cloud mode
    DATA lt_source TYPE string_table.
    APPEND 'REPORT zcloud_prog.' TO lt_source.
    APPEND 'DATA lv_test TYPE string.' TO lt_source.

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZCLOUD_PROG'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_name
      exp = 'ZCLOUD_PROG'
      msg = 'Object name should match' ).
  ENDMETHOD.

ENDCLASS.
