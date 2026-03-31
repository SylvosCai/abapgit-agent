*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_command_list DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_list.
    CLASS-DATA: go_env TYPE REF TO if_osql_test_environment.

    CLASS-METHODS: class_setup, class_teardown.
    METHODS: setup.
    METHODS: test_get_name FOR TESTING.
    METHODS: test_interface FOR TESTING.
    METHODS: test_validate_type FOR TESTING.
    METHODS: test_exec_no_package FOR TESTING.
    METHODS: test_exec_invalid_package FOR TESTING.
    METHODS: test_exec_valid_package FOR TESTING.
    METHODS: test_exec_with_type_filter FOR TESTING.
    METHODS: test_exec_with_limit_offset FOR TESTING.
ENDCLASS.

CLASS ltcl_zcl_abgagt_command_list IMPLEMENTATION.

  METHOD class_setup.
    DATA lt_dep TYPE if_osql_test_environment=>ty_t_sobjnames.
    DATA lv_dep LIKE LINE OF lt_dep.
    CLEAR lv_dep.
    lv_dep = 'TDEVC'.
    APPEND lv_dep TO lt_dep.
    CLEAR lv_dep.
    lv_dep = 'TADIR'.
    APPEND lv_dep TO lt_dep.
    go_env = cl_osql_test_environment=>create(
      i_dependency_list = lt_dep ).
  ENDMETHOD.

  METHOD class_teardown.
    go_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    go_env->clear_doubles( ).
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_name.
    DATA(lv_name) = mo_cut->zif_abgagt_command~get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = zif_abgagt_command=>gc_list
      msg = 'Command name should be LIST' ).
  ENDMETHOD.

  METHOD test_interface.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_list.
    DATA(lo_interface) = mo_cut.
    cl_abap_unit_assert=>assert_bound(
      act = lo_interface
      msg = 'Object should implement zif_abgagt_command interface' ).
  ENDMETHOD.

  METHOD test_validate_type.
    DATA(lv_valid) = mo_cut->validate_type( 'CLAS' ).
    cl_abap_unit_assert=>assert_true(
      act = lv_valid
      msg = 'validate_type should return true' ).

    lv_valid = mo_cut->validate_type( 'INTF' ).
    cl_abap_unit_assert=>assert_true(
      act = lv_valid
      msg = 'validate_type should return true for INTF' ).
  ENDMETHOD.

  METHOD test_exec_no_package.
    " Test execute with no package - should return error
    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
          END OF ls_param.

    ls_param-package = ''.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Result should indicate failure' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*Package*required*'
      msg = 'Error message should mention package required' ).
  ENDMETHOD.

  METHOD test_exec_invalid_package.
    " Clear any existing data to ensure package doesn't exist
    go_env->clear_doubles( ).

    " Test execute with non-existent package
    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
          END OF ls_param.

    ls_param-package = '$INVALID_PACKAGE'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Result should indicate failure' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*does not exist*'
      msg = 'Error message should mention package does not exist' ).
  ENDMETHOD.

  METHOD test_exec_valid_package.
    " Insert test data into TDEVC - valid package
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    DATA ls_tdevc LIKE LINE OF lt_tdevc.

    CLEAR ls_tdevc.
    ls_tdevc-devclass = '$ZTEST_PACK'.
    ls_tdevc-parentcl = '$SAP'.
    ls_tdevc-ctext    = 'Test Package'.
    APPEND ls_tdevc TO lt_tdevc.
    go_env->insert_test_data( i_data = lt_tdevc ).

    " Insert test data into TADIR - sample objects
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    DATA ls_tadir LIKE LINE OF lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_TEST_CLASS'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_ANOTHER'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'INTF'.
    ls_tadir-obj_name = 'ZIF_TEST'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'PROG'.
    ls_tadir-obj_name = 'ZTEST_PROG'.
    APPEND ls_tadir TO lt_tadir.
    go_env->insert_test_data( i_data = lt_tadir ).

    " Execute with valid package
    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
            limit TYPE i,
          END OF ls_param.

    ls_param-package = '$ZTEST_PACK'.
    ls_param-limit = 10.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZCL_TEST_CLASS*'
      msg = 'Result should contain test class' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*total*4*'
      msg = 'Result should show total count' ).
  ENDMETHOD.

  METHOD test_exec_with_type_filter.
    " Insert test data into TDEVC
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    DATA ls_tdevc LIKE LINE OF lt_tdevc.

    CLEAR ls_tdevc.
    ls_tdevc-devclass = '$ZTEST_PACK'.
    ls_tdevc-parentcl = '$SAP'.
    ls_tdevc-ctext    = 'Test Package'.
    APPEND ls_tdevc TO lt_tdevc.
    go_env->insert_test_data( i_data = lt_tdevc ).

    " Insert test data into TADIR
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    DATA ls_tadir LIKE LINE OF lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_TEST_CLASS'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'INTF'.
    ls_tadir-obj_name = 'ZIF_TEST'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'PROG'.
    ls_tadir-obj_name = 'ZTEST_PROG'.
    APPEND ls_tadir TO lt_tadir.
    go_env->insert_test_data( i_data = lt_tadir ).

    " Execute with type filter
    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
            type TYPE string,
          END OF ls_param.

    ls_param-package = '$ZTEST_PACK'.
    ls_param-type = 'CLAS'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*ZCL_TEST_CLASS*'
      msg = 'Result should contain CLAS object' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be empty' ).
  ENDMETHOD.

  METHOD test_exec_with_limit_offset.
    " Insert test data into TDEVC
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    DATA ls_tdevc LIKE LINE OF lt_tdevc.

    CLEAR ls_tdevc.
    ls_tdevc-devclass = '$ZTEST_PACK'.
    ls_tdevc-parentcl = '$SAP'.
    ls_tdevc-ctext    = 'Test Package'.
    APPEND ls_tdevc TO lt_tdevc.
    go_env->insert_test_data( i_data = lt_tdevc ).

    " Insert more test data into TADIR (10 objects)
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    DATA ls_tadir LIKE LINE OF lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_01'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_02'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_03'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_04'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_05'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_06'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_07'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_08'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_09'.
    APPEND ls_tadir TO lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-devclass = '$ZTEST_PACK'.
    ls_tadir-object   = 'CLAS'.
    ls_tadir-obj_name = 'ZCL_CLASS_10'.
    APPEND ls_tadir TO lt_tadir.
    go_env->insert_test_data( i_data = lt_tadir ).

    " Execute with limit and offset
    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
            limit TYPE i,
            offset TYPE i,
          END OF ls_param.

    ls_param-package = '$ZTEST_PACK'.
    ls_param-limit = 3.
    ls_param-offset = 2.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*limit*3*'
      msg = 'Result should reflect limit' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*offset*2*'
      msg = 'Result should reflect offset' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*total*10*'
      msg = 'Total should still be 10' ).
  ENDMETHOD.

ENDCLASS.
