*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_cmd_preview DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_preview.
    CLASS-DATA: go_env TYPE REF TO if_osql_test_environment.

    CLASS-METHODS: class_setup, class_teardown.
    METHODS: setup.
    METHODS: test_get_name FOR TESTING.
    METHODS: test_interface FOR TESTING.
    METHODS: test_exec_no_objects FOR TESTING.
    METHODS: test_detect_object_type_tabl FOR TESTING.
    METHODS: test_detect_object_type_ddls FOR TESTING.
    METHODS: test_fetch_data FOR TESTING.
    METHODS: test_fetch_data_where FOR TESTING.
    METHODS: test_fetch_data_limit FOR TESTING.
ENDCLASS.

CLASS ltcl_cmd_preview IMPLEMENTATION.

  METHOD class_setup.
    DATA lt_dep TYPE if_osql_test_environment=>ty_t_sobjnames.
    DATA lv_dep LIKE LINE OF lt_dep.
    CLEAR lv_dep.
    lv_dep = 'TADIR'.
    APPEND lv_dep TO lt_dep.
    CLEAR lv_dep.
    lv_dep = 'SCARR'.
    APPEND lv_dep TO lt_dep.
    CLEAR lv_dep.
    lv_dep = 'SFLIGHT'.
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
      exp = zif_abgagt_command=>gc_preview
      msg = 'Command name should be PREVIEW' ).
  ENDMETHOD.

  METHOD test_interface.

    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_preview.
    DATA(lo_interface) = mo_cut.
    cl_abap_unit_assert=>assert_bound(
      act = lo_interface
      msg = 'Object should implement zif_abgagt_command interface' ).
  ENDMETHOD.

  METHOD test_exec_no_objects.
    " Test execute with no objects - should return error
    DATA: BEGIN OF ls_param,
            objects TYPE string_table,
          END OF ls_param.

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
      exp = '*Objects*required*'
      msg = 'Error message should mention objects required' ).
  ENDMETHOD.

  METHOD test_detect_object_type_tabl.
    " Insert TADIR entry for table
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    DATA ls_tadir LIKE LINE OF lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-object   = 'TABL'.
    ls_tadir-obj_name = 'ZTEST_TABLE'.
    ls_tadir-devclass = '$TEST'.
    APPEND ls_tadir TO lt_tadir.
    go_env->insert_test_data( i_data = lt_tadir ).

    DATA(lv_type) = mo_cut->detect_object_type( 'ZTEST_TABLE' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = 'TABL'
      msg = 'Should detect as TABL' ).
  ENDMETHOD.

  METHOD test_detect_object_type_ddls.
    " Insert TADIR entry for CDS view
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    DATA ls_tadir LIKE LINE OF lt_tadir.

    CLEAR ls_tadir.
    ls_tadir-object   = 'DDLS'.
    ls_tadir-obj_name = 'ZC_TEST_VIEW'.
    ls_tadir-devclass = '$TEST'.
    APPEND ls_tadir TO lt_tadir.
    go_env->insert_test_data( i_data = lt_tadir ).

    DATA(lv_type) = mo_cut->detect_object_type( 'ZC_TEST_VIEW' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = 'DDLS'
      msg = 'Should detect as DDLS' ).
  ENDMETHOD.

  METHOD test_fetch_data.
    " Insert test data into SCARR
    DATA lt_scarr TYPE STANDARD TABLE OF scarr.
    DATA ls_scarr LIKE LINE OF lt_scarr.

    CLEAR ls_scarr.
    ls_scarr-carrid   = 'AA'.
    ls_scarr-carrname = 'American Airlines'.
    ls_scarr-currcode = 'USD'.
    ls_scarr-url      = 'http://aa.com'.
    APPEND ls_scarr TO lt_scarr.

    CLEAR ls_scarr.
    ls_scarr-carrid   = 'LH'.
    ls_scarr-carrname = 'Lufthansa'.
    ls_scarr-currcode = 'EUR'.
    ls_scarr-url      = 'http://lh.com'.
    APPEND ls_scarr TO lt_scarr.

    CLEAR ls_scarr.
    ls_scarr-carrid   = 'BA'.
    ls_scarr-carrname = 'British Airways'.
    ls_scarr-currcode = 'GBP'.
    ls_scarr-url      = 'http://ba.com'.
    APPEND ls_scarr TO lt_scarr.
    go_env->insert_test_data( i_data = lt_scarr ).

    " Fetch table data
    DATA lt_columns TYPE string_table.
    CLEAR lt_columns.
    DATA(ls_result) = mo_cut->get_table_data(
      iv_name = 'SCARR'
      iv_type = 'TABL'
      iv_limit = 10
      iv_offset = 0
      iv_where = ''
      it_columns = lt_columns ).

    cl_abap_unit_assert=>assert_initial(
      act = ls_result-error
      msg = 'Should have no error' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-row_count
      exp = 3
      msg = 'Should return 3 rows' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-total_rows
      exp = 3
      msg = 'Total rows should be 3' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = ls_result-rows
      exp = '*AA*'
      msg = 'Rows should contain AA' ).
  ENDMETHOD.

  METHOD test_fetch_data_where.
    " Insert test data into SFLIGHT
    DATA lt_sflight TYPE STANDARD TABLE OF sflight.
    DATA ls_sflight LIKE LINE OF lt_sflight.

    CLEAR ls_sflight.
    ls_sflight-carrid   = 'AA'.
    ls_sflight-connid   = '0017'.
    ls_sflight-fldate   = '20240101'.
    ls_sflight-price    = '422.94'.
    ls_sflight-currency = 'USD'.
    APPEND ls_sflight TO lt_sflight.

    CLEAR ls_sflight.
    ls_sflight-carrid   = 'AA'.
    ls_sflight-connid   = '0017'.
    ls_sflight-fldate   = '20240102'.
    ls_sflight-price    = '422.94'.
    ls_sflight-currency = 'USD'.
    APPEND ls_sflight TO lt_sflight.

    CLEAR ls_sflight.
    ls_sflight-carrid   = 'LH'.
    ls_sflight-connid   = '0400'.
    ls_sflight-fldate   = '20240101'.
    ls_sflight-price    = '500.00'.
    ls_sflight-currency = 'EUR'.
    APPEND ls_sflight TO lt_sflight.
    go_env->insert_test_data( i_data = lt_sflight ).

    " Fetch with WHERE clause
    DATA lt_columns TYPE string_table.
    CLEAR lt_columns.
    DATA(ls_result) = mo_cut->get_table_data(
      iv_name = 'SFLIGHT'
      iv_type = 'TABL'
      iv_limit = 10
      iv_offset = 0
      iv_where = 'CARRID = ''AA'''
      it_columns = lt_columns ).

    cl_abap_unit_assert=>assert_initial(
      act = ls_result-error
      msg = 'Should have no error' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-row_count
      exp = 2
      msg = 'Should return 2 rows for AA' ).
  ENDMETHOD.

  METHOD test_fetch_data_limit.
    " Insert test data into SCARR
    DATA lt_scarr TYPE STANDARD TABLE OF scarr.
    DATA ls_scarr LIKE LINE OF lt_scarr.

    CLEAR ls_scarr.
    ls_scarr-carrid   = 'AA'.
    ls_scarr-carrname = 'American Airlines'.
    ls_scarr-currcode = 'USD'.
    APPEND ls_scarr TO lt_scarr.

    CLEAR ls_scarr.
    ls_scarr-carrid   = 'LH'.
    ls_scarr-carrname = 'Lufthansa'.
    ls_scarr-currcode = 'EUR'.
    APPEND ls_scarr TO lt_scarr.

    CLEAR ls_scarr.
    ls_scarr-carrid   = 'BA'.
    ls_scarr-carrname = 'British Airways'.
    ls_scarr-currcode = 'GBP'.
    APPEND ls_scarr TO lt_scarr.

    CLEAR ls_scarr.
    ls_scarr-carrid   = 'DL'.
    ls_scarr-carrname = 'Delta'.
    ls_scarr-currcode = 'USD'.
    APPEND ls_scarr TO lt_scarr.

    CLEAR ls_scarr.
    ls_scarr-carrid   = 'AF'.
    ls_scarr-carrname = 'Air France'.
    ls_scarr-currcode = 'EUR'.
    APPEND ls_scarr TO lt_scarr.
    go_env->insert_test_data( i_data = lt_scarr ).

    " Fetch with limit
    DATA lt_columns TYPE string_table.
    CLEAR lt_columns.
    DATA(ls_result) = mo_cut->get_table_data(
      iv_name = 'SCARR'
      iv_type = 'TABL'
      iv_limit = 3
      iv_offset = 0
      iv_where = ''
      it_columns = lt_columns ).

    cl_abap_unit_assert=>assert_initial(
      act = ls_result-error
      msg = 'Should have no error' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-row_count
      exp = 3
      msg = 'Should return 3 rows (limit)' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-total_rows
      exp = 5
      msg = 'Total rows should equal actual table row count (5)' ).
  ENDMETHOD.

ENDCLASS.
