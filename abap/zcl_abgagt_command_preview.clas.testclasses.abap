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
    METHODS: test_fetch_table_data FOR TESTING.
    METHODS: test_fetch_table_data_with_where FOR TESTING.
    METHODS: test_fetch_table_data_with_limit FOR TESTING.
ENDCLASS.

CLASS ltcl_cmd_preview IMPLEMENTATION.

  METHOD class_setup.
    go_env = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'TADIR' )
        ( 'SCARR' )
        ( 'SFLIGHT' ) ) ).
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
    DATA lo_interface TYPE REF TO zif_abgagt_command.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_preview.
    lo_interface = mo_cut.
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
    lt_tadir = VALUE #( ( object = 'TABL' obj_name = 'ZTEST_TABLE' devclass = '$TEST' ) ).
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
    lt_tadir = VALUE #( ( object = 'DDLS' obj_name = 'ZC_TEST_VIEW' devclass = '$TEST' ) ).
    go_env->insert_test_data( i_data = lt_tadir ).

    DATA(lv_type) = mo_cut->detect_object_type( 'ZC_TEST_VIEW' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = 'DDLS'
      msg = 'Should detect as DDLS' ).
  ENDMETHOD.

  METHOD test_fetch_table_data.
    " Insert test data into SCARR
    DATA lt_scarr TYPE STANDARD TABLE OF scarr.
    lt_scarr = VALUE #(
      ( carrid = 'AA' carrname = 'American Airlines' currcode = 'USD' url = 'http://aa.com' )
      ( carrid = 'LH' carrname = 'Lufthansa' currcode = 'EUR' url = 'http://lh.com' )
      ( carrid = 'BA' carrname = 'British Airways' currcode = 'GBP' url = 'http://ba.com' ) ).
    go_env->insert_test_data( i_data = lt_scarr ).

    " Fetch table data
    DATA(ls_result) = mo_cut->get_table_data(
      iv_name = 'SCARR'
      iv_type = 'TABL'
      iv_limit = 10
      iv_where = ''
      it_columns = VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_true
      msg = 'Should succeed' ).

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

  METHOD test_fetch_table_data_with_where.
    " Insert test data into SFLIGHT
    DATA lt_sflight TYPE STANDARD TABLE OF sflight.
    lt_sflight = VALUE #(
      ( carrid = 'AA' connid = '0017' fldate = '20240101' price = '422.94' currency = 'USD' )
      ( carrid = 'AA' connid = '0017' fldate = '20240102' price = '422.94' currency = 'USD' )
      ( carrid = 'LH' connid = '0400' fldate = '20240101' price = '500.00' currency = 'EUR' ) ).
    go_env->insert_test_data( i_data = lt_sflight ).

    " Fetch with WHERE clause
    DATA(ls_result) = mo_cut->get_table_data(
      iv_name = 'SFLIGHT'
      iv_type = 'TABL'
      iv_limit = 10
      iv_where = 'CARRID = ''AA'''
      it_columns = VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_true
      msg = 'Should succeed' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-row_count
      exp = 2
      msg = 'Should return 2 rows for AA' ).
  ENDMETHOD.

  METHOD test_fetch_table_data_with_limit.
    " Insert test data into SCARR
    DATA lt_scarr TYPE STANDARD TABLE OF scarr.
    lt_scarr = VALUE #(
      ( carrid = 'AA' carrname = 'American Airlines' currcode = 'USD' )
      ( carrid = 'LH' carrname = 'Lufthansa' currcode = 'EUR' )
      ( carrid = 'BA' carrname = 'British Airways' currcode = 'GBP' )
      ( carrid = 'DL' carrname = 'Delta' currcode = 'USD' )
      ( carrid = 'AF' carrname = 'Air France' currcode = 'EUR' ) ).
    go_env->insert_test_data( i_data = lt_scarr ).

    " Fetch with limit
    DATA(ls_result) = mo_cut->get_table_data(
      iv_name = 'SCARR'
      iv_type = 'TABL'
      iv_limit = 3
      iv_where = ''
      it_columns = VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_true
      msg = 'Should succeed' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-row_count
      exp = 3
      msg = 'Should return 3 rows (limit)' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-total_rows
      exp = 5
      msg = 'Total rows should be 5' ).
  ENDMETHOD.

ENDCLASS.
