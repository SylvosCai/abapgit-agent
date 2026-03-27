*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_cts_api_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_cts_api.
    DATA mv_subrc    TYPE i      VALUE 0.
    DATA mv_trkorr   TYPE trkorr VALUE 'DEVK999999'.
    DATA mv_category TYPE char01.
ENDCLASS.

CLASS ltcl_cts_api_double IMPLEMENTATION.
  METHOD zif_abgagt_cts_api~create_transport.
    mv_category      = iv_category.
    rs_result-subrc  = mv_subrc.
    rs_result-trkorr = mv_trkorr.
  ENDMETHOD.
  METHOD zif_abgagt_cts_api~check_transport.
    rv_subrc = mv_subrc.
  ENDMETHOD.
  METHOD zif_abgagt_cts_api~release_transport.
    rv_subrc = mv_subrc.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_cmd_transport DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_transport.
    CLASS-DATA: go_env TYPE REF TO if_osql_test_environment.

    CLASS-METHODS: class_setup, class_teardown.
    METHODS: setup.
    METHODS: test_get_name FOR TESTING.
    METHODS: test_interface FOR TESTING.
    METHODS: test_exec_unknown_action FOR TESTING.
    METHODS: test_list_scope_default FOR TESTING.
    METHODS: test_list_scope_mine FOR TESTING.
    METHODS: test_list_scope_all FOR TESTING.
    METHODS: test_list_scope_tasks FOR TESTING.
    METHODS: test_list_unknown_scope FOR TESTING.
    METHODS: test_check_no_number FOR TESTING.
    METHODS: test_release_no_number FOR TESTING.
    METHODS: test_format_date FOR TESTING.
    METHODS: test_create_success FOR TESTING.
    METHODS: test_create_failure FOR TESTING.
    METHODS: test_check_passes FOR TESTING.
    METHODS: test_check_fails FOR TESTING.
    METHODS: test_release_success FOR TESTING.
    METHODS: test_release_failure FOR TESTING.
    METHODS: test_create_workbench_type FOR TESTING.
    METHODS: test_create_customizing_type FOR TESTING.
ENDCLASS.

CLASS ltcl_cmd_transport IMPLEMENTATION.

  METHOD class_setup.
    go_env = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'E070' )
        ( 'E07T' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    go_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    go_env->clear_doubles( ).
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD test_get_name.
    DATA(lv_name) = mo_cut->zif_abgagt_command~get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = zif_abgagt_command=>gc_transport
      msg = 'Command name should be TRANSPORT' ).
  ENDMETHOD.

  METHOD test_interface.

    mo_cut = NEW zcl_abgagt_command_transport( ).
    DATA(lo_interface) = mo_cut.
    cl_abap_unit_assert=>assert_bound(
      act = lo_interface
      msg = 'Object should implement zif_abgagt_command interface' ).
  ENDMETHOD.

  METHOD test_exec_unknown_action.
    DATA: BEGIN OF ls_param,
            action TYPE string,
          END OF ls_param.

    ls_param-action = 'INVALID'.

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
      exp = '*Unknown action*'
      msg = 'Error should mention unknown action' ).
  ENDMETHOD.

  METHOD test_list_scope_default.
    " When no action supplied, default action should be empty and fall to OTHERS
    DATA: BEGIN OF ls_param,
            action TYPE string,
            scope  TYPE string,
          END OF ls_param.

    ls_param-action = 'LIST'.
    " scope left empty — list_transports defaults to 'mine'

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
      exp = '*scope*mine*'
      msg = 'Default scope should be mine' ).
  ENDMETHOD.

  METHOD test_list_scope_mine.
    " Insert test data into E070
    DATA lt_e070 TYPE STANDARD TABLE OF e070.
    lt_e070 = VALUE #(
      ( trkorr    = 'DEVK900001'
        trstatus  = 'D'
        trfunction = 'K'
        as4user   = sy-uname
        as4date   = '20240101' ) ).
    go_env->insert_test_data( i_data = lt_e070 ).

    " Insert matching text in E07T
    DATA lt_e07t TYPE STANDARD TABLE OF e07t.
    lt_e07t = VALUE #(
      ( trkorr  = 'DEVK900001'
        langu   = sy-langu
        as4text = 'My Test Transport' ) ).
    go_env->insert_test_data( i_data = lt_e07t ).

    DATA: BEGIN OF ls_param,
            action TYPE string,
            scope  TYPE string,
          END OF ls_param.

    ls_param-action = 'LIST'.
    ls_param-scope  = 'mine'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*DEVK900001*'
      msg = 'Result should contain transport number' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*My Test Transport*'
      msg = 'Result should contain transport description' ).
  ENDMETHOD.

  METHOD test_list_scope_all.
    " Insert test data for two different users
    DATA lt_e070 TYPE STANDARD TABLE OF e070.
    lt_e070 = VALUE #(
      ( trkorr     = 'DEVK900002'
        trstatus   = 'D'
        trfunction = 'K'
        as4user    = 'OTHER_USER'
        as4date    = '20240115' ) ).
    go_env->insert_test_data( i_data = lt_e070 ).

    DATA lt_e07t TYPE STANDARD TABLE OF e07t.
    lt_e07t = VALUE #(
      ( trkorr  = 'DEVK900002'
        langu   = sy-langu
        as4text = 'Other User Transport' ) ).
    go_env->insert_test_data( i_data = lt_e07t ).

    DATA: BEGIN OF ls_param,
            action TYPE string,
            scope  TYPE string,
          END OF ls_param.

    ls_param-action = 'LIST'.
    ls_param-scope  = 'all'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*DEVK900002*'
      msg = 'Result should contain all transports including other users' ).
  ENDMETHOD.

  METHOD test_list_scope_tasks.
    " Insert a task owned by current user whose parent is DEVK900003
    DATA lt_e070 TYPE STANDARD TABLE OF e070.
    lt_e070 = VALUE #(
      ( trkorr     = 'DEVK900003'
        trstatus   = 'D'
        trfunction = 'K'
        as4user    = 'ANY_OWNER'
        as4date    = '20240201'
        strkorr    = '' )
      ( trkorr     = 'DEVK900004'
        trstatus   = 'D'
        trfunction = 'T'
        as4user    = sy-uname
        as4date    = '20240201'
        strkorr    = 'DEVK900003' ) ).
    go_env->insert_test_data( i_data = lt_e070 ).

    DATA lt_e07t TYPE STANDARD TABLE OF e07t.
    lt_e07t = VALUE #(
      ( trkorr  = 'DEVK900003'
        langu   = sy-langu
        as4text = 'Parent Transport' ) ).
    go_env->insert_test_data( i_data = lt_e07t ).

    DATA: BEGIN OF ls_param,
            action TYPE string,
            scope  TYPE string,
          END OF ls_param.

    ls_param-action = 'LIST'.
    ls_param-scope  = 'task'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*DEVK900003*'
      msg = 'Result should contain parent transport of user task' ).
  ENDMETHOD.

  METHOD test_list_unknown_scope.
    DATA: BEGIN OF ls_param,
            action TYPE string,
            scope  TYPE string,
          END OF ls_param.

    ls_param-action = 'LIST'.
    ls_param-scope  = 'invalid_scope'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Result should indicate failure for unknown scope' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*Unknown scope*'
      msg = 'Error should mention unknown scope' ).
  ENDMETHOD.

  METHOD test_check_no_number.
    DATA: BEGIN OF ls_param,
            action TYPE string,
            number TYPE trkorr,
          END OF ls_param.

    ls_param-action = 'CHECK'.
    " number left empty

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Result should indicate failure when number is missing' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*required*'
      msg = 'Error should mention number is required' ).
  ENDMETHOD.

  METHOD test_release_no_number.
    DATA: BEGIN OF ls_param,
            action TYPE string,
            number TYPE trkorr,
          END OF ls_param.

    ls_param-action = 'RELEASE'.
    " number left empty

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Result should indicate failure when number is missing' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*required*'
      msg = 'Error should mention number is required' ).
  ENDMETHOD.

  METHOD test_format_date.
    " Test format_date indirectly via list result — date should be formatted
    DATA lt_e070 TYPE STANDARD TABLE OF e070.
    lt_e070 = VALUE #(
      ( trkorr     = 'DEVK900005'
        trstatus   = 'D'
        trfunction = 'K'
        as4user    = sy-uname
        as4date    = '20240315' ) ).
    go_env->insert_test_data( i_data = lt_e070 ).

    DATA lt_e07t TYPE STANDARD TABLE OF e07t.
    lt_e07t = VALUE #(
      ( trkorr  = 'DEVK900005'
        langu   = sy-langu
        as4text = 'Date Format Test' ) ).
    go_env->insert_test_data( i_data = lt_e07t ).

    DATA: BEGIN OF ls_param,
            action TYPE string,
            scope  TYPE string,
          END OF ls_param.

    ls_param-action = 'LIST'.
    ls_param-scope  = 'mine'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*2024-03-15*'
      msg = 'Date should be formatted as YYYY-MM-DD' ).
  ENDMETHOD.

  METHOD test_create_success.
    DATA(lo_double) = NEW ltcl_cts_api_double( ).
    lo_double->mv_subrc  = 0.
    lo_double->mv_trkorr = 'DEVK999999'.
    DATA(lo_cut) = NEW zcl_abgagt_command_transport( io_cts_api = lo_double ).

    DATA: BEGIN OF ls_param,
            action      TYPE string,
            description TYPE string,
            type        TYPE string,
          END OF ls_param.
    ls_param-action      = 'CREATE'.
    ls_param-description = 'Test transport'.
    ls_param-type        = 'workbench'.

    DATA(lv_result) = lo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Create should succeed when CTS API returns subrc 0' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*DEVK999999*'
      msg = 'Result should contain the new transport number' ).
  ENDMETHOD.

  METHOD test_create_failure.
    DATA(lo_double) = NEW ltcl_cts_api_double( ).
    lo_double->mv_subrc = 1.
    DATA(lo_cut) = NEW zcl_abgagt_command_transport( io_cts_api = lo_double ).

    DATA: BEGIN OF ls_param,
            action      TYPE string,
            description TYPE string,
            type        TYPE string,
          END OF ls_param.
    ls_param-action      = 'CREATE'.
    ls_param-description = 'Test transport'.
    ls_param-type        = 'workbench'.

    DATA(lv_result) = lo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Create should fail when CTS API returns non-zero subrc' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*Failed*'
      msg = 'Error message should mention failure' ).
  ENDMETHOD.

  METHOD test_check_passes.
    DATA(lo_double) = NEW ltcl_cts_api_double( ).
    lo_double->mv_subrc = 0.
    DATA(lo_cut) = NEW zcl_abgagt_command_transport( io_cts_api = lo_double ).

    DATA: BEGIN OF ls_param,
            action TYPE string,
            number TYPE trkorr,
          END OF ls_param.
    ls_param-action = 'CHECK'.
    ls_param-number = 'DEVK999999'.

    DATA(lv_result) = lo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Check action should succeed' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*passed*true*'
      msg = 'Transport should have passed check when subrc is 0' ).
  ENDMETHOD.

  METHOD test_check_fails.
    DATA(lo_double) = NEW ltcl_cts_api_double( ).
    lo_double->mv_subrc = 4.
    DATA(lo_cut) = NEW zcl_abgagt_command_transport( io_cts_api = lo_double ).

    DATA: BEGIN OF ls_param,
            action TYPE string,
            number TYPE trkorr,
          END OF ls_param.
    ls_param-action = 'CHECK'.
    ls_param-number = 'DEVK999999'.

    DATA(lv_result) = lo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Check action result should still be success (not a fatal error)' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*passed*false*'
      msg = 'Transport should not have passed check when subrc is non-zero' ).
  ENDMETHOD.

  METHOD test_release_success.
    DATA(lo_double) = NEW ltcl_cts_api_double( ).
    lo_double->mv_subrc = 0.
    DATA(lo_cut) = NEW zcl_abgagt_command_transport( io_cts_api = lo_double ).

    DATA: BEGIN OF ls_param,
            action TYPE string,
            number TYPE trkorr,
          END OF ls_param.
    ls_param-action = 'RELEASE'.
    ls_param-number = 'DEVK999999'.

    DATA(lv_result) = lo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Release should succeed when CTS API returns subrc 0' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*released*'
      msg = 'Message should mention released' ).
  ENDMETHOD.

  METHOD test_release_failure.
    DATA(lo_double) = NEW ltcl_cts_api_double( ).
    lo_double->mv_subrc = 1.
    DATA(lo_cut) = NEW zcl_abgagt_command_transport( io_cts_api = lo_double ).

    DATA: BEGIN OF ls_param,
            action TYPE string,
            number TYPE trkorr,
          END OF ls_param.
    ls_param-action = 'RELEASE'.
    ls_param-number = 'DEVK999999'.

    DATA(lv_result) = lo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Release should fail when CTS API returns non-zero subrc' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*Failed*'
      msg = 'Error message should mention failure' ).
  ENDMETHOD.

  METHOD test_create_workbench_type.
    DATA(lo_double) = NEW ltcl_cts_api_double( ).
    DATA(lo_cut) = NEW zcl_abgagt_command_transport( io_cts_api = lo_double ).

    DATA: BEGIN OF ls_param,
            action      TYPE string,
            description TYPE string,
            type        TYPE string,
          END OF ls_param.
    ls_param-action      = 'CREATE'.
    ls_param-description = 'WB transport'.
    ls_param-type        = 'workbench'.

    lo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_double->mv_category
      exp = 'K'
      msg = 'Workbench type should pass category K to CTS API' ).
  ENDMETHOD.

  METHOD test_create_customizing_type.
    DATA(lo_double) = NEW ltcl_cts_api_double( ).
    DATA(lo_cut) = NEW zcl_abgagt_command_transport( io_cts_api = lo_double ).

    DATA: BEGIN OF ls_param,
            action      TYPE string,
            description TYPE string,
            type        TYPE string,
          END OF ls_param.
    ls_param-action      = 'CREATE'.
    ls_param-description = 'Cust transport'.
    ls_param-type        = 'customizing'.

    lo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_double->mv_category
      exp = 'W'
      msg = 'Customizing type should pass category W to CTS API' ).
  ENDMETHOD.

ENDCLASS.
