*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_cmd_customize DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_customize.
    CLASS-DATA go_env TYPE REF TO if_osql_test_environment.

    CLASS-METHODS: class_setup, class_teardown.
    METHODS: setup.

    METHODS: test_get_name              FOR TESTING.
    METHODS: test_missing_table_name    FOR TESTING.
    METHODS: test_missing_field_values  FOR TESTING.
    METHODS: test_table_not_found       FOR TESTING.
    METHODS: test_non_customizing_table FOR TESTING.
    METHODS: test_delivery_class_e_ok   FOR TESTING.
    METHODS: test_transport_not_found   FOR TESTING.
    METHODS: test_transport_released    FOR TESTING.
    METHODS: test_workbench_transport   FOR TESTING.
    METHODS: test_table_name_uppercased FOR TESTING.
ENDCLASS.

CLASS ltcl_cmd_customize IMPLEMENTATION.

  METHOD class_setup.
    go_env = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'DD02L' )
        ( 'TADIR' )
        ( 'TDEVC' )
        ( 'E070' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    go_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    go_env->clear_doubles( ).
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD test_get_name.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->zif_abgagt_command~get_name( )
      exp = zif_abgagt_command=>gc_customize
      msg = 'Command name should be CUSTOMIZE' ).
  ENDMETHOD.

  METHOD test_missing_table_name.
    DATA ls_params TYPE zcl_abgagt_command_customize=>ty_customize_params.
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_params ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Should fail when table_name is missing' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*table_name*required*'
      msg = 'Error should mention table_name required' ).
  ENDMETHOD.

  METHOD test_missing_field_values.
    DATA lt_dd02l TYPE STANDARD TABLE OF dd02l.
    lt_dd02l = VALUE #( ( tabname = 'ZTABLE' as4local = 'A' contflag = 'C' ) ).
    go_env->insert_test_data( i_data = lt_dd02l ).

    DATA ls_params TYPE zcl_abgagt_command_customize=>ty_customize_params.
    ls_params-table_name = 'ZTABLE'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_params ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Should fail when field_values is empty' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*field_values*required*'
      msg = 'Error should mention field_values required' ).
  ENDMETHOD.

  METHOD test_table_not_found.
    " DD02L returns nothing → table not found
    DATA ls_params TYPE zcl_abgagt_command_customize=>ty_customize_params.
    ls_params-table_name   = 'ZZNOEXIST99'.
    ls_params-field_values = VALUE #( ( field = 'KEY' value = 'X' ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_params ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Should fail for non-existent table' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*not found*'
      msg = 'Error should mention not found' ).
  ENDMETHOD.

  METHOD test_non_customizing_table.
    " DD02L returns delivery class S (not C or E)
    DATA lt_dd02l TYPE STANDARD TABLE OF dd02l.
    lt_dd02l = VALUE #( ( tabname = 'TADIR' as4local = 'A' contflag = 'S' ) ).
    go_env->insert_test_data( i_data = lt_dd02l ).

    DATA ls_params TYPE zcl_abgagt_command_customize=>ty_customize_params.
    ls_params-table_name   = 'TADIR'.
    ls_params-field_values = VALUE #( ( field = 'PGMID' value = 'R3TR' ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_params ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Should reject non-customizing table' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*not a customizing table*'
      msg = 'Error should mention not a customizing table' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*delivery class*S*'
      msg = 'Error should include the actual delivery class' ).
  ENDMETHOD.

  METHOD test_delivery_class_e_ok.
    " Delivery class E is allowed — must pass the delivery-class check
    DATA lt_dd02l TYPE STANDARD TABLE OF dd02l.
    lt_dd02l = VALUE #( ( tabname = 'ZTABLE_E' as4local = 'A' contflag = 'E' ) ).
    go_env->insert_test_data( i_data = lt_dd02l ).

    " TADIR empty → transport not required; use --no-transport to skip transport step
    DATA ls_params TYPE zcl_abgagt_command_customize=>ty_customize_params.
    ls_params-table_name   = 'ZTABLE_E'.
    ls_params-field_values = VALUE #( ( field = 'KEY' value = 'X' ) ).
    ls_params-no_transport = abap_true.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_params ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial for delivery class E' ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_result
      exp = '*not a customizing table*'
      msg = 'Delivery class E should be accepted' ).
  ENDMETHOD.

  METHOD test_transport_not_found.
    " Table is valid customizing; transport explicitly provided but not in E070
    DATA lt_dd02l TYPE STANDARD TABLE OF dd02l.
    lt_dd02l = VALUE #( ( tabname = 'ZTABLE' as4local = 'A' contflag = 'C' ) ).
    go_env->insert_test_data( i_data = lt_dd02l ).

    DATA ls_params TYPE zcl_abgagt_command_customize=>ty_customize_params.
    ls_params-table_name   = 'ZTABLE'.
    ls_params-field_values = VALUE #( ( field = 'KEY' value = 'X' ) ).
    ls_params-transport    = 'DEVK999999'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_params ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Should fail when transport not found in E070' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*not found*'
      msg = 'Error should mention transport not found' ).
  ENDMETHOD.

  METHOD test_transport_released.
    DATA lt_dd02l TYPE STANDARD TABLE OF dd02l.
    lt_dd02l = VALUE #( ( tabname = 'ZTABLE' as4local = 'A' contflag = 'C' ) ).
    go_env->insert_test_data( i_data = lt_dd02l ).

    DATA lt_e070 TYPE STANDARD TABLE OF e070.
    lt_e070 = VALUE #( ( trkorr = 'DEVK900010' trstatus = 'R' trfunction = 'W' ) ).
    go_env->insert_test_data( i_data = lt_e070 ).

    DATA ls_params TYPE zcl_abgagt_command_customize=>ty_customize_params.
    ls_params-table_name   = 'ZTABLE'.
    ls_params-field_values = VALUE #( ( field = 'KEY' value = 'X' ) ).
    ls_params-transport    = 'DEVK900010'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_params ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Should fail when transport is already released' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*already released*'
      msg = 'Error should mention already released' ).
  ENDMETHOD.

  METHOD test_workbench_transport.
    DATA lt_dd02l TYPE STANDARD TABLE OF dd02l.
    lt_dd02l = VALUE #( ( tabname = 'ZTABLE' as4local = 'A' contflag = 'C' ) ).
    go_env->insert_test_data( i_data = lt_dd02l ).

    " Transport modifiable (status D) but workbench type (K)
    DATA lt_e070 TYPE STANDARD TABLE OF e070.
    lt_e070 = VALUE #( ( trkorr = 'DEVK900020' trstatus = 'D' trfunction = 'K' ) ).
    go_env->insert_test_data( i_data = lt_e070 ).

    DATA ls_params TYPE zcl_abgagt_command_customize=>ty_customize_params.
    ls_params-table_name   = 'ZTABLE'.
    ls_params-field_values = VALUE #( ( field = 'KEY' value = 'X' ) ).
    ls_params-transport    = 'DEVK900020'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_params ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Should reject workbench transport for customizing table' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*workbench request*'
      msg = 'Error should mention workbench request' ).
  ENDMETHOD.

  METHOD test_table_name_uppercased.
    " Pass lowercase table name — should be uppercased internally
    DATA lt_dd02l TYPE STANDARD TABLE OF dd02l.
    lt_dd02l = VALUE #( ( tabname = 'ZTABLE' as4local = 'A' contflag = 'S' ) ).
    go_env->insert_test_data( i_data = lt_dd02l ).

    DATA ls_params TYPE zcl_abgagt_command_customize=>ty_customize_params.
    ls_params-table_name   = 'ztable'.
    ls_params-field_values = VALUE #( ( field = 'KEY' value = 'X' ) ).

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_params ).

    " Should find the table (uppercased to ZTABLE) and fail on delivery class S
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*not a customizing table*'
      msg = 'Table name should be uppercased and looked up correctly' ).
  ENDMETHOD.

ENDCLASS.
