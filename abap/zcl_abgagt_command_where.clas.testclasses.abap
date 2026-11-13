CLASS ltcl_where_types DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA go_env TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS setup.
    METHODS rejects_explicit_types FOR TESTING.
    METHODS rejects_detected_type FOR TESTING.
    METHODS rejects_misleading_prefix FOR TESTING.
    METHODS keeps_supported_priority FOR TESTING.
ENDCLASS.

CLASS ltcl_where_types IMPLEMENTATION.
  METHOD class_setup.
    go_env = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'TADIR' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    go_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    go_env->clear_doubles( ).
  ENDMETHOD.

  METHOD rejects_explicit_types.
    DATA(lo_cut) = NEW zcl_abgagt_command_where( ).
    DATA(lt_types) = VALUE string_table( ( `BDEF` ) ( `SRVD` ) ( `DDLX` ) ( `TABL` ) ( `INVALID` ) ).
    LOOP AT lt_types INTO DATA(lv_type).
      DATA(ls_params) = VALUE zcl_abgagt_command_where=>ty_where_params(
        objects = VALUE #( ( `ZTEST_UNSUPPORTED` ) ) type = lv_type ).
      DATA(lv_json) = lo_cut->zif_abgagt_command~execute( is_param = ls_params ).
      DATA ls_result TYPE zcl_abgagt_command_where=>ty_where_result.
      CLEAR ls_result.
      /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
      cl_abap_unit_assert=>assert_false( ls_result-success ).
      cl_abap_unit_assert=>assert_equals( act = lines( ls_result-objects ) exp = 1 ).
      cl_abap_unit_assert=>assert_equals( act = ls_result-objects[ 1 ]-type exp = lv_type ).
      cl_abap_unit_assert=>assert_true( xsdbool( ls_result-objects[ 1 ]-error CS 'not supported' ) ).
      cl_abap_unit_assert=>assert_equals( act = lines( ls_result-objects[ 1 ]-references ) exp = 0 ).
    ENDLOOP.
  ENDMETHOD.

  METHOD rejects_detected_type.
    DATA lt_tadir TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.
    lt_tadir = VALUE #( ( pgmid = 'R3TR' object = 'SRVD' obj_name = 'ZTEST_RAP_SERVICE' ) ).
    go_env->insert_test_data( i_data = lt_tadir ).
    DATA(lo_cut) = NEW zcl_abgagt_command_where( ).
    DATA(ls_params) = VALUE zcl_abgagt_command_where=>ty_where_params(
      objects = VALUE #( ( `ZTEST_RAP_SERVICE` ) ) ).
    DATA(lv_json) = lo_cut->zif_abgagt_command~execute( is_param = ls_params ).
    DATA ls_result TYPE zcl_abgagt_command_where=>ty_where_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-objects[ 1 ]-type exp = 'SRVD' ).
    cl_abap_unit_assert=>assert_true( xsdbool( ls_result-objects[ 1 ]-error CS 'not supported' ) ).
  ENDMETHOD.

  METHOD rejects_misleading_prefix.
    DATA lt_tadir TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.
    lt_tadir = VALUE #( ( pgmid = 'R3TR' object = 'DDLX' obj_name = 'ZCL_RAP_ONLY' ) ).
    go_env->insert_test_data( i_data = lt_tadir ).
    DATA(lo_cut) = NEW zcl_abgagt_command_where( ).
    DATA(ls_params) = VALUE zcl_abgagt_command_where=>ty_where_params(
      objects = VALUE #( ( `ZCL_RAP_ONLY` ) ) ).
    DATA(lv_json) = lo_cut->zif_abgagt_command~execute( is_param = ls_params ).
    DATA ls_result TYPE zcl_abgagt_command_where=>ty_where_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-objects[ 1 ]-type exp = 'DDLX' ).
    cl_abap_unit_assert=>assert_true( xsdbool( ls_result-objects[ 1 ]-error CS 'not supported' ) ).
  ENDMETHOD.

  METHOD keeps_supported_priority.
    DATA lt_tadir TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.
    lt_tadir = VALUE #( ( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZTEST_SHARED' )
                       ( pgmid = 'R3TR' object = 'BDEF' obj_name = 'ZTEST_SHARED' ) ).
    go_env->insert_test_data( i_data = lt_tadir ).
    DATA(lo_cut) = NEW zcl_abgagt_command_where( ).
    cl_abap_unit_assert=>assert_equals( act = lo_cut->detect_object_type( `ZTEST_SHARED` ) exp = 'PROG' ).
  ENDMETHOD.
ENDCLASS.
