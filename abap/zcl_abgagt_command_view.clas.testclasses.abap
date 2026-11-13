CLASS ltcl_view_errors DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_env TYPE REF TO if_osql_test_environment.
    METHODS setup.
    METHODS teardown.
    METHODS native_failure_not_absence FOR TESTING.
    METHODS missing_is_not_failure FOR TESTING.
    METHODS unsupported_viewer_fails FOR TESTING.
ENDCLASS.

CLASS ltcl_view_errors IMPLEMENTATION.
  METHOD setup.
    mo_env = cl_osql_test_environment=>create( VALUE #( ( 'TADIR' ) ) ).
  ENDMETHOD.
  METHOD teardown.
    mo_env->destroy( ).
  ENDMETHOD.
  METHOD native_failure_not_absence.
    DATA lt_rows TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.
    lt_rows = VALUE #( ( pgmid = 'R3TR' object = 'BDEF' obj_name = 'ZABGAGT_VIEW_MISSING' devclass = '$ABAP_AI_BRIDGE' masterlang = 'E' )
                      ( pgmid = 'R3TR' object = 'SRVD' obj_name = 'ZABGAGT_VIEW_MISSING' devclass = '$ABAP_AI_BRIDGE' masterlang = 'E' )
                      ( pgmid = 'R3TR' object = 'DDLX' obj_name = 'ZABGAGT_VIEW_MISSING' devclass = '$ABAP_AI_BRIDGE' masterlang = 'E' ) ).
    mo_env->insert_test_data( lt_rows ).
    DATA(lo_command) = NEW zcl_abgagt_command_view( ).
    LOOP AT lt_rows INTO DATA(ls_row).
      DATA(ls_params) = VALUE zcl_abgagt_command_view=>ty_view_params(
        objects = VALUE #( ( CONV #( ls_row-obj_name ) ) ) type = ls_row-object ).
      DATA(lv_json) = lo_command->zif_abgagt_command~execute( is_param = ls_params ).
      DATA ls_result TYPE zcl_abgagt_command_view=>ty_view_result.
      CLEAR ls_result.
      /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
      cl_abap_unit_assert=>assert_false( ls_result-success ).
      cl_abap_unit_assert=>assert_not_initial( ls_result-error ).
      cl_abap_unit_assert=>assert_equals( act = lines( ls_result-objects ) exp = 1 ).
      DATA(ls_info) = ls_result-objects[ 1 ].
      cl_abap_unit_assert=>assert_equals( act = ls_info-type exp = ls_row-object ).
      cl_abap_unit_assert=>assert_equals( act = ls_info-name exp = ls_row-obj_name ).
      cl_abap_unit_assert=>assert_false( ls_info-not_found ).
      cl_abap_unit_assert=>assert_not_initial( ls_info-error ).
    ENDLOOP.
  ENDMETHOD.
  METHOD missing_is_not_failure.
    DATA(lo_command) = NEW zcl_abgagt_command_view( ).
    DATA(ls_params) = VALUE zcl_abgagt_command_view=>ty_view_params(
      objects = VALUE #( ( `ZABGAGT_VIEW_MISSING` ) ) type = 'SRVD' ).
    DATA(lv_json) = lo_command->zif_abgagt_command~execute( is_param = ls_params ).
    DATA ls_result TYPE zcl_abgagt_command_view=>ty_view_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-objects[ 1 ]-not_found ).
    cl_abap_unit_assert=>assert_initial( ls_result-objects[ 1 ]-error ).
  ENDMETHOD.
  METHOD unsupported_viewer_fails.
    DATA(lo_command) = NEW zcl_abgagt_command_view( ).
    DATA(ls_params) = VALUE zcl_abgagt_command_view=>ty_view_params(
      objects = VALUE #( ( `ZABGAGT_VIEW_MISSING` ) ) type = 'XXXX' ).
    DATA(lv_json) = lo_command->zif_abgagt_command~execute( is_param = ls_params ).
    DATA ls_result TYPE zcl_abgagt_command_view=>ty_view_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_false( ls_result-objects[ 1 ]-not_found ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-objects[ 1 ]-error ).
  ENDMETHOD.
ENDCLASS.
