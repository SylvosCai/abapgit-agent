CLASS ltcl_rap_syntax DEFINITION DEFERRED.
CLASS zcl_abgagt_rap_syntax_util DEFINITION LOCAL FRIENDS ltcl_rap_syntax.
CLASS ltcl_rap_syntax DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS empty_source FOR TESTING.
    METHODS unsupported_type FOR TESTING.
    METHODS maps_errors_and_warnings FOR TESTING.
    METHODS warning_only_succeeds FOR TESTING.
    METHODS factory_dispatch FOR TESTING.
    METHODS bdef_source_unchanged FOR TESTING RAISING cx_wb_object_operation_error.
    METHODS new_bdef_attributes FOR TESTING RAISING cx_wb_object_operation_error.
    METHODS srvd_source_unchanged FOR TESTING RAISING cx_wb_object_operation_error.
    METHODS ddlx_source_unchanged FOR TESTING RAISING cx_wb_object_operation_error.
    METHODS invalid_new_object FOR TESTING.
    METHODS verify_fixture IMPORTING iv_type TYPE string iv_name TYPE seu_objkey
      iv_new TYPE abap_bool DEFAULT abap_false RAISING cx_wb_object_operation_error.
ENDCLASS.
CLASS ltcl_rap_syntax IMPLEMENTATION.
  METHOD empty_source.
    DATA(ls_result) = zcl_abgagt_rap_syntax_util=>check( iv_type = 'BDEF' iv_name = 'Z_EMPTY' it_source = VALUE #( ) ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-message exp = 'No source provided' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-error_count exp = 1 ).
  ENDMETHOD.
  METHOD unsupported_type.
    DATA(ls_result) = zcl_abgagt_rap_syntax_util=>check( iv_type = 'SRVB' iv_name = 'Z_UNKNOWN'
      it_source = VALUE #( ( `source` ) ) ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-message exp = 'Unsupported RAP syntax type: SRVB' ).
  ENDMETHOD.
  METHOD maps_errors_and_warnings.
    DATA(lo_log) = NEW cl_wb_checklist( ).
    lo_log->add_error_message( p_message_text = VALUE #( ( `Invalid token` ) ) p_line = 3 ).
    lo_log->add_error_message( p_message_text = VALUE #( ( `Deprecated annotation` ) ) p_line = 5 p_message_type = 'W' ).
    DATA ls_result TYPE zif_abgagt_syntax_checker=>ty_result.
    zcl_abgagt_rap_syntax_util=>convert_checklist( EXPORTING io_log = lo_log CHANGING cs_result = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-error_count exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-errors[ 1 ]-line exp = 3 ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-errors[ 1 ]-text exp = 'Invalid token' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-warnings[ 1 ]-line exp = 5 ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-warnings[ 1 ]-text exp = 'Deprecated annotation' ).
  ENDMETHOD.
  METHOD warning_only_succeeds.
    DATA(lo_log) = NEW cl_wb_checklist( ).
    lo_log->add_error_message( p_message_text = VALUE #( ( `Warning` ) ) p_message_type = 'W' ).
    DATA ls_result TYPE zif_abgagt_syntax_checker=>ty_result.
    zcl_abgagt_rap_syntax_util=>convert_checklist( EXPORTING io_log = lo_log CHANGING cs_result = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-error_count exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lines( ls_result-warnings ) exp = 1 ).
  ENDMETHOD.
  METHOD factory_dispatch.
    DATA(lt_types) = VALUE string_table( ( `BDEF` ) ( `SRVD` ) ( `DDLX` ) ).
    LOOP AT lt_types INTO DATA(lv_type).
      DATA(lo_checker) = zcl_abgagt_syntax_chk_factory=>create( lv_type ).
      cl_abap_unit_assert=>assert_bound( act = lo_checker msg = |Missing { lv_type } checker| ).
      cl_abap_unit_assert=>assert_equals( act = lo_checker->get_object_type( ) exp = lv_type ).
      DATA(ls_result) = lo_checker->check( iv_name = 'Z_EMPTY' it_source = VALUE #( ) ).
      cl_abap_unit_assert=>assert_equals( act = ls_result-object_type exp = lv_type ).
      cl_abap_unit_assert=>assert_false( ls_result-success ).
    ENDLOOP.
  ENDMETHOD.
  METHOD bdef_source_unchanged.
    verify_fixture( iv_type = 'BDEF' iv_name = 'ZR_ABGAGT_DRP_RAP' ).
  ENDMETHOD.
  METHOD new_bdef_attributes.
    verify_fixture( iv_type = 'BDEF' iv_name = 'ZR_ABGAGT_DRP_RAP' iv_new = abap_true ).
  ENDMETHOD.
  METHOD srvd_source_unchanged.
    verify_fixture( iv_type = 'SRVD' iv_name = 'ZUI_ABGAGT_DRP_RAP' ).
  ENDMETHOD.
  METHOD ddlx_source_unchanged.
    verify_fixture( iv_type = 'DDLX' iv_name = 'ZR_ABGAGT_DRP_RAP' ).
  ENDMETHOD.
  METHOD verify_fixture.
    DATA(lv_data_type) = SWITCH string( iv_type
      WHEN 'BDEF' THEN 'CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA'
      WHEN 'SRVD' THEN 'CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA'
      WHEN 'DDLX' THEN 'CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA' ).
    DATA ls_type TYPE wbobjtype.
    cl_wb_object_type=>get_global_from_transport_type( EXPORTING p_transport_type = CONV #( iv_type )
      IMPORTING p_global_type = ls_type EXCEPTIONS no_unique_mapping = 1 ).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Missing native Workbench mapping for { iv_type }| ).
    DATA(lo_operator) = cl_wb_object_operator=>create_instance( object_key = iv_name do_commits = abap_false
      object_type = ls_type ).
    cl_abap_unit_assert=>assert_true( act = lo_operator->check_existence( version = 'A' )
      msg = |Required active RAP fixture missing: { iv_type } { iv_name }| ).
    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_source> TYPE any.
    CREATE DATA lr_data TYPE (lv_data_type).
    ASSIGN lr_data->* TO <ls_data>.
    lo_operator->read( EXPORTING version = 'A' IMPORTING data = <ls_data> ).
    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_data> TO <lv_source>.
    DATA(lv_active_before) = CONV string( <lv_source> ).
    DATA(lv_inactive_before) = lo_operator->check_existence( version = 'I' ).
    DATA(lv_inactive_source) = CONV string( '' ).
    IF lv_inactive_before = abap_true.
      lo_operator->read( EXPORTING version = 'I' IMPORTING data = <ls_data> ).
      lv_inactive_source = <lv_source>.
    ENDIF.
    SPLIT lv_active_before AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_source).
    IF iv_new = abap_true.
      SELECT SINGLE FROM tadir FIELDS devclass WHERE pgmid = 'R3TR'
        AND object = @iv_type AND obj_name = @iv_name INTO @DATA(lv_package).
      cl_abap_unit_assert=>assert_subrc( exp = 0 msg = 'Required BDEF TADIR fixture missing' ).
      DATA lt_tadir TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.
      SELECT FROM tadir FIELDS pgmid, object, obj_name, devclass, srcsystem, masterlang
        WHERE devclass = @lv_package AND object <> 'BDEF'
        INTO CORRESPONDING FIELDS OF TABLE @lt_tadir.
      DATA(lo_env) = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'TADIR' ) ) ).
      lo_env->insert_test_data( i_data = lt_tadir ).
    ENDIF.
    TRY.
        DATA(ls_valid) = zcl_abgagt_rap_syntax_util=>check( iv_type = iv_type iv_name = iv_name it_source = lt_source ).
        DATA(ls_invalid) = zcl_abgagt_rap_syntax_util=>check( iv_type = iv_type iv_name = iv_name
          it_source = VALUE #( ( `This is not valid RAP source.` ) ) ).
      CLEANUP.
        IF lo_env IS BOUND.
          lo_env->destroy( ).
        ENDIF.
    ENDTRY.
    IF lo_env IS BOUND.
      lo_env->destroy( ).
    ENDIF.
    lo_operator->read( EXPORTING version = 'A' IMPORTING data = <ls_data> ).
    cl_abap_unit_assert=>assert_equals( act = CONV string( <lv_source> ) exp = lv_active_before ).
    cl_abap_unit_assert=>assert_equals( act = lo_operator->check_existence( version = 'I' ) exp = lv_inactive_before ).
    IF lv_inactive_before = abap_true.
      lo_operator->read( EXPORTING version = 'I' IMPORTING data = <ls_data> ).
      cl_abap_unit_assert=>assert_equals( act = CONV string( <lv_source> ) exp = lv_inactive_source ).
    ENDIF.
    cl_abap_unit_assert=>assert_true( act = ls_valid-success msg = ls_valid-message ).
    cl_abap_unit_assert=>assert_false( act = ls_invalid-success msg = |Invalid supplied { iv_type } source passed| ).
    cl_abap_unit_assert=>assert_not_initial( ls_invalid-errors ).
  ENDMETHOD.
  METHOD invalid_new_object.
    SELECT SINGLE FROM tadir FIELDS obj_name WHERE pgmid = 'R3TR'
      AND object = 'SRVD' AND obj_name = 'ZABGAGT_SYNTAX_MISSING' INTO @DATA(lv_existing).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = 'New-object test name is already occupied' ).
    DATA(ls_result) = zcl_abgagt_rap_syntax_util=>check( iv_type = 'SRVD' iv_name = 'ZABGAGT_SYNTAX_MISSING'
      it_source = VALUE #( ( `This is not valid RAP source.` ) ) ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    SELECT SINGLE FROM tadir FIELDS obj_name WHERE pgmid = 'R3TR'
      AND object = 'SRVD' AND obj_name = 'ZABGAGT_SYNTAX_MISSING' INTO @lv_existing.
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = 'Syntax check created a repository object' ).
  ENDMETHOD.
ENDCLASS.
