*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_util DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_util TYPE REF TO zif_abgagt_util.

    METHODS setup.
    METHODS test_parse_clas FOR TESTING.
    METHODS test_parse_intf FOR TESTING.
    METHODS test_parse_with_path FOR TESTING.
    METHODS test_parse_invalid FOR TESTING.
    METHODS test_rap_formats FOR TESTING.
    METHODS test_rap_invalid_reset FOR TESTING.
    METHODS test_parse_namespace_path FOR TESTING.
    METHODS test_parse_enho_hash_file FOR TESTING.
    METHODS test_parse_enho_with_path FOR TESTING.
    METHODS test_parse_enho_unknown_4part FOR TESTING.
    METHODS test_detect_include_method FOR TESTING.
    METHODS test_detect_intf_section FOR TESTING.
    METHODS test_detect_public_section FOR TESTING.
    METHODS test_convert_method_index FOR TESTING.
    METHODS test_convert_index_to_cm FOR TESTING.
ENDCLASS.

CLASS ltcl_zcl_abgagt_util IMPLEMENTATION.

  METHOD setup.
    mo_util = zcl_abgagt_util=>get_instance( ).
  ENDMETHOD.

  METHOD test_parse_clas.
    " Test parsing CLAS file
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->parse_file_to_object(
      EXPORTING iv_file = 'zcl_my_class.clas.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type exp = 'CLAS' msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name exp = 'ZCL_MY_CLASS' msg = 'Object name mismatch' ).
  ENDMETHOD.

  METHOD test_parse_intf.
    " Test parsing INTF file
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->parse_file_to_object(
      EXPORTING iv_file = 'zif_my_intf.intf.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type exp = 'INTF' msg = 'Object type should be INTF' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name exp = 'ZIF_MY_INTF' msg = 'Object name mismatch' ).
  ENDMETHOD.

  METHOD test_parse_with_path.
    " Test parsing file with path
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->parse_file_to_object(
      EXPORTING iv_file = 'src/zcl_my_class.clas.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type exp = 'CLAS' msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name exp = 'ZCL_MY_CLASS' msg = 'Path should be stripped' ).
  ENDMETHOD.

  METHOD test_parse_invalid.
    " Test parsing invalid file (should return empty)
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->parse_file_to_object(
      EXPORTING iv_file = 'invalid_file.txt'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_type msg = 'Invalid file should return empty type' ).
    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_name msg = 'Invalid file should return empty name' ).
  ENDMETHOD.

  METHOD test_rap_formats.
    TYPES: BEGIN OF ty_case,
             file TYPE string,
             obj_type TYPE string,
           END OF ty_case.
    TYPES tt_case TYPE STANDARD TABLE OF ty_case WITH EMPTY KEY.
    DATA(lt_cases) = VALUE tt_case(
      ( file = 'src.v2/zrap.bdef.asbdef' obj_type = 'BDEF' )
      ( file = 'src/zrap.bdef.xml' obj_type = 'BDEF' )
      ( file = 'src/zrap.srvd.srvdsrv' obj_type = 'SRVD' )
      ( file = 'src/zrap.srvd.assrvd' obj_type = 'SRVD' )
      ( file = 'src/zrap.srvd.xml' obj_type = 'SRVD' )
      ( file = 'src/zrap.ddlx.asddlxs' obj_type = 'DDLX' )
      ( file = 'src/zrap.ddlx.xml' obj_type = 'DDLX' ) ).
    LOOP AT lt_cases INTO DATA(ls_case).
      mo_util->parse_file_to_object(
        EXPORTING iv_file = ls_case-file
        IMPORTING ev_obj_type = DATA(lv_type) ev_obj_name = DATA(lv_name) ).
      cl_abap_unit_assert=>assert_equals( act = lv_type exp = ls_case-obj_type msg = ls_case-file ).
      cl_abap_unit_assert=>assert_equals( act = lv_name exp = 'ZRAP' msg = ls_case-file ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_rap_invalid_reset.
    DATA(lt_invalid) = VALUE string_table(
      ( `zrap.bdef.abap` ) ( `zrap.srvd.asbdef` ) ( `zrap.ddlx.srvdsrv` )
      ( `zrap.unknown.asbdef` ) ( `zrap.bdef.foo.xml` ) ( `.bdef.asbdef` )
      ( `zrap..bdef.asbdef` ) ( `zrap.bdef.` ) ( `#ns##root.bdef.xml` )
      ( `#ns#root#x.bdef.xml` ) ( `z rap.bdef.xml` )
      ( `zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz.bdef.asbdef` ) ( `invalid.txt` ) ).
    LOOP AT lt_invalid INTO DATA(lv_file).
      mo_util->parse_file_to_object(
        EXPORTING iv_file = 'zrap.bdef.asbdef'
        IMPORTING ev_obj_type = DATA(lv_type) ev_obj_name = DATA(lv_name) ).
      mo_util->parse_file_to_object(
        EXPORTING iv_file = lv_file
        IMPORTING ev_obj_type = lv_type ev_obj_name = lv_name ).
      cl_abap_unit_assert=>assert_initial( act = lv_type msg = lv_file ).
      cl_abap_unit_assert=>assert_initial( act = lv_name msg = lv_file ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_parse_namespace_path.
    DATA(lt_files) = VALUE string_table(
      ( `src.v2/sub/#Ns#Root.BdEf.AsBdEf` )
      ( `src.v2\sub\#Ns#Root.BdEf.AsBdEf` )
      ( `src.v2/#ns#root.clas.locals_imp.abap` )
      ( `src.v2/#ns#root.ddls.asddls` ) ).
    LOOP AT lt_files INTO DATA(lv_file).
      mo_util->parse_file_to_object(
        EXPORTING iv_file = lv_file
        IMPORTING ev_obj_type = DATA(lv_type) ev_obj_name = DATA(lv_name) ).
      cl_abap_unit_assert=>assert_equals( act = lv_name exp = '/NS/ROOT' msg = lv_file ).
      cl_abap_unit_assert=>assert_not_initial( act = lv_type msg = lv_file ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_parse_enho_hash_file.
    " 4-part ENHO file: <name>.enho.<hash>.abap → type=ENHO, name=ZCL_FOO
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->parse_file_to_object(
      EXPORTING iv_file = 'zcl_foo.enho.28bbfe2f.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type exp = 'ENHO' msg = 'Object type should be ENHO' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name exp = 'ZCL_FOO' msg = 'Object name should be ZCL_FOO' ).
  ENDMETHOD.

  METHOD test_parse_enho_with_path.
    " Path-prefixed ENHO hash file
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->parse_file_to_object(
      EXPORTING iv_file = 'src/zcl_foo.enho.d639f45c.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type exp = 'ENHO' msg = 'Object type should be ENHO' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_name exp = 'ZCL_FOO' msg = 'Path should be stripped' ).
  ENDMETHOD.

  METHOD test_parse_enho_unknown_4part.
    " 4-part file with unknown type should return empty
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->parse_file_to_object(
      EXPORTING iv_file = 'zcl_foo.unkn.d639f45c.abap'
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_type msg = 'Unknown 4-part type should return empty type' ).
    cl_abap_unit_assert=>assert_initial(
      act = lv_obj_name msg = 'Unknown 4-part type should return empty name' ).
  ENDMETHOD.

  METHOD test_detect_include_method.
    " Test detecting method include - obj_name should NOT have trailing equals
    DATA ls_info TYPE zif_abgagt_util=>ty_include_info.

    ls_info = mo_util->detect_include_info( 'ZCL_ABGAGT_COMMAND_PULL=======CM001' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_info-is_source_include exp = abap_true
      msg = 'Should be detected as source include' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_type exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_name exp = 'ZCL_ABGAGT_COMMAND_PULL'
      msg = 'Object name should NOT have trailing equals' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-include_type exp = 'CM001'
      msg = 'Include type should be CM001' ).
  ENDMETHOD.

  METHOD test_detect_intf_section.
    " Test detecting interface section
    DATA ls_info TYPE zif_abgagt_util=>ty_include_info.

    ls_info = mo_util->detect_include_info( 'ZIF_ABGAGT_CMD_FACTORY========IU' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_info-is_source_include exp = abap_true
      msg = 'Should be detected as source include' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_type exp = 'INTF'
      msg = 'Object type should be INTF' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_name exp = 'ZIF_ABGAGT_CMD_FACTORY'
      msg = 'Object name should NOT have trailing equals' ).
  ENDMETHOD.

  METHOD test_detect_public_section.
    " Test detecting public section
    DATA ls_info TYPE zif_abgagt_util=>ty_include_info.

    ls_info = mo_util->detect_include_info( 'ZCL_ABGAGT_COMMAND_VIEW=======CU' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_info-is_source_include exp = abap_true
      msg = 'Should be detected as source include' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_type exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_info-obj_name exp = 'ZCL_ABGAGT_COMMAND_VIEW'
      msg = 'Object name should NOT have trailing equals' ).
  ENDMETHOD.

  METHOD test_convert_method_index.
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_method_index( 'CM001' ) exp = 1
      msg = 'CM001 should be 1' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_method_index( 'CM00A' ) exp = 10
      msg = 'CM00A should be 10' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_method_index( 'CM00Z' ) exp = 35
      msg = 'CM00Z should be 35' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_method_index( 'CM010' ) exp = 36
      msg = 'CM010 should be 36' ).
  ENDMETHOD.

  METHOD test_convert_index_to_cm.
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_index_to_cm_suffix( 1 ) exp = 'CM001'
      msg = '1 should be CM001' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_index_to_cm_suffix( 10 ) exp = 'CM00A'
      msg = '10 should be CM00A' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_index_to_cm_suffix( 35 ) exp = 'CM00Z'
      msg = '35 should be CM00Z' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_util->convert_index_to_cm_suffix( 36 ) exp = 'CM010'
      msg = '36 should be CM010' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_rap_tadir DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_env TYPE REF TO if_osql_test_environment.
    METHODS setup.
    METHODS teardown.
    METHODS detects_rap FOR TESTING.
    METHODS preserves_legacy_priority FOR TESTING.
ENDCLASS.

CLASS ltcl_rap_tadir IMPLEMENTATION.
  METHOD setup.
    mo_env = cl_osql_test_environment=>create( VALUE #( ( 'TADIR' ) ) ).
  ENDMETHOD.
  METHOD teardown.
    mo_env->destroy( ).
  ENDMETHOD.
  METHOD detects_rap.
    DATA lt_rows TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.
    lt_rows = VALUE #( ( pgmid = 'R3TR' object = 'BDEF' obj_name = 'ZRAP_VIEW_B' devclass = '$TEST' )
                      ( pgmid = 'R3TR' object = 'SRVD' obj_name = 'ZRAP_VIEW_S' devclass = '$TEST' )
                      ( pgmid = 'R3TR' object = 'DDLX' obj_name = 'ZRAP_VIEW_X' devclass = '$TEST' ) ).
    mo_env->insert_test_data( lt_rows ).
    DATA(lo_util) = zcl_abgagt_util=>get_instance( ).
    LOOP AT lt_rows INTO DATA(ls_row).
      DATA(ls_info) = lo_util->get_object_info_from_tadir( CONV #( ls_row-obj_name ) ).
      cl_abap_unit_assert=>assert_equals( act = ls_info-obj_type exp = ls_row-object ).
      cl_abap_unit_assert=>assert_equals( act = ls_info-obj_name exp = ls_row-obj_name ).
      cl_abap_unit_assert=>assert_not_initial( ls_info-type_text ).
    ENDLOOP.
  ENDMETHOD.
  METHOD preserves_legacy_priority.
    DATA lt_rows TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.
    lt_rows = VALUE #( ( pgmid = 'R3TR' object = 'DDLS' obj_name = 'ZRAP_SHARED' )
                      ( pgmid = 'R3TR' object = 'BDEF' obj_name = 'ZRAP_SHARED' )
                      ( pgmid = 'R3TR' object = 'DDLX' obj_name = 'ZRAP_SHARED' ) ).
    mo_env->insert_test_data( lt_rows ).
    DATA(ls_info) = zcl_abgagt_util=>get_instance( )->get_object_info_from_tadir( 'ZRAP_SHARED' ).
    cl_abap_unit_assert=>assert_equals( act = ls_info-obj_type exp = 'DDLS' ).
  ENDMETHOD.
ENDCLASS.
