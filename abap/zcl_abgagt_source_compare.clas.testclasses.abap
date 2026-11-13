CLASS ltcl_source_compare DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS bdef_endings FOR TESTING RAISING zcx_abapgit_exception.
    METHODS other_types_exact FOR TESTING RAISING zcx_abapgit_exception.
    METHODS whitespace_preserved FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_source_compare IMPLEMENTATION.
  METHOD bdef_endings.
    DATA(lo_cut) = NEW zcl_abgagt_source_compare( ).
    DATA(lt_endings) = VALUE string_table( ( `` )
      ( CONV string( cl_abap_char_utilities=>newline ) )
      ( CONV string( cl_abap_char_utilities=>cr_lf ) ) ).
    LOOP AT lt_endings INTO DATA(lv_ending).
      DATA(ls_source) = lo_cut->zif_abgagt_source_compare~prepare(
        iv_obj_type = 'BDEF' iv_source = |managed;{ lv_ending }| ).
      cl_abap_unit_assert=>assert_equals( act = ls_source-comparable_source exp = `managed;` ).
      cl_abap_unit_assert=>assert_equals( act = lines( ls_source-baseline_candidates ) exp = 3 ).
    ENDLOOP.
  ENDMETHOD.

  METHOD other_types_exact.
    DATA(lo_cut) = NEW zcl_abgagt_source_compare( ).
    DATA(lt_types) = VALUE string_table( ( `CLAS` ) ( `DDLS` ) ( `DCLS` ) ( `SRVD` ) ( `DDLX` ) ).
    DATA(lv_source) = |source{ cl_abap_char_utilities=>newline }|.
    LOOP AT lt_types INTO DATA(lv_type).
      DATA(ls_source) = lo_cut->zif_abgagt_source_compare~prepare(
        iv_obj_type = CONV #( lv_type ) iv_source = lv_source ).
      cl_abap_unit_assert=>assert_equals( act = ls_source-comparable_source exp = lv_source ).
      cl_abap_unit_assert=>assert_equals( act = ls_source-baseline_candidates
                                        exp = VALUE string_table( ( lv_source ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD whitespace_preserved.
    DATA(lo_cut) = NEW zcl_abgagt_source_compare( ).
    DATA(lv_source) = | managed;  { cl_abap_char_utilities=>newline }{ cl_abap_char_utilities=>newline }|.
    DATA(ls_source) = lo_cut->zif_abgagt_source_compare~prepare(
      iv_obj_type = 'BDEF' iv_source = lv_source ).
    cl_abap_unit_assert=>assert_equals( act = ls_source-comparable_source
      exp = | managed;  { cl_abap_char_utilities=>newline }| ).
    cl_abap_unit_assert=>assert_equals( act = ls_source-baseline_candidates
                                      exp = VALUE string_table( ( lv_source ) ) ).
  ENDMETHOD.
ENDCLASS.
