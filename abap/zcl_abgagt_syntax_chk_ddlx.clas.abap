CLASS zcl_abgagt_syntax_chk_ddlx DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.
ENDCLASS.
CLASS zcl_abgagt_syntax_chk_ddlx IMPLEMENTATION.
  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'DDLX'.
  ENDMETHOD.
  METHOD zif_abgagt_syntax_checker~set_fixpt.
  ENDMETHOD.
  METHOD zif_abgagt_syntax_checker~check.
    rs_result = zcl_abgagt_rap_syntax_util=>check( iv_type = 'DDLX' iv_name = iv_name it_source = it_source ).
  ENDMETHOD.
ENDCLASS.
