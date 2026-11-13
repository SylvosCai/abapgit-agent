CLASS zcl_abgagt_syntax_chk_bdef DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.
ENDCLASS.
CLASS zcl_abgagt_syntax_chk_bdef IMPLEMENTATION.
  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'BDEF'.
  ENDMETHOD.
  METHOD zif_abgagt_syntax_checker~set_fixpt.
  ENDMETHOD.
  METHOD zif_abgagt_syntax_checker~check.
    rs_result = zcl_abgagt_rap_syntax_util=>check( iv_type = 'BDEF' iv_name = iv_name it_source = it_source ).
  ENDMETHOD.
ENDCLASS.
