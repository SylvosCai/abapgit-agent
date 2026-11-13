CLASS zcl_abgagt_source_compare DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_source_compare.
ENDCLASS.

CLASS zcl_abgagt_source_compare IMPLEMENTATION.
  METHOD zif_abgagt_source_compare~prepare.
    rs_source-comparable_source = iv_source.
    rs_source-baseline_candidates = VALUE #( ( iv_source ) ).
    IF iv_obj_type <> 'BDEF'.
      RETURN.
    ENDIF.

    " Native BDEF readback omits one terminal delimiter, not source whitespace.
    rs_source-comparable_source = replace( val = iv_source pcre = `\r?\n\z` with = `` ).
    " Multiple terminal delimiters must not match a one-delimiter historical baseline.
    FIND PCRE `\n\z` IN rs_source-comparable_source.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    rs_source-baseline_candidates = VALUE #(
      ( rs_source-comparable_source )
      ( |{ rs_source-comparable_source }{ cl_abap_char_utilities=>newline }| )
      ( |{ rs_source-comparable_source }{ cl_abap_char_utilities=>cr_lf }| ) ).
  ENDMETHOD.
ENDCLASS.
