*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_src_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             error_count TYPE i,
             line TYPE string,
             column TYPE string,
             text TYPE string,
           END OF ty_result.

    METHODS syntax_check_source
      IMPORTING
        iv_source_name TYPE string
        it_source_code TYPE string_table
      RETURNING
        VALUE(rs_result) TYPE ty_result.

ENDCLASS.

CLASS zcl_abapgit_agent_src_agent IMPLEMENTATION.

  METHOD syntax_check_source.
    DATA: lv_line TYPE i,
          lv_word TYPE string.

    rs_result-success = abap_true.

    " Try INSERT REPORT + SYNTAX-CHECK (no DIRECTORY ENTRY)
    DATA(lv_prog_name) = |ZSYN_{ sy-uname }|.
    INSERT REPORT lv_prog_name FROM it_source_code STATE 'A'.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = '1'.
      rs_result-text = 'Failed to insert source'.
      RETURN.
    ENDIF.

    " Read back
    DATA lt_source TYPE STANDARD TABLE OF STRING.
    READ REPORT lv_prog_name INTO lt_source.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = '1'.
      rs_result-text = 'Failed to read source'.
      DELETE REPORT lv_prog_name.
      RETURN.
    ENDIF.

    " Perform syntax check without DIRECTORY ENTRY
    SYNTAX-CHECK FOR lt_source
      MESSAGE lv_word
      LINE lv_line.

    " Cleanup
    DELETE REPORT lv_prog_name.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = lv_line.
      rs_result-text = lv_word.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
