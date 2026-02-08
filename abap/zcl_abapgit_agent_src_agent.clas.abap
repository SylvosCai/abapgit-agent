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

    " Prepare source for subroutine pool - strip REPORT/PROGRAM statement
    DATA lt_source TYPE string_table.

    LOOP AT it_source_code ASSIGNING FIELD-SYMBOL(<ls_line>).
      DATA(lv_line_text) = <ls_line>.
      CONDENSE lv_line_text.

      " Skip REPORT, PROGRAM, or FUNCTION-POOL statements
      IF lv_line_text CP 'REPORT*' OR lv_line_text CP 'PROGRAM*' OR lv_line_text CP 'FUNCTION-POOL*'.
        CONTINUE.
      ENDIF.

      APPEND <ls_line> TO lt_source.
    ENDLOOP.

    " Wrap in subroutine pool structure
    INSERT 'PROGRAM SUBPOOL.' INTO lt_source INDEX 1.
    INSERT 'FORM check_syntax.' INTO lt_source INDEX 2.
    APPEND 'ENDFORM.' TO lt_source.

    " Generate subroutine pool for syntax checking
    GENERATE SUBROUTINE POOL lt_source
      NAME DATA(lv_prog_name)
      MESSAGE lv_word
      LINE lv_line.

    IF sy-subrc <> 0.
      " Syntax error detected
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = lv_line.
      rs_result-text = lv_word.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
