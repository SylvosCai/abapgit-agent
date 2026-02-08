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
          lv_word TYPE string,
          lv_prog_name TYPE progname,
          lv_line_text TYPE string.

    rs_result-success = abap_true.

    " Build subroutine pool source
    DATA lt_source TYPE string_table.

    " Check if this is a CLASS definition
    DATA(lv_is_class) = abap_false.
    LOOP AT it_source_code ASSIGNING FIELD-SYMBOL(<ls_line>).
      lv_line_text = <ls_line>.
      CONDENSE lv_line_text.
      IF lv_line_text CP 'CLASS * DEFINITION*'.
        lv_is_class = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_is_class = abap_true.
      " For CLASS: add dummy data declaration to suppress class-level errors
      APPEND 'DATA lv_dummy TYPE string.' TO lt_source.
      APPEND 'FORM check.' TO lt_source.

      LOOP AT it_source_code ASSIGNING <ls_line>.
        APPEND <ls_line> TO lt_source.
      ENDLOOP.

      APPEND 'ENDFORM.' TO lt_source.
    ELSE.
      " For non-CLASS: strip REPORT/PROGRAM and wrap in subroutine pool
      APPEND 'PROGRAM SUBPOOL.' TO lt_source.
      APPEND 'FORM check.' TO lt_source.

      LOOP AT it_source_code ASSIGNING <ls_line>.
        lv_line_text = <ls_line>.
        CONDENSE lv_line_text.
        IF lv_line_text CP 'REPORT*' OR lv_line_text CP 'PROGRAM*' OR lv_line_text CP 'FUNCTION-POOL*'.
          CONTINUE.
        ENDIF.
        APPEND <ls_line> TO lt_source.
      ENDLOOP.

      APPEND 'ENDFORM.' TO lt_source.
    ENDIF.

    " Generate subroutine pool for syntax checking
    lv_prog_name = |ZCHK_{ sy-uname }|.
    GENERATE SUBROUTINE POOL lt_source
      NAME lv_prog_name
      MESSAGE lv_word
      LINE lv_line.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = lv_line.
      rs_result-text = lv_word.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
