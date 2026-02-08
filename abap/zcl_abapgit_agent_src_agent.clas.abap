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
          lv_prog_name TYPE progname.

    rs_result-success = abap_true.

    " Build source for syntax check
    DATA lt_source TYPE string_table.

    LOOP AT it_source_code ASSIGNING FIELD-SYMBOL(<ls_line>).
      " Skip top-level class/program statements
      IF <ls_line> CS 'CLASS ' OR <ls_line> CS 'ENDCLASS' OR
         <ls_line> CS 'REPORT' OR <ls_line> CS 'PROGRAM' OR
         <ls_line> CS 'FUNCTION-POOL' OR <ls_line> CS 'PUBLIC SECTION' OR
         <ls_line> CS 'PRIVATE SECTION' OR <ls_line> CS 'PROTECTED SECTION' OR
         <ls_line> CS 'METHODS ' OR <ls_line> CS 'CLASS-METHODS ' OR
         <ls_line> CS 'DATA:' OR <ls_line> CS 'CONSTANTS:' OR
         <ls_line> CS 'INTERFACES' OR <ls_line> CS 'METHOD ' OR
         <ls_line> CS 'ENDMETHOD' OR <ls_line> CS 'CLASS-DATA:' OR
         <ls_line> CS 'PARAMETERS' OR <ls_line> CS 'SELECT-OPTIONS'.
        CONTINUE.
      ENDIF.
      APPEND <ls_line> TO lt_source.
    ENDLOOP.

    " Wrap in subroutine pool structure
    INSERT 'PROGRAM SUBPOOL.' INTO lt_source INDEX 1.
    INSERT 'FORM check.' INTO lt_source INDEX 2.
    APPEND 'ENDFORM.' TO lt_source.

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
