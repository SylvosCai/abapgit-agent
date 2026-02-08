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
        it_source_code TYPE string_table
      RETURNING
        VALUE(rs_result) TYPE ty_result.

ENDCLASS.

CLASS zcl_abapgit_agent_src_agent IMPLEMENTATION.

  METHOD syntax_check_source.
    DATA: lv_line TYPE i,
          lv_word TYPE string,
          ls_dir TYPE trdir.

    rs_result-success = abap_true.

    " Initialize directory entry with default values for a report program
    " Program type '1' = executable report program
    CLEAR ls_dir.
    ls_dir-name = 'ZSYNTAX_CHECK'.
    ls_dir-type = '1'.        " Report program
    ls_dir-subc = '1'.         " Executable program
    ls_dir-uccheck = 'X'.      " Unicode check active

    " Perform syntax check using SYNTAX-CHECK FOR ITAB with DIRECTORY ENTRY
    " sy-subrc = 0: no errors, 4: syntax error found, 8: runtime error
    SYNTAX-CHECK FOR it_source_code
      MESSAGE lv_word
      LINE lv_line
      DIRECTORY ENTRY ls_dir.

    IF sy-subrc <> 0.
      " Syntax error found - add to results
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = lv_line.
      rs_result-text = lv_word.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
