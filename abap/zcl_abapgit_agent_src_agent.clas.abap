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
        iv_program_name TYPE string
      RETURNING
        VALUE(rs_result) TYPE ty_result.

ENDCLASS.

CLASS zcl_abapgit_agent_src_agent IMPLEMENTATION.

  METHOD syntax_check_source.
    DATA: lv_line TYPE i,
          lv_word TYPE string,
          lt_source TYPE STANDARD TABLE OF string,
          ls_dir TYPE trdir.

    rs_result-success = abap_true.

    " Read source code of the specified program into internal table
    READ REPORT iv_program_name INTO lt_source.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = '1'.
      rs_result-text = |Program { iv_program_name } not found or not accessible|.
      RETURN.
    ENDIF.

    " Get program properties from TRDIR
    SELECT SINGLE * FROM trdir
      INTO ls_dir
      WHERE name = iv_program_name.

    " Perform syntax check with DIRECTORY ENTRY
    SYNTAX-CHECK FOR lt_source
      MESSAGE lv_word
      LINE lv_line
      DIRECTORY ENTRY ls_dir.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = lv_line.
      rs_result-text = lv_word.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
