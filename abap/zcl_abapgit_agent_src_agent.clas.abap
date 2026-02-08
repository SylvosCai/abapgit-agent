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
        iv_source_code TYPE c LENGTH 40
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

    " Try to read the source code from TRDIR
    " First try with the exact name (for PROG objects)
    READ REPORT iv_source_code INTO lt_source.

    IF sy-subrc <> 0.
      " For CLAS objects, TRDIR entries have format: ZCL_XXX======CP
      " Search for entries with pattern <name>======*
      DATA(lv_name_pattern) = |{ iv_source_code }======%|.
      SELECT SINGLE name FROM trdir
        INTO @DATA(lv_trdir_name)
        WHERE name LIKE @lv_name_pattern
        AND subc = '1'.  " Main program

      IF sy-subrc = 0.
        READ REPORT lv_trdir_name INTO lt_source.
      ENDIF.
    ENDIF.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = '1'.
      rs_result-text = |Source { iv_source_code } not found in TRDIR|.
      RETURN.
    ENDIF.

    " Get program properties from TRDIR
    SELECT SINGLE * FROM trdir
      INTO ls_dir
      WHERE name = iv_source_code.

    IF sy-subrc <> 0.
      " Try with class pattern
      DATA(lv_dir_pattern) = |{ iv_source_code }======%|.
      SELECT SINGLE * FROM trdir
        INTO ls_dir
        WHERE name LIKE @lv_dir_pattern.
    ENDIF.

    IF sy-subrc <> 0.
      " Use default properties
      CLEAR ls_dir.
      ls_dir-name = iv_source_code.
      ls_dir-type = '1'.
      ls_dir-subc = '1'.
      ls_dir-uccheck = 'X'.
    ENDIF.

    " Perform syntax check
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
