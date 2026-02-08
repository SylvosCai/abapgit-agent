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
          ls_dir TYPE trdir.

    rs_result-success = abap_true.

    " Try to get TRDIR properties for the source name
    " First try exact name (for PROG objects)
    SELECT SINGLE * FROM trdir
      INTO ls_dir
      WHERE name = iv_source_name.

    IF sy-subrc <> 0.
      " For CLAS objects, TRDIR entries have format: ZCL_XXX======CP
      " Try pattern <name>======%
      DATA(lv_name_pattern) = |{ iv_source_name }======%|.
      SELECT SINGLE * FROM trdir
        INTO ls_dir
        WHERE name LIKE lv_name_pattern
        AND subc = '1'.  " Main program
    ENDIF.

    IF sy-subrc <> 0.
      " Use default properties if not found
      CLEAR ls_dir.
      ls_dir-name = iv_source_name.
      ls_dir-type = '1'.
      ls_dir-subc = '1'.
      ls_dir-uccheck = 'X'.
    ENDIF.

    " Perform syntax check on the provided source code table
    SYNTAX-CHECK FOR it_source_code
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
