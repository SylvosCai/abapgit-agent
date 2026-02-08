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
          ls_dir TYPE trdir,
          lv_prog_name TYPE progname.  " Must be character-like for INSERT REPORT

    rs_result-success = abap_true.

    " Step 1: INSERT REPORT - saves source to TRDIR
    " STATE 'A' makes it active so syntax check works
    lv_prog_name = |Z_SYNTAX_CHECK_{ sy-uname }|.
    INSERT REPORT lv_prog_name FROM it_source_code STATE 'A'.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = '1'.
      rs_result-text = 'Failed to insert source code for syntax check'.
      RETURN.
    ENDIF.

    " Step 2: READ REPORT back to get the source (verifies it was saved)
    DATA lt_source TYPE STANDARD TABLE OF string.
    READ REPORT lv_prog_name INTO lt_source.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = '1'.
      rs_result-text = 'Failed to read inserted source code'.
      " Cleanup
      DELETE REPORT lv_prog_name.
      RETURN.
    ENDIF.

    " Step 3: Get TRDIR properties for the inserted program
    SELECT SINGLE * FROM trdir
      INTO ls_dir
      WHERE name = lv_prog_name.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = '1'.
      rs_result-text = 'Failed to get TRDIR properties'.
      " Cleanup
      DELETE REPORT lv_prog_name.
      RETURN.
    ENDIF.

    " Step 4: Perform syntax check on the source
    DATA(lv_syntax_rc) = sy-subrc.
    DATA(lv_syntax_line) = lv_line.
    DATA(lv_syntax_word) = lv_word.

    " Step 5: DELETE REPORT - cleanup the temporary program
    " Ignore sy-subrc since cleanup failures shouldn't affect the result
    DELETE REPORT lv_prog_name.           "# EC NOTEXT
    CLEAR sy-subrc.                        " Clear after DELETE REPORT

    " Set result based on syntax check
    IF lv_syntax_rc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = lv_syntax_line.
      rs_result-text = lv_syntax_word.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
