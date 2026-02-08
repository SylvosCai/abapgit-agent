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
          lv_has_class TYPE abap_bool.

    rs_result-success = abap_true.

    " Check if source contains CLASS DEFINITION
    LOOP AT it_source_code ASSIGNING FIELD-SYMBOL(<ls_line>).
      IF <ls_line> CP 'CLASS * DEFINITION*'.
        lv_has_class = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_has_class = abap_true.
      " For CLASS files, use INSERT REPORT + SYNTAX-CHECK
      lv_prog_name = |Z_SYNTAX_CHECK_{ sy-uname }|.
      INSERT REPORT lv_prog_name FROM it_source_code STATE 'A'.

      IF sy-subrc <> 0.
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-line = '1'.
        rs_result-text = 'Failed to insert source code for syntax check'.
        RETURN.
      ENDIF.

      " Read back and check
      DATA lt_source TYPE STANDARD TABLE OF STRING.
      READ REPORT lv_prog_name INTO lt_source.

      IF sy-subrc <> 0.
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-line = '1'.
        rs_result-text = 'Failed to read inserted source'.
        RETURN.
      ENDIF.

      " Get TRDIR properties
      DATA ls_dir TYPE trdir.
      SELECT SINGLE * FROM trdir INTO ls_dir WHERE name = lv_prog_name.

      IF sy-subrc <> 0.
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-line = '1'.
        rs_result-text = 'Failed to get TRDIR properties'.
        RETURN.
      ENDIF.

      " Perform syntax check
      SYNTAX-CHECK FOR lt_source
        MESSAGE lv_word
        LINE lv_line
        DIRECTORY ENTRY ls_dir.

      " Cleanup - ignore result
      DELETE REPORT lv_prog_name.       "# EC NOTEXT
    ELSE.
      " For non-CLASS files (PROG, etc.), use subroutine pool
      " Strip REPORT/PROGRAM statement
      DATA lt_subpool TYPE string_table.

      LOOP AT it_source_code ASSIGNING <ls_line>.
        DATA(lv_line_text) = <ls_line>.
        CONDENSE lv_line_text.

        " Skip REPORT, PROGRAM, or FUNCTION-POOL statements
        IF lv_line_text CP 'REPORT*' OR lv_line_text CP 'PROGRAM*' OR lv_line_text CP 'FUNCTION-POOL*'.
          CONTINUE.
        ENDIF.

        APPEND <ls_line> TO lt_subpool.
      ENDLOOP.

      " Wrap in subroutine pool structure
      INSERT 'PROGRAM SUBPOOL.' INTO lt_subpool INDEX 1.
      INSERT 'FORM check_syntax.' INTO lt_subpool INDEX 2.
      APPEND 'ENDFORM.' TO lt_subpool.

      " Generate subroutine pool for syntax checking
      lv_prog_name = |Z_SUBPOOL_{ sy-uname }|.
      GENERATE SUBROUTINE POOL lt_subpool
        NAME lv_prog_name
        MESSAGE lv_word
        LINE lv_line.
    ENDIF.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = lv_line.
      rs_result-text = lv_word.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
