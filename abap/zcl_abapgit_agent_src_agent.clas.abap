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
          lv_is_class TYPE abap_bool,
          lv_line_text TYPE string.

    rs_result-success = abap_true.

    " Check if this is a CLASS definition
    lv_is_class = abap_false.
    LOOP AT it_source_code ASSIGNING FIELD-SYMBOL(<ls_line>).
      lv_line_text = <ls_line>.
      CONDENSE lv_line_text.
      IF lv_line_text CP 'CLASS * DEFINITION*'.
        lv_is_class = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_is_class = abap_true.
      " For CLASS files: extract method implementations and check with SUBPOOL
      DATA lt_methods TYPE string_table.
      DATA lv_in_method TYPE abap_bool.

      LOOP AT it_source_code ASSIGNING <ls_line>.
        CLEAR lv_line_text.
        lv_line_text = <ls_line>.
        CONDENSE lv_line_text.

        " Skip class definition statements
        IF lv_line_text CP 'CLASS * DEFINITION*' OR lv_line_text CP 'ENDCLASS*' OR
           lv_line_text CP 'PUBLIC SECTION*' OR lv_line_text CP 'PRIVATE SECTION*' OR
           lv_line_text CP 'PROTECTED SECTION*' OR lv_line_text CP 'METHODS*' OR
           lv_line_text CP 'CLASS-METHODS*' OR lv_line_text CP 'DATA*' OR
           lv_line_text CP 'CLASS-DATA*' OR lv_line_text CP 'CONSTANTS*' OR
           lv_line_text CP 'INTERFACES*'.
          CONTINUE.
        ENDIF.

        " Track if we're inside a method implementation
        IF lv_line_text CP 'METHOD *'.
          lv_in_method = abap_true.
          APPEND <ls_line> TO lt_methods.
        ELSEIF lv_line_text CP 'ENDMETHOD*'.
          lv_in_method = abap_false.
        ELSEIF lv_in_method = abap_true.
          APPEND <ls_line> TO lt_methods.
        ENDIF.
      ENDLOOP.

      " Add FORM structure for syntax check
      INSERT 'FORM check.' INTO lt_methods INDEX 1.
      APPEND 'ENDFORM.' TO lt_methods.

      lv_prog_name = |ZSPC_{ sy-uname }|.
      GENERATE SUBROUTINE POOL lt_methods
        NAME lv_prog_name
        MESSAGE lv_word
        LINE lv_line.
    ELSE.
      " For PROG files: use INSERT REPORT + SYNTAX-CHECK
      lv_prog_name = |ZSYN_{ sy-uname }|.
      INSERT REPORT lv_prog_name FROM it_source_code.

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

      " Get TRDIR properties
      DATA ls_dir TYPE trdir.
      SELECT SINGLE * FROM trdir INTO ls_dir WHERE name = lv_prog_name.

      IF sy-subrc <> 0.
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-line = '1'.
        rs_result-text = 'Failed to get TRDIR properties'.
        DELETE REPORT lv_prog_name.
        RETURN.
      ENDIF.

      " Perform syntax check - save result before cleanup
      DATA(lv_syntax_rc) = sy-subrc.
      DATA(lv_syntax_line) = lv_line.
      DATA(lv_syntax_word) = lv_word.

      SYNTAX-CHECK FOR lt_source
        MESSAGE lv_word
        LINE lv_line
        DIRECTORY ENTRY ls_dir.

      lv_syntax_rc = sy-subrc.
      lv_syntax_line = lv_line.
      lv_syntax_word = lv_word.

      " Cleanup
      DELETE REPORT lv_prog_name.

      IF lv_syntax_rc <> 0.
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-line = lv_syntax_line.
        rs_result-text = lv_syntax_word.
        RETURN.
      ENDIF.
    ENDIF.

    " For SUBPOOL, check result
    IF lv_is_class = abap_true AND sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-line = lv_line.
      rs_result-text = lv_word.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
