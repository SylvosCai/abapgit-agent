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
          lv_has_class TYPE abap_bool,
          lv_line_text TYPE string.

    rs_result-success = abap_true.

    " Check if source contains CLASS DEFINITION
    LOOP AT it_source_code ASSIGNING FIELD-SYMBOL(<ls_line>).
      lv_line_text = <ls_line>.
      CONDENSE lv_line_text.

      IF lv_line_text CP 'CLASS * DEFINITION*'.
        lv_has_class = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_has_class = abap_true.
      " For CLASS files, extract method implementations for checking
      DATA lt_methods TYPE string_table.
      DATA lv_in_method TYPE abap_bool.

      LOOP AT it_source_code ASSIGNING <ls_line>.
        lv_line_text = <ls_line>.
        CONDENSE lv_line_text.

        " Skip CLASS statements
        IF lv_line_text CP 'CLASS * DEFINITION*' OR lv_line_text CP 'ENDCLASS*' OR
           lv_line_text CP 'PUBLIC SECTION*' OR lv_line_text CP 'PRIVATE SECTION*' OR
           lv_line_text CP 'PROTECTED SECTION*' OR lv_line_text CP 'METHODS*' OR
           lv_line_text CP 'CLASS-METHODS*' OR lv_line_text CP 'DATA*' OR
           lv_line_text CP 'CLASS-DATA*' OR lv_line_text CP 'CONSTANTS*'.
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

      " Add dummy FORM structure for syntax check
      INSERT 'FORM check.' INTO lt_methods INDEX 1.
      APPEND 'ENDFORM.' TO lt_methods.

      lv_prog_name = |Z_SUBPOOL_{ sy-uname }|.
      GENERATE SUBROUTINE POOL lt_methods
        NAME lv_prog_name
        MESSAGE lv_word
        LINE lv_line.
    ELSE.
      " For non-CLASS files, strip REPORT/PROGRAM
      DATA lt_source TYPE string_table.

      LOOP AT it_source_code ASSIGNING <ls_line>.
        lv_line_text = <ls_line>.
        CONDENSE lv_line_text.

        IF lv_line_text CP 'REPORT*' OR lv_line_text CP 'PROGRAM*' OR lv_line_text CP 'FUNCTION-POOL*'.
          CONTINUE.
        ENDIF.

        APPEND <ls_line> TO lt_source.
      ENDLOOP.

      INSERT 'PROGRAM SUBPOOL.' INTO lt_source INDEX 1.
      INSERT 'FORM check_syntax.' INTO lt_source INDEX 2.
      APPEND 'ENDFORM.' TO lt_source.

      lv_prog_name = |Z_SUBPOOL_{ sy-uname }|.
      GENERATE SUBROUTINE POOL lt_source
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
