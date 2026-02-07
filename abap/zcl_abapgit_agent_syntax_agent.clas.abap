*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_syntax_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_error,
             line TYPE string,
             column TYPE string,
             text TYPE string,
             word TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             object_type TYPE string,
             object_name TYPE string,
             error_count TYPE i,
             errors TYPE ty_errors,
           END OF ty_result.

    METHODS syntax_check
      IMPORTING
        iv_object_type TYPE string
        iv_object_name TYPE string
      RETURNING
        VALUE(rs_result) TYPE ty_result.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax_agent IMPLEMENTATION.

  METHOD syntax_check.
    DATA lt_errors TYPE ty_errors.
    DATA ls_error LIKE LINE OF lt_errors.

    rs_result-success = abap_true.
    rs_result-object_type = iv_object_type.
    rs_result-object_name = iv_object_name.

    TRY.
        DATA(lo_inspection) = NEW cl_ci_inspection( ).
        lo_inspection->set_object(
          EXPORTING
            p_object_type = iv_object_type
            p_object_name = iv_object_name
            p_devclass    = '$ABAP_AI_BRIDGE' ).

        DATA lv_check TYPE string VALUE 'SYNTAX'.
        lo_inspection->inspect(
          EXPORTING
            p_name        = lv_check
            p_value       = 'X' ).

        DATA lt_result TYPE cl_ci_inspection=>ty_result.
        lo_inspection->get_result(
          IMPORTING
            p_result = lt_result ).

        LOOP AT lt_result INTO DATA(ls_result).
          CLEAR ls_error.
          IF ls_result-message CP 'Line *'.
            FIND REGEX 'Line ([0-9]+), Column ([0-9]+): (.*)' IN ls_result-message
              SUBMATCHINGS DATA(lv_line) DATA(lv_col) DATA(lv_msg).
            IF sy-subrc = 0.
              ls_error-line = lv_line.
              ls_error-column = lv_col.
              ls_error-text = lv_msg.
            ELSE.
              ls_error-line = '1'.
              ls_error-column = '1'.
              ls_error-text = ls_result-message.
            ENDIF.
          ELSE.
            ls_error-line = '1'.
            ls_error-column = '1'.
            ls_error-text = ls_result-message.
          ENDIF.
          ls_error-word = ls_result-code.
          APPEND ls_error TO lt_errors.
        ENDLOOP.

        rs_result-error_count = lines( lt_errors ).
        IF rs_result-error_count > 0.
          rs_result-success = abap_false.
        ENDIF.
        rs_result-errors = lt_errors.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        ls_error-line = '1'.
        ls_error-column = '1'.
        ls_error-text = lx_error->get_text( ).
        ls_error-word = ''.
        APPEND ls_error TO rs_result-errors.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
