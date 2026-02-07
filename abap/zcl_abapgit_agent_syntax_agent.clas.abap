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
    DATA ls_error LIKE LINE OF rs_result-errors.

    rs_result-success = abap_true.
    rs_result-object_type = iv_object_type.
    rs_result-object_name = iv_object_name.

    TRY.
        " Use ZCL_ABAPGIT_CODE_INSPECTOR to run syntax check
        DATA(li_code_inspector) = zcl_abapgit_code_inspector=>get_code_inspector(
          iv_package = '$ABAP_AI_BRIDGE' ).

        " Get results using the interface
        DATA lt_results TYPE zif_abapgit_code_inspector=>ty_results.
        lt_results = li_code_inspector->run(
          iv_variant = 'SYNTAX_CHECK' ).

        " Parse results
        LOOP AT lt_results INTO DATA(ls_result).
          CLEAR ls_error.
          ls_error-line = ls_result-line.
          ls_error-column = ls_result-col.
          ls_error-text = ls_result-text.
          ls_error-word = ls_result-code.
          APPEND ls_error TO rs_result-errors.
        ENDLOOP.

        rs_result-error_count = lines( rs_result-errors ).
        IF rs_result-error_count > 0.
          rs_result-success = abap_false.
        ENDIF.

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
