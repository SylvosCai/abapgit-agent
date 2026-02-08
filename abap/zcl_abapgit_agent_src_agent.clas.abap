*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_src_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_error,
             line TYPE string,
             column TYPE string,
             text TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             error_count TYPE i,
             errors TYPE ty_errors,
           END OF ty_result.

    METHODS syntax_check_source
      IMPORTING
        it_source_code TYPE string_table
      RETURNING
        VALUE(rs_result) TYPE ty_result.

ENDCLASS.

CLASS zcl_abapgit_agent_src_agent IMPLEMENTATION.

  METHOD syntax_check_source.
    DATA: lv_word     TYPE string,
          lv_line     TYPE i,
          lt_errtab   TYPE TABLE OF scsynerr.

    rs_result-success = abap_true.

    " Perform syntax check using SYNTAX-CHECK FOR ITAB
    " ID 'ERR' TABLE - collects all errors into internal table
    " Continues checking after errors (doesn't stop at first error)
    SYNTAX-CHECK FOR it_source_code
      MESSAGE lv_word
      LINE lv_line
      ID 'ERR' TABLE lt_errtab
      PROGRAM sy-repid.

    " Convert errors from scsynerr to ty_errors
    LOOP AT lt_errtab INTO DATA(ls_err).
      DATA(ls_error) = VALUE ty_error(
        line   = ls_err-line
        column = ls_err-col
        text   = ls_err-errortext ).
      APPEND ls_error TO rs_result-errors.
    ENDLOOP.

    rs_result-error_count = lines( rs_result-errors ).
    IF rs_result-error_count > 0.
      rs_result-success = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
