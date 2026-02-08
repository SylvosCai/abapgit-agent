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
    DATA: lv_word TYPE string,
          lv_line TYPE i.

    rs_result-success = abap_true.

    " Perform syntax check using SYNTAX-CHECK FOR ITAB
    " Reports first error via MESSAGE and LINE parameters
    " sy-subrc = 0: no errors, 4: syntax error found
    SYNTAX-CHECK FOR it_source_code
      MESSAGE lv_word
      LINE lv_line
      PROGRAM sy-repid.

    IF sy-subrc <> 0.
      " Syntax error found - add to results
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      APPEND VALUE #( line = lv_line
                      column = '1'
                      text = lv_word ) TO rs_result-errors.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
