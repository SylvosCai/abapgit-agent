*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_syntax_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_syntax_error,
      line TYPE string,
      column TYPE string,
      text TYPE string,
      word TYPE string,
    END OF ty_syntax_error.

    TYPES: ty_syntax_errors TYPE STANDARD TABLE OF ty_syntax_error WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_syntax_result,
      success TYPE abap_bool,
      object_type TYPE string,
      object_name TYPE string,
      error_count TYPE i,
      errors TYPE ty_syntax_errors,
    END OF ty_syntax_result.

    METHODS syntax_check
      IMPORTING
        iv_object_type TYPE string
        iv_object_name TYPE string
      RETURNING
        VALUE(rs_result) TYPE ty_syntax_result.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax_agent IMPLEMENTATION.

  METHOD syntax_check.
    rs_result-success = abap_false.
    rs_result-object_type = iv_object_type.
    rs_result-object_name = iv_object_name.

    DATA ls_err TYPE zif_abapgit_agent=>ty_syntax_error.

    IF iv_object_type IS INITIAL OR iv_object_name IS INITIAL.
      ls_err-line = '1'.
      ls_err-column = '1'.
      ls_err-text = 'Object type and name are required'.
      APPEND ls_err TO rs_result-errors.
      rs_result-error_count = 1.
      RETURN.
    ENDIF.

    " Return placeholder - CL_CI_INSPECTION needs further investigation
    ls_err-line = '1'.
    ls_err-column = '1'.
    ls_err-text = 'Syntax check not available - CL_CI_INSPECTION API requires different implementation'.
    APPEND ls_err TO rs_result-errors.
    rs_result-error_count = 1.
  ENDMETHOD.

ENDCLASS.
