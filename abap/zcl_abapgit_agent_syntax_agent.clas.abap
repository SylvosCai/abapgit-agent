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
        " Use CL_CI_INSPECTION for syntax check
        DATA(lo_inspection) = NEW cl_ci_inspection( ).

        " Get error objects for the specified object
        DATA lt_err_objects TYPE cl_ci_inspection=>ty_obj_tab.
        lo_inspection->get_err_objects(
          EXPORTING
            p_obj_type = iv_object_type
            p_obj_name = iv_object_name
          IMPORTING
            p_err_obj_tab = lt_err_objects ).

        " Process each error object
        LOOP AT lt_err_objects INTO DATA(ls_err_obj).
          CLEAR ls_error.
          ls_error-line = ls_err_obj-line.
          ls_error-column = ls_err_obj-column.
          ls_error-text = ls_err_obj-text.
          ls_error-word = ls_err_obj-word.
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
