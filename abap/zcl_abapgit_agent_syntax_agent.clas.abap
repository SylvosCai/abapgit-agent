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

    METHODS syntax_check
      IMPORTING
        iv_object_type TYPE string
        iv_object_name TYPE string
      RETURNING
        VALUE(rs_result) TYPE string.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax_agent IMPLEMENTATION.

  METHOD syntax_check.
    DATA: lv_success TYPE string VALUE '',
          lv_error_count TYPE i VALUE 0.
    DATA lt_errors TYPE ty_errors.
    DATA ls_error LIKE LINE OF lt_errors.

    " Use CL_CI_INSPECTION for syntax check
    TRY.
        DATA(lo_inspection) = NEW cl_ci_inspection( ).

        " Set the object to check
        lo_inspection->set_object(
          EXPORTING
            p_object_type = iv_object_type
            p_object_name = iv_object_name
            p_devclass    = '$ABAP_AI_BRIDGE' ).

        " Run the inspection with syntax check
        DATA lv_check TYPE string VALUE 'SYNTAX'.
        lo_inspection->inspect(
          EXPORTING
            p_name        = lv_check
            p_value       = 'X' ).

        " Get the results
        DATA lt_result TYPE cl_ci_inspection=>ty_result.
        lo_inspection->get_result(
          IMPORTING
            p_result = lt_result ).

        " Parse results into error structure
        LOOP AT lt_result INTO DATA(ls_result).
          CLEAR ls_error.
          " Extract line and column from message
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

        lv_error_count = lines( lt_errors ).
        IF lv_error_count = 0.
          lv_success = 'X'.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " If inspection fails, return error message
        CLEAR lt_errors.
        ls_error-line = '1'.
        ls_error-column = '1'.
        ls_error-text = |Inspection failed: { lx_error->get_text( ) }|.
        ls_error-word = ''.
        APPEND ls_error TO lt_errors.
        lv_error_count = 1.
    ENDTRY.

    " Build JSON response
    DATA lv_json_errors TYPE string.
    LOOP AT lt_errors INTO ls_error.
      IF lv_json_errors IS NOT INITIAL.
        lv_json_errors = lv_json_errors && ','.
      ENDIF.
      lv_json_errors = lv_json_errors &&
        '{"line":"' && ls_error-line && '",' &&
        '"column":"' && ls_error-column && '",' &&
        '"text":"' && ls_error-text && '",' &&
        '"word":"' && ls_error-word && '"}'.
    ENDLOOP.

    CONCATENATE
      '{"success":"' lv_success '",'
      '"object_type":"' iv_object_type '",'
      '"object_name":"' iv_object_name '",'
      '"error_count":' lv_error_count ','
      '"errors":[' lv_json_errors ']}'
      INTO rs_result.
  ENDMETHOD.

ENDCLASS.
