*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_syntax_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS syntax_check
      IMPORTING
        iv_object_type TYPE string
        iv_object_name TYPE string
      RETURNING
        VALUE(rs_result) TYPE string.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax_agent IMPLEMENTATION.

  METHOD syntax_check.
    " Return placeholder - CL_CI_INSPECTION not available in this system
    DATA lv_success TYPE string VALUE ''.
    DATA lv_error_count TYPE string VALUE '1'.

    CONCATENATE
      '{"success":"' lv_success '",'
      '"object_type":"' iv_object_type '",'
      '"object_name":"' iv_object_name '",'
      '"error_count":' lv_error_count ','
      '"errors":[{"line":"1","column":"1","text":"Syntax check not available in this system","word":""}]'
      '}'
      INTO rs_result.
  ENDMETHOD.

ENDCLASS.
