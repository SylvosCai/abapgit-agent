*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_view DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS parse_request REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_view IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_view.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'VIEW'.
  ENDMETHOD.

  METHOD parse_request.
    DATA: ls_request TYPE zcl_abgagt_command_view=>ty_view_params.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = iv_json
      CHANGING
        data = ls_request ).

    cs_request = ls_request.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_view=>ty_view_params.
    ls_request = is_request.
    rv_valid = boolc( ls_request-objects IS NOT INITIAL ).
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE zcl_abgagt_command_view=>ty_view_params.
    ls_request = is_request.
    IF ls_request-objects IS INITIAL.
      rv_message = 'Objects parameter is required'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
