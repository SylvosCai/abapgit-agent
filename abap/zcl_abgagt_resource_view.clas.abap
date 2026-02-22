*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_view DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS get_command_constant ABSTRACT
      IMPORTING iv_command TYPE string
      RETURNING VALUE(rv_constant) TYPE string REDEFINITION.

    METHODS get_command_name ABSTRACT REDEFINITION.

    METHODS parse_request ABSTRACT
      IMPORTING iv_json TYPE string
      CHANGING cs_request TYPE any REDEFINITION.

    METHODS validate_request ABSTRACT
      IMPORTING is_request TYPE any
      RETURNING VALUE(rv_valid) TYPE abap_bool REDEFINITION.

    METHODS get_error_message ABSTRACT
      IMPORTING is_request TYPE any
      RETURNING VALUE(rv_message) TYPE string REDEFINITION.

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
