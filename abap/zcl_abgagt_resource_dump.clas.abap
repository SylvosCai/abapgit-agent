*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_dump DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_dump IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_dump.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Dump'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_dump=>ty_dump_params.
  ENDMETHOD.

  METHOD validate_request.
    " All parameters are optional for dump command
    rv_valid = abap_true.
  ENDMETHOD.

  METHOD get_error_message.
    rv_message = 'Dump request is invalid'.
  ENDMETHOD.

ENDCLASS.
