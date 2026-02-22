*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_import DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_import IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_import.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Import'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_import=>ty_import_params.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_import=>ty_import_params.
    ls_request = is_request.
    rv_valid = boolc( ls_request-url IS NOT INITIAL ).
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE zcl_abgagt_command_import=>ty_import_params.
    ls_request = is_request.
    IF ls_request-url IS INITIAL.
      rv_message = 'URL is required'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
