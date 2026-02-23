*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_inspect DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_inspect IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_inspect.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Inspect'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_inspect=>ty_inspect_params.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_inspect=>ty_inspect_params.
    ls_request = is_request.
    rv_valid = boolc( ls_request-files IS NOT INITIAL AND lines( ls_request-files ) > 0 ).
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE zcl_abgagt_command_inspect=>ty_inspect_params.
    ls_request = is_request.
    IF ls_request-files IS INITIAL OR lines( ls_request-files ) = 0.
      rv_message = 'Files list is required'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
