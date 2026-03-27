*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_create DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_create IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_create.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Create'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_create=>ty_create_params.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_create=>ty_create_params.
    ls_request = is_request.
    rv_valid = xsdbool( ls_request-url IS NOT INITIAL AND ls_request-package IS NOT INITIAL ).
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE zcl_abgagt_command_create=>ty_create_params.
    ls_request = is_request.
    IF ls_request-url IS INITIAL OR ls_request-package IS INITIAL.
      rv_message = 'URL and package are required'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
