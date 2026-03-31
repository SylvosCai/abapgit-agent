*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_delete DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_delete IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_delete.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Delete'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_delete=>ty_delete_params.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_delete=>ty_delete_params.
    ls_request = is_request.
    IF ls_request-url IS NOT INITIAL OR ls_request-repo_key IS NOT INITIAL.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE zcl_abgagt_command_delete=>ty_delete_params.
    ls_request = is_request.
    IF ls_request-url IS INITIAL AND ls_request-repo_key IS INITIAL.
      rv_message = 'URL or repo_key is required'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
