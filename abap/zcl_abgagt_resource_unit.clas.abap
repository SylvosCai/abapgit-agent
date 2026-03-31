*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_unit DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_unit IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_unit.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Unit'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_unit=>ty_unit_params.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_unit=>ty_unit_params.
    ls_request = is_request.
    IF ls_request-package IS NOT INITIAL OR ( ls_request-files IS NOT INITIAL AND lines( ls_request-files ) > 0 ).
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE zcl_abgagt_command_unit=>ty_unit_params.
    ls_request = is_request.
    IF ls_request-package IS INITIAL AND ( ls_request-files IS INITIAL OR lines( ls_request-files ) = 0 ).
      rv_message = 'Package or files required'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
