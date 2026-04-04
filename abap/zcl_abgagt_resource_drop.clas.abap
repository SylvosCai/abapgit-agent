*"* use source
*"* Local Interface:
**********************************************************************
CLASS zcl_abgagt_resource_drop DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_drop IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_drop.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Drop'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_drop=>ty_drop_params.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_drop=>ty_drop_params.
    ls_request = is_request.
    rv_valid = xsdbool( ls_request-file IS NOT INITIAL ).
  ENDMETHOD.

  METHOD get_error_message.
    rv_message = 'file is required'.
  ENDMETHOD.

ENDCLASS.
