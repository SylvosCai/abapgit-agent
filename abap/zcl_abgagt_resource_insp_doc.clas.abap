*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_insp_doc DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_insp_doc IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_inspect_doc.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'InspectDoc'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_inspect_doc=>ty_inspect_doc_params.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_inspect_doc=>ty_inspect_doc_params.
    ls_request = is_request.
    rv_valid = xsdbool(
      ls_request-check_class IS NOT INITIAL AND
      ls_request-check_code  IS NOT INITIAL ).
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE zcl_abgagt_command_inspect_doc=>ty_inspect_doc_params.
    ls_request = is_request.
    IF ls_request-check_class IS INITIAL.
      rv_message = 'check_class is required'.
    ELSEIF ls_request-check_code IS INITIAL.
      rv_message = 'check_code is required'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
