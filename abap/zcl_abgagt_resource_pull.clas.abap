*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_pull DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PROTECTED SECTION.

    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS parse_request REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_pull IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_pull.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Pull'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_pull=>ty_pull_params.
  ENDMETHOD.

  METHOD parse_request.
    " Use typed local variable to set default branch
    DATA: ls_request TYPE zcl_abgagt_command_pull=>ty_pull_params.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = iv_json
      CHANGING
        data = ls_request ).

    " Set default branch if not provided
    IF ls_request-branch IS INITIAL.
      ls_request-branch = 'main'.
    ENDIF.

    es_request = ls_request.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE zcl_abgagt_command_pull=>ty_pull_params.
    ls_request = is_request.
    IF ls_request-url IS NOT INITIAL.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE zcl_abgagt_command_pull=>ty_pull_params.
    ls_request = is_request.
    IF ls_request-url IS INITIAL.
      rv_message = 'URL is required'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
