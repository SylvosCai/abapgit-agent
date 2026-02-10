*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_pull DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_pull_params,
             url TYPE string,
             branch TYPE string,
             username TYPE string,
             password TYPE string,
             files TYPE string_table,
           END OF ty_pull_params.

ENDCLASS.

CLASS zcl_abgagt_command_pull IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_pull.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_pull_params,
          lo_agent TYPE REF TO zcl_abgagt_agent,
          ls_pull_result TYPE zif_abgagt_agent=>ty_result,
          lx_error TYPE REF TO zcx_abapgit_exception.

    " Parse parameters from is_param
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Get agent instance and execute pull
    lo_agent = NEW zcl_abgagt_agent( ).

    TRY.
        ls_pull_result = lo_agent->zif_abgagt_agent~pull(
          iv_url      = ls_params-url
          iv_branch   = ls_params-branch
          iv_username = ls_params-username
          iv_password = ls_params-password
          it_files    = ls_params-files ).
      CATCH zcx_abapgit_exception INTO lx_error.
        ls_pull_result-success = abap_false.
        ls_pull_result-message = lx_error->get_text( ).
    ENDTRY.

    " Convert result to JSON string using /ui2/cl_json
    DATA: BEGIN OF ls_response,
            success TYPE string,
            job_id TYPE string,
            message TYPE string,
            error_detail TYPE string,
            activated_count TYPE i,
            failed_count TYPE i,
            log_messages TYPE zif_abgagt_agent=>ty_object_list,
            activated_objects TYPE zif_abgagt_agent=>ty_object_list,
            failed_objects TYPE zif_abgagt_agent=>ty_object_list,
          END OF ls_response.

    ls_response-success = COND string( WHEN ls_pull_result-success = abap_true THEN 'X' ELSE '' ).
    ls_response-job_id = ls_pull_result-job_id.
    ls_response-message = ls_pull_result-message.
    ls_response-error_detail = ls_pull_result-error_detail.
    ls_response-activated_count = ls_pull_result-activated_count.
    ls_response-failed_count = ls_pull_result-failed_count.
    ls_response-log_messages = ls_pull_result-log_messages.
    ls_response-activated_objects = ls_pull_result-activated_objects.
    ls_response-failed_objects = ls_pull_result-failed_objects.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.

ENDCLASS.
