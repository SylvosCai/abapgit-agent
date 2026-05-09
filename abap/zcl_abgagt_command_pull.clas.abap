*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_pull DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    INTERFACES zif_abgagt_progressable.

    TYPES: BEGIN OF ty_pull_params,
             url TYPE string,
             branch TYPE string,
             username TYPE string,
             password TYPE string,
             transport_request TYPE string,
             files TYPE string_table,
             conflict_mode TYPE string,
           END OF ty_pull_params.

ENDCLASS.

CLASS zcl_abgagt_command_pull IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_pull.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_pull_params,
          ls_pull_result TYPE zif_abgagt_agent=>ty_result,
          lx_error TYPE REF TO zcx_abapgit_exception.

    " Stage 1: Parse parameters (10%)
    RAISE EVENT zif_abgagt_progressable~progress_update
      EXPORTING
        iv_stage    = 'INITIALIZATION'
        iv_message  = 'Initializing pull...'
        iv_progress = 10.

    " Parse parameters from is_param
    IF is_param IS SUPPLIED.
      " Handle both structure and JSON string input
      " When called from background job, is_param is a JSON string
      " When called directly (e.g., in unit tests), is_param is a structure
      DATA lv_param_type TYPE string.
      DESCRIBE FIELD is_param TYPE lv_param_type.

      IF lv_param_type = 'g' OR lv_param_type = 'C'.
        " String type - deserialize JSON
        DATA lv_json_string TYPE string.
        lv_json_string = is_param.
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = lv_json_string
          CHANGING
            data = ls_params
        ).
      ELSE.
        " Structure type - use CORRESPONDING
        ls_params = CORRESPONDING #( is_param ).
      ENDIF.
    ENDIF.

    " Default conflict_mode to 'abort' if not provided
    IF ls_params-conflict_mode IS INITIAL.
      ls_params-conflict_mode = 'abort'.
    ENDIF.

    " Stage 2: Find repository (30%)
    RAISE EVENT zif_abgagt_progressable~progress_update
      EXPORTING
        iv_stage    = 'FIND_REPO'
        iv_message  = 'Finding repository...'
        iv_progress = 30.

    " Get agent instance and execute pull
    DATA(lo_agent) = NEW zcl_abgagt_agent( ).

    TRY.
        " Stage 3: Activate objects (50%)
        RAISE EVENT zif_abgagt_progressable~progress_update
          EXPORTING
            iv_stage    = 'ACTIVATE'
            iv_message  = |Activating { lines( ls_params-files ) } file(s)...|
            iv_progress = 50
            iv_total    = lines( ls_params-files ).

        ls_pull_result = lo_agent->zif_abgagt_agent~pull(
          iv_url               = ls_params-url
          iv_branch            = ls_params-branch
          iv_username          = ls_params-username
          iv_password          = ls_params-password
          iv_transport_request = ls_params-transport_request
          it_files             = ls_params-files
          iv_conflict_mode     = ls_params-conflict_mode ).
      CATCH zcx_abapgit_exception INTO lx_error.
        ls_pull_result-success = abap_false.
        ls_pull_result-message = lx_error->get_text( ).
    ENDTRY.

    " Stage 4: Complete (100%)
    RAISE EVENT zif_abgagt_progressable~progress_update
      EXPORTING
        iv_stage    = 'COMPLETE'
        iv_message  = 'Pull completed'
        iv_progress = 100.

    " Convert result to JSON string using /ui2/cl_json
    DATA: BEGIN OF ls_response,
            success TYPE string,
            job_id TYPE string,
            message TYPE string,
            error_detail TYPE string,
            transport_request TYPE string,
            activated_count TYPE i,
            failed_count TYPE i,
            log_messages TYPE zif_abgagt_agent=>ty_object_list,
            activated_objects TYPE zif_abgagt_agent=>ty_object_list,
            failed_objects TYPE zif_abgagt_agent=>ty_object_list,
            conflict_report TYPE string,
            conflict_count TYPE i,
            local_xml_files TYPE zif_abgagt_agent=>ty_xml_files,
          END OF ls_response.

    ls_response-success = COND string( WHEN ls_pull_result-success = abap_true THEN 'X' ELSE '' ).
    ls_response-job_id = ls_pull_result-job_id.
    ls_response-message = ls_pull_result-message.
    ls_response-error_detail = ls_pull_result-error_detail.
    ls_response-transport_request = ls_pull_result-transport_request.
    ls_response-activated_count = ls_pull_result-activated_count.
    ls_response-failed_count = ls_pull_result-failed_count.
    ls_response-log_messages = ls_pull_result-log_messages.
    ls_response-activated_objects = ls_pull_result-activated_objects.
    ls_response-failed_objects = ls_pull_result-failed_objects.
    ls_response-conflict_report = ls_pull_result-conflict_report.
    ls_response-conflict_count = ls_pull_result-conflict_count.
    ls_response-local_xml_files = ls_pull_result-local_xml_files.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.

ENDCLASS.
