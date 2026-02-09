*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_pull DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_pull IMPLEMENTATION.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Parse JSON using /ui2/cl_json
    DATA: BEGIN OF ls_request,
            url TYPE string,
            branch TYPE string,
            username TYPE string,
            password TYPE string,
            files TYPE string_table,
          END OF ls_request.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    IF ls_request-branch IS INITIAL.
      ls_request-branch = 'main'.
    ENDIF.

    DATA lv_json_resp TYPE string.
    IF ls_request-url IS INITIAL.
      lv_json_resp = '{"success":"","job_id":"","error":"URL is required"}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    DATA(lo_agent) = NEW zcl_abgagt_agent( ).
    DATA(ls_result) = lo_agent->zif_abgagt_agent~pull(
      iv_url      = ls_request-url
      iv_branch   = ls_request-branch
      iv_username = ls_request-username
      iv_password = ls_request-password
      it_files    = ls_request-files ).

    DATA(lv_success) = COND string( WHEN ls_result-success = abap_true THEN 'X' ELSE '' ).

    " Build response structure with internal tables
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

    ls_response-success = lv_success.
    ls_response-job_id = ls_result-job_id.
    ls_response-message = ls_result-message.
    ls_response-error_detail = ls_result-error_detail.
    ls_response-activated_count = ls_result-activated_count.
    ls_response-failed_count = ls_result-failed_count.
    ls_response-log_messages = ls_result-log_messages.
    ls_response-activated_objects = ls_result-activated_objects.
    ls_response-failed_objects = ls_result-failed_objects.

    " Use /UI2/CL_JSON for proper JSON serialization
    lv_json_resp = /ui2/cl_json=>serialize( data = ls_response ).


    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
