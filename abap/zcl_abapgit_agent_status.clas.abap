*"*"use source
*"*"Local Interface:
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_status DEFINITION PUBLIC FINAL
                              INHERITING FROM cl_rest_resource
                              CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~get REDEFINITION.

ENDCLASS.

CLASS zcl_abapgit_agent_status IMPLEMENTATION.

  METHOD if_rest_resource~get.
    DATA lv_job_id TYPE string.
    lv_job_id = mo_request->get_uri_query_parameter( 'job_id' ).

    DATA lv_status TYPE string.
    DATA lv_success TYPE char1.
    DATA lv_message TYPE string.

    " TEST SYNTAX ERROR
    undefined_syntax_error_variable.

    CALL FUNCTION 'ZABAPGAGENT_GET_STATUS'
      EXPORTING
        iv_job_id = lv_job_id
      IMPORTING
        ev_status  = lv_status
        ev_success = lv_success
        ev_message = lv_message.

    DATA lv_json_resp TYPE string.
    lv_json_resp = '{"job_id":"' && lv_job_id && '","status":"' &&
                 lv_status && '","success":"' && lv_success &&
                 '","message":"' && lv_message && '"}'.

    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
