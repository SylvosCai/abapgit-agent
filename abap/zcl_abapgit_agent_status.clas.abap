*"*"use source
*"* Local Interface:
*"  IMPORTING
*"    REQUEST TYPE REF TO if_rest_request
*"  METHOD(if_rest_handler~handle_request)
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_status DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_rest_handler.

ENDCLASS.

CLASS zcl_abapgit_agent_status IMPLEMENTATION.

  METHOD if_rest_handler~handle_request.
    DATA lv_job_id TYPE string.
    lv_job_id = request->get_form_field( 'job_id' ).

    DATA lv_status TYPE string.
    DATA lv_success TYPE char1.
    DATA lv_message TYPE string.

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

    response->set_entity( response->create_entity( ) ).
    response->get_entity( )->set_content_type( 'application/json' ).
    response->get_entity( )->set_cdata( lv_json_resp ).
    response->set_status( cl_rest_status_code=>ok ).
  ENDMETHOD.

ENDCLASS.
