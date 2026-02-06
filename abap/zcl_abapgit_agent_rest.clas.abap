*"*"use source
*"* Local Interface:
*"  IMPORTING
*"    REQUEST TYPE REF TO if_web_http_request OPTIONAL
*"  METHOD(if_http_service_extension~handle_request)
*"  EXPORTING
*"    RESPONSE TYPE REF TO if_web_http_response OPTIONAL
*"--------------------------------------------------------------------
CLASS zcl_abapgit_agent_rest DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_http_service_extension.

  PRIVATE SECTION.
    CONSTANTS: gc_path_pull TYPE string VALUE '/pull',
               gc_path_status TYPE string VALUE '/status',
               gc_path_health TYPE string VALUE '/health'.

    METHODS: handle_pull
      IMPORTING ir_request TYPE REF TO if_web_http_request
               ir_response TYPE REF TO if_web_http_response,
             handle_status
      IMPORTING ir_request TYPE REF TO if_web_http_request
               ir_response TYPE REF TO if_web_http_response,
             handle_health
      IMPORTING ir_request TYPE REF TO if_web_http_request
               ir_response TYPE REF TO if_web_http_response,
             extract_json_value
      IMPORTING iv_json TYPE string
                iv_key TYPE string
      RETURNING VALUE(rv_value) TYPE string.

ENDCLASS.

CLASS zcl_abapgit_agent_rest IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.
    DATA lv_path TYPE string.
    lv_path = to_upper( request->get_path( ) ).

    CASE lv_path.
      WHEN gc_path_pull.
        handle_pull( ir_request = request ir_response = response ).
      WHEN gc_path_status.
        handle_status( ir_request = request ir_response = response ).
      WHEN gc_path_health.
        handle_health( ir_request = request ir_response = response ).
      WHEN OTHERS.
        response->set_status( i_code = 404 i_reason = 'Not Found' ).
        response->set_text( '{"error":"Unknown endpoint"}' ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_pull.
    DATA lv_json TYPE string.
    lv_json = request->get_cdata( ).

    DATA lv_url TYPE string.
    lv_url = extract_json_value( iv_json = lv_json iv_key = 'url' ).

    DATA lv_branch TYPE string.
    lv_branch = extract_json_value( iv_json = lv_json iv_key = 'branch' ).

    IF lv_branch IS INITIAL.
      lv_branch = 'main'.
    ENDIF.

    DATA lv_success TYPE char1.
    DATA lv_job_id TYPE string.
    DATA lv_msg TYPE string.

    CALL FUNCTION 'ZABAPGAGENT_PULL'
      EXPORTING
        iv_url    = lv_url
        iv_branch = lv_branch
      IMPORTING
        ev_success = lv_success
        ev_job_id  = lv_job_id
        ev_message = lv_msg.

    DATA lv_response TYPE string.
    lv_response = '{"success":"' && lv_success && '","job_id":"' &&
                 lv_job_id && '","message":"' && lv_msg && '"}'.

    response->set_content_type( 'application/json' ).
    response->set_text( lv_response ).
  ENDMETHOD.

  METHOD handle_status.
    DATA lv_job_id TYPE string.
    lv_job_id = request->get_form_field( 'job_id' ).

    DATA lv_status TYPE string.
    DATA lv_success TYPE char1.
    DATA lv_msg TYPE string.

    CALL FUNCTION 'ZABAPGAGENT_GET_STATUS'
      EXPORTING
        iv_job_id = lv_job_id
      IMPORTING
        ev_status  = lv_status
        ev_success = lv_success
        ev_message = lv_msg.

    DATA lv_response TYPE string.
    lv_response = '{"job_id":"' && lv_job_id && '","status":"' &&
                 lv_status && '","success":"' && lv_success &&
                 '","message":"' && lv_msg && '"}'.

    response->set_content_type( 'application/json' ).
    response->set_text( lv_response ).
  ENDMETHOD.

  METHOD handle_health.
    response->set_content_type( 'application/json' ).
    response->set_text( '{"status":"OK","version":"1.0"}' ).
  ENDMETHOD.

  METHOD extract_json_value.
    DATA lv_key TYPE string.
    DATA lv_pos TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_rest TYPE string.
    DATA lv_end_pos TYPE i.
    DATA lv_len TYPE i.

    lv_key = '"' && iv_key && '":'.
    FIND lv_key IN iv_json.
    IF sy-subrc = 0.
      lv_pos = strlen( lv_key ).
      lv_offset = sy-fdpos + lv_pos.
      lv_rest = iv_json+lv_offset.
      IF lv_rest(1) = '"'.
        lv_offset = 1.
        FIND '"' IN lv_rest MATCH OFFSET lv_end_pos.
        IF sy-subrc = 0.
          lv_len = lv_end_pos - 1.
          rv_value = lv_rest+lv_offset(lv_len).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
