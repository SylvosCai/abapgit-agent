*"*"use source
*"/----------------------------------------------------------------------\
*"* Local Interface:
*"  IMPORTING
*"    REQUEST(If_Http_Request) OPTIONAL
*"    METHOD(If_Http_Service~Handle_Request)
*"  EXPORTING
*"    RESPONSE(If_Http_Response) OPTIONAL
*"  EXCEPTIONS
*"      Invalid_Payload
*"--------------------------------------------------------------------

CLASS zcl_abapgit_agent_rest DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_http_service_extension.

  PRIVATE SECTION.
    CONSTANTS: gc_path_pull TYPE string VALUE '/pull',
               gc_path_status TYPE string VALUE '/status',
               gc_path_health TYPE string VALUE '/health'.

    METHODS: handle_pull
      IMPORTING ir_request  TYPE REF TO if_http_request
               ir_response TYPE REF TO if_http_response,
             handle_status
      IMPORTING ir_request  TYPE REF TO if_http_request
               ir_response TYPE REF TO if_http_response,
             handle_health
      IMPORTING ir_request  TYPE REF TO if_http_request
               ir_response TYPE REF TO if_http_response,
             parse_json_to_data
      IMPORTING iv_json TYPE string
      RETURNING VALUE(rs_data) TYPE string.

ENDCLASS.

CLASS zcl_abapgit_agent_rest IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.
    DATA(lv_path) = to_upper( request->get_path( ) ).

    CASE lv_path.
      WHEN gc_path_pull.
        handle_pull( request = request response = response ).
      WHEN gc_path_status.
        handle_status( request = request response = response ).
      WHEN gc_path_health.
        handle_health( request = request response = response ).
      WHEN OTHERS.
        response->set_status( code = 404 reason = 'Not Found' ).
        response->set_cdata( '{"error":"Unknown endpoint"}' ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_pull.
    DATA: lv_url    TYPE string,
          lv_branch TYPE string.

    " Parse JSON body
    DATA(lv_json) = request->get_cdata( ).

    " Simple JSON parsing (or use /ui2/cl_json)
    FIND REGEX '"url"\s*:\s*"([^"]+)"' IN lv_json SUBMATCHES lv_url.
    FIND REGEX '"branch"\s*:\s*"([^"]+)"' IN lv_json SUBMATCHES lv_branch.

    IF lv_branch IS INITIAL.
      lv_branch = 'main'.
    ENDIF.

    " Call function module
    DATA: lv_success TYPE char1,
          lv_job_id TYPE string,
          lv_msg    TYPE string.

    CALL FUNCTION 'ZABAPGAGENT_PULL'
      EXPORTING
        iv_url    = lv_url
        iv_branch = lv_branch
      IMPORTING
        ev_success = lv_success
        ev_job_id  = lv_job_id
        ev_message = lv_msg.

    " Build JSON response
    DATA(lv_response) = |\{\ "success\": "{ lv_success }", "job_id": "{ lv_job_id }", "message": "{ lv_msg }" \}|.

    response->set_content_type( 'application/json' ).
    response->set_cdata( lv_response ).
  ENDMETHOD.

  METHOD handle_status.
    DATA(lv_job_id) = request->get_query_string_parameter( 'job_id' ).

    DATA: lv_status TYPE string,
          lv_success TYPE char1,
          lv_msg    TYPE string.

    CALL FUNCTION 'ZABAPGAGENT_GET_STATUS'
      EXPORTING
        iv_job_id = lv_job_id
      IMPORTING
        ev_status  = lv_status
        ev_success = lv_success
        ev_message = lv_msg.

    DATA(lv_response) = |\{\ "job_id": "{ lv_job_id }", "status": "{ lv_status }", "success": "{ lv_success }", "message": "{ lv_msg }" \}|.

    response->set_content_type( 'application/json' ).
    response->set_cdata( lv_response ).
  ENDMETHOD.

  METHOD handle_health.
    response->set_content_type( 'application/json' ).
    response->set_cdata( '{"status":"OK","version":"1.0"}' ).
  ENDMETHOD.

  METHOD parse_json_to_data.
    " Placeholder for JSON parsing
  ENDMETHOD.

ENDCLASS.
