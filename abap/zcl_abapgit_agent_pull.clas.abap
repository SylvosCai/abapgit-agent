*"*"use source
*"* Local Interface:
*"  IMPORTING
*"    REQUEST TYPE REF TO if_rest_request
*"  METHOD(if_rest_handler~handle_request)
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_pull DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_rest_handler.

ENDCLASS.

CLASS zcl_abapgit_agent_pull IMPLEMENTATION.

  METHOD if_rest_handler~handle_request.
    DATA lv_json TYPE string.
    lv_json = request->get_entity( )->get_cdata( ).

    DATA lv_url TYPE string.
    DATA lv_branch TYPE string.

    FIND '"url"' IN lv_json.
    IF sy-subrc = 0.
      DATA lv_pos TYPE i.
      lv_pos = sy-fdpos + 6.
      lv_url = lv_json+lv_pos.
      SHIFT lv_url LEFT DELETING LEADING '"'.
      DATA lv_len TYPE i.
      lv_len = strlen( lv_url ).
      DATA lv_i TYPE i.
      DO lv_len TIMES.
        IF lv_url+lv_i(1) = '"' AND lv_url+lv_i+1(1) <> '\'.
          EXIT.
        ENDIF.
        lv_i = lv_i + 1.
      ENDDO.
      lv_url = lv_url(lv_i).
    ENDIF.

    FIND '"branch"' IN lv_json.
    IF sy-subrc = 0.
      lv_pos = sy-fdpos + 10.
      lv_branch = lv_json+lv_pos.
      SHIFT lv_branch LEFT DELETING LEADING '"'.
      lv_len = strlen( lv_branch ).
      DO lv_len TIMES.
        IF lv_branch+lv_i(1) = '"' AND lv_branch+lv_i+1(1) <> '\'.
          EXIT.
        ENDIF.
        lv_i = lv_i + 1.
      ENDDO.
      lv_branch = lv_branch(lv_i).
    ENDIF.

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

    DATA lv_json_resp TYPE string.
    lv_json_resp = '{"success":"' && lv_success && '","job_id":"' &&
                 lv_job_id && '","message":"' && lv_msg && '"}'.

    response->set_entity( response->create_entity( ) ).
    response->get_entity( )->set_content_type( 'application/json' ).
    response->get_entity( )->set_cdata( lv_json_resp ).
    response->set_status( cl_rest_status_code=>ok ).
  ENDMETHOD.

ENDCLASS.
