*"*"use source
*"*"Local Interface:
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_activate DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

ENDCLASS.

CLASS zcl_abapgit_agent_activate IMPLEMENTATION.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    DATA lv_url TYPE string.

    " Parse URL: find "url": "
    DATA lv_pos TYPE i.
    FIND '"url":' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_pos = lv_pos + 6.
      lv_url = lv_json+lv_pos.
      SHIFT lv_url LEFT UP TO '"'.
      SHIFT lv_url LEFT.
      FIND '"' IN lv_url MATCH OFFSET lv_pos.
      IF sy-subrc = 0.
        lv_url = lv_url(lv_pos).
      ENDIF.
    ENDIF.

    " Validate required fields
    IF lv_url IS INITIAL.
      DATA lv_json_resp TYPE string.
      lv_json_resp = '{"success":"","error":"URL is required"}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    DATA lv_success TYPE char1.
    DATA lv_msg TYPE string.

    CALL FUNCTION 'ZABAPGAGENT_ACTIVATE'
      EXPORTING
        iv_url = lv_url
      IMPORTING
        ev_success = lv_success
        ev_message = lv_msg.

    lv_json_resp = '{"success":"' && lv_success && '","message":"' && lv_msg && '"}'.

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
