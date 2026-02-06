*"*"use source
*"*"Local Interface:
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_pull DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

ENDCLASS.

CLASS zcl_abapgit_agent_pull IMPLEMENTATION.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    DATA lv_url TYPE string.
    DATA lv_branch TYPE string.
    DATA lv_username TYPE string.
    DATA lv_password TYPE string.

    " Extract URL from JSON using simple string operations
    FIND '"url"' IN lv_json MATCH OFFSET DATA(lv_off).
    IF sy-subrc = 0.
      DATA(lv_part) = lv_json+lv_off.
      FIND ':' IN lv_part MATCH OFFSET DATA(lv_col).
      IF sy-subrc = 0.
        lv_part = lv_part+lv_col+1.
        SHIFT lv_part LEFT UP TO '"'.
        SHIFT lv_part LEFT.
        FIND '"' IN lv_part MATCH OFFSET DATA(lv_qend).
        lv_url = lv_part(lv_qend).
      ENDIF.
    ENDIF.

    " Extract branch from JSON
    FIND '"branch"' IN lv_json MATCH OFFSET lv_off.
    IF sy-subrc = 0.
      lv_part = lv_json+lv_off.
      FIND ':' IN lv_part MATCH OFFSET lv_col.
      IF sy-subrc = 0.
        lv_part = lv_part+lv_col+1.
        SHIFT lv_part LEFT UP TO '"'.
        SHIFT lv_part LEFT.
        FIND '"' IN lv_part MATCH OFFSET lv_qend.
        lv_branch = lv_part(lv_qend).
      ENDIF.
    ENDIF.

    " Extract username from JSON
    FIND '"username"' IN lv_json MATCH OFFSET lv_off.
    IF sy-subrc = 0.
      lv_part = lv_json+lv_off.
      FIND ':' IN lv_part MATCH OFFSET lv_col.
      IF sy-subrc = 0.
        lv_part = lv_part+lv_col+1.
        SHIFT lv_part LEFT UP TO '"'.
        SHIFT lv_part LEFT.
        FIND '"' IN lv_part MATCH OFFSET lv_qend.
        lv_username = lv_part(lv_qend).
      ENDIF.
    ENDIF.

    " Extract password from JSON
    FIND '"password"' IN lv_json MATCH OFFSET lv_off.
    IF sy-subrc = 0.
      lv_part = lv_json+lv_off.
      FIND ':' IN lv_part MATCH OFFSET lv_col.
      IF sy-subrc = 0.
        lv_part = lv_part+lv_col+1.
        SHIFT lv_part LEFT UP TO '"'.
        SHIFT lv_part LEFT.
        FIND '"' IN lv_part MATCH OFFSET lv_qend.
        lv_password = lv_part(lv_qend).
      ENDIF.
    ENDIF.

    IF lv_branch IS INITIAL.
      lv_branch = 'main'.
    ENDIF.

    " Validate required fields
    IF lv_url IS INITIAL.
      DATA lv_json_resp TYPE string.
      lv_json_resp = '{"success":"","job_id":"","error":"URL is required"}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    DATA lv_success TYPE char1.
    DATA lv_job_id TYPE string.
    DATA lv_msg TYPE string.

    CALL FUNCTION 'ZABAPGAGENT_PULL'
      EXPORTING
        iv_url      = lv_url
        iv_branch   = lv_branch
        iv_username = lv_username
        iv_password = lv_password
      IMPORTING
        ev_success = lv_success
        ev_job_id  = lv_job_id
        ev_message = lv_msg.

    lv_json_resp = '{"success":"' && lv_success && '","job_id":"' &&
                 lv_job_id && '","message":"' && lv_msg && '"}'.

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
