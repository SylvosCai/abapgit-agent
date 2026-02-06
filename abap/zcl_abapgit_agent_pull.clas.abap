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

    " Parse branch: find "branch": "
    FIND '"branch":' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_pos = lv_pos + 9.
      lv_branch = lv_json+lv_pos.
      SHIFT lv_branch LEFT UP TO '"'.
      SHIFT lv_branch LEFT.
      FIND '"' IN lv_branch MATCH OFFSET lv_pos.
      IF sy-subrc = 0.
        lv_branch = lv_branch(lv_pos).
      ENDIF.
    ENDIF.

    " Parse username: find "username": "
    FIND '"username":' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_pos = lv_pos + 11.
      lv_username = lv_json+lv_pos.
      SHIFT lv_username LEFT UP TO '"'.
      SHIFT lv_username LEFT.
      FIND '"' IN lv_username MATCH OFFSET lv_pos.
      IF sy-subrc = 0.
        lv_username = lv_username(lv_pos).
      ENDIF.
    ENDIF.

    " Parse password: find "password": "
    FIND '"password":' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_pos = lv_pos + 11.
      lv_password = lv_json+lv_pos.
      SHIFT lv_password LEFT UP TO '"'.
      SHIFT lv_password LEFT.
      FIND '"' IN lv_password MATCH OFFSET lv_pos.
      IF sy-subrc = 0.
        lv_password = lv_password(lv_pos).
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

    " Use OO implementation
    DATA(lo_agent) = NEW zcl_abapgit_agent( ).
    DATA(ls_result) = lo_agent->zif_abapgit_agent~pull(
      iv_url      = lv_url
      iv_branch   = lv_branch
      iv_username = lv_username
      iv_password = lv_password ).

    " Build JSON response
    DATA(lv_success) = COND string( WHEN ls_result-success = abap_true THEN 'X' ELSE '' ).
    IF ls_result-success = abap_true.
      lv_json_resp = '{"success":"' && lv_success && '","job_id":"' &&
                   ls_result-job_id && '","message":"' && ls_result-message && '"}'.
    ELSE.
      lv_json_resp = '{"success":"' && lv_success && '","job_id":"' &&
                   ls_result-job_id && '","message":"' && ls_result-message &&
                   '","error_detail":"' && ls_result-error_detail && '"}'.
    ENDIF.

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
