*"*"use source
*"*"Local Interface:
*"**********************************************************************
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

    FIND '"url":' IN lv_json MATCH OFFSET DATA(lv_pos).
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

    DATA lv_json_resp TYPE string.
    IF lv_url IS INITIAL.
      lv_json_resp = '{"success":"","job_id":"","error":"URL is required"}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    DATA(lo_agent) = NEW zcl_abapgit_agent( ).
    DATA(ls_result) = lo_agent->zif_abapgit_agent~pull(
      iv_url      = lv_url
      iv_branch   = lv_branch
      iv_username = lv_username
      iv_password = lv_password ).

    DATA(lv_success) = COND string( WHEN ls_result-success = abap_true THEN 'X' ELSE '' ).

    " Build activated objects JSON array
    DATA(lv_activated_json) = '[]'.
    IF ls_result-activated_objects IS NOT INITIAL.
      DATA: ls_act TYPE zif_abapgit_agent=>ty_object.
      DATA(lv_activated_items) = ''.
      LOOP AT ls_result-activated_objects INTO ls_act.
        DATA(lv_item) = |{{"obj_type":"{ ls_act-obj_type }","obj_name":"{ ls_act-obj_name }","text":"{ ls_act-text }"}}|.
        IF lv_activated_items IS INITIAL.
          lv_activated_items = lv_item.
        ELSE.
          lv_activated_items = lv_activated_items && ',' && lv_item.
        ENDIF.
      ENDLOOP.
      CONCATENATE '[' lv_activated_items ']' INTO lv_activated_json.
    ENDIF.

    " Build failed objects JSON array
    DATA(lv_failed_json) = '[]'.
    IF ls_result-failed_objects IS NOT INITIAL.
      DATA: ls_fail TYPE zif_abapgit_agent=>ty_object.
      DATA(lv_failed_items) = ''.
      LOOP AT ls_result-failed_objects INTO ls_fail.
        DATA(lv_fail_item) = |{{"obj_type":"{ ls_fail-obj_type }","obj_name":"{ ls_fail-obj_name }","text":"{ ls_fail-text }"}}|.
        IF lv_failed_items IS INITIAL.
          lv_failed_items = lv_fail_item.
        ELSE.
          lv_failed_items = lv_failed_items && ',' && lv_fail_item.
        ENDIF.
      ENDLOOP.
      CONCATENATE '[' lv_failed_items ']' INTO lv_failed_json.
    ENDIF.

    " Build complete JSON response
    IF ls_result-success = abap_true.
      CONCATENATE '{"success":"' lv_success '","job_id":"' ls_result-job_id
                   '","message":"' ls_result-message
                   '","activated_count":' ls_result-activated_count
                   ',"failed_count":' ls_result-failed_count
                   ',"activated_objects":' lv_activated_json
                   ',"failed_objects":' lv_failed_json '}'
      INTO lv_json_resp.
    ELSE.
      CONCATENATE '{"success":"' lv_success '","job_id":"' ls_result-job_id
                   '","message":"' ls_result-message
                   '","error_detail":"' ls_result-error_detail
                   '","activated_count":' ls_result-activated_count
                   ',"failed_count":' ls_result-failed_count
                   ',"activated_objects":' lv_activated_json
                   ',"failed_objects":' lv_failed_json '}'
      INTO lv_json_resp.
    ENDIF.

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
