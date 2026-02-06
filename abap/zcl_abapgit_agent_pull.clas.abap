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

    " Extract URL from JSON
    FIND REGEX '"url"\s*:\s*"([^"]*)"' IN lv_json SUBMATCHES lv_url.
    IF lv_url IS INITIAL.
      FIND REGEX '"url"\s*:\s*([^\s,}]+)' IN lv_json SUBMATCHES lv_url.
      SHIFT lv_url LEFT DELETING LEADING '"'.
    ENDIF.

    " Extract branch from JSON
    FIND REGEX '"branch"\s*:\s*"([^"]*)"' IN lv_json SUBMATCHES lv_branch.
    IF lv_branch IS INITIAL.
      FIND REGEX '"branch"\s*:\s*([^\s,}]+)' IN lv_json SUBMATCHES lv_branch.
      SHIFT lv_branch LEFT DELETING LEADING '"'.
    ENDIF.

    " Extract username from JSON
    FIND REGEX '"username"\s*:\s*"([^"]*)"' IN lv_json SUBMATCHES lv_username.
    IF lv_username IS INITIAL.
      FIND REGEX '"username"\s*:\s*([^\s,}]+)' IN lv_json SUBMATCHES lv_username.
      SHIFT lv_username LEFT DELETING LEADING '"'.
    ENDIF.

    " Extract password from JSON
    FIND REGEX '"password"\s*:\s*"([^"]*)"' IN lv_json SUBMATCHES lv_password.
    IF lv_password IS INITIAL.
      FIND REGEX '"password"\s*:\s*([^\s,}]+)' IN lv_json SUBMATCHES lv_password.
      SHIFT lv_password LEFT DELETING LEADING '"'.
    ENDIF.

    IF lv_branch IS INITIAL.
      lv_branch = 'main'.
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

    DATA lv_json_resp TYPE string.
    lv_json_resp = '{"success":"' && lv_success && '","job_id":"' &&
                 lv_job_id && '","message":"' && lv_msg && '"}'.

    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
