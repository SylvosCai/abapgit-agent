*"*"use source
*"*"Local Interface:
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_pull DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    METHODS get_json_value
      IMPORTING
        iv_json TYPE string
        iv_key TYPE string
      RETURNING
        VALUE(rv_value) TYPE string.

ENDCLASS.

CLASS zcl_abapgit_agent_pull IMPLEMENTATION.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    DATA lv_url TYPE string.
    DATA lv_branch TYPE string.
    DATA lv_username TYPE string.
    DATA lv_password TYPE string.

    " Extract fields from JSON
    lv_url = get_json_value( iv_json = lv_json iv_key = 'url' ).
    lv_branch = get_json_value( iv_json = lv_json iv_key = 'branch' ).
    lv_username = get_json_value( iv_json = lv_json iv_key = 'username' ).
    lv_password = get_json_value( iv_json = lv_json iv_key = 'password' ).

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

  METHOD get_json_value.
    DATA lv_key TYPE string.
    DATA lv_pos TYPE i.
    DATA lv_len TYPE i.
    DATA lv_after_len TYPE i.
    DATA lv_after TYPE string.
    DATA lv_quote TYPE i.

    " Build key pattern
    CONCATENATE '"' iv_key '"' ':' INTO lv_key.

    " Find the key in JSON
    FIND lv_key IN iv_json MATCH OFFSET lv_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Get the part after the key
    lv_len = strlen( iv_json ).
    lv_after_len = lv_len - lv_pos.
    lv_after = iv_json+lv_pos(lv_after_len).

    " Find first quote after key
    FIND '"' IN lv_after MATCH OFFSET lv_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Get substring after first quote
    lv_after = lv_after+lv_pos+1.

    " Find closing quote
    FIND '"' IN lv_after MATCH OFFSET lv_quote.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_value = lv_after(lv_quote).
  ENDMETHOD.

ENDCLASS.
