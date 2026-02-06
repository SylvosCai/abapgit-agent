*"*"use source
*"*"Local Interface:
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_pull DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    METHODS extract_json_field
      IMPORTING
        iv_json TYPE string
        iv_field TYPE string
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
    lv_url = extract_json_field( iv_json = lv_json iv_field = 'url' ).
    lv_branch = extract_json_field( iv_json = lv_json iv_field = 'branch' ).
    lv_username = extract_json_field( iv_json = lv_json iv_field = 'username' ).
    lv_password = extract_json_field( iv_json = lv_json iv_field = 'password' ).

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

  METHOD extract_json_field.
    DATA lv_search TYPE string.
    DATA lv_pos TYPE i.
    DATA lv_len TYPE i.

    " Build search string for field name
    CONCATENATE '"' iv_field '"' ':' INTO lv_search.
    FIND lv_search IN iv_json MATCH OFFSET lv_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Get substring after field
    DATA lv_rest TYPE string.
    lv_rest = iv_json+lv_pos.

    " Find opening quote
    FIND '"' IN lv_rest MATCH OFFSET lv_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Get length of rest
    lv_len = strlen( lv_rest ).
    DATA(lv_skip) = lv_pos + 1.
    DATA(lv_remaining) = lv_len - lv_skip.

    " Get the part after opening quote
    DATA lv_after TYPE string.
    lv_after = lv_rest+lv_skip(lv_remaining).

    " Find closing quote
    FIND '"' IN lv_after MATCH OFFSET lv_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Extract the value
    rv_value = lv_after.
    DATA lv_cut TYPE i.
    lv_cut = strlen( lv_after ) - lv_pos.
    SHIFT rv_value RIGHT BY lv_cut PLACES.
    SHIFT rv_value LEFT.
  ENDMETHOD.

ENDCLASS.
