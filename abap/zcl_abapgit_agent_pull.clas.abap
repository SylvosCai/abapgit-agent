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
      lv_i = 0.
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

    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
