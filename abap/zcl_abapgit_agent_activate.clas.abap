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
    DATA lv_package TYPE string.

    " Parse URL from JSON
    DATA lv_pos TYPE i.
    FIND '"url":' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      DATA lv_url_part TYPE string.
      lv_url_part = lv_json+lv_pos.
      SHIFT lv_url_part LEFT BY 6 PLACES.
      IF lv_url_part(1) = ' '.
        SHIFT lv_url_part LEFT.
      ENDIF.
      IF lv_url_part(1) = '"'.
        SHIFT lv_url_part LEFT.
        FIND '"' IN lv_url_part MATCH OFFSET DATA(lv_q).
        lv_url = lv_url_part(lv_q).
      ENDIF.
    ENDIF.

    " Parse package from JSON
    FIND '"package":' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      DATA lv_pkg_part TYPE string.
      lv_pkg_part = lv_json+lv_pos.
      SHIFT lv_pkg_part LEFT BY 10 PLACES.
      IF lv_pkg_part(1) = ' '.
        SHIFT lv_pkg_part LEFT.
      ENDIF.
      IF lv_pkg_part(1) = '"'.
        SHIFT lv_pkg_part LEFT.
        FIND '"' IN lv_pkg_part MATCH OFFSET DATA(lv_q2).
        lv_package = lv_pkg_part(lv_q2).
      ENDIF.
    ENDIF.

    " Validate required fields
    IF lv_url IS INITIAL AND lv_package IS INITIAL.
      DATA lv_json_resp TYPE string.
      lv_json_resp = '{"success":"","error":"URL or package is required"}'.
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
        iv_url     = lv_url
        iv_package = lv_package
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
