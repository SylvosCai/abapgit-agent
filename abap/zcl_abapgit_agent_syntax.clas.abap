*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_syntax DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax IMPLEMENTATION.

  METHOD if_rest_resource~post.
    DATA: lv_json TYPE string,
          lv_pos TYPE i,
          lv_object_type TYPE string,
          lv_object_name TYPE string.

    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Parse object_type from JSON
    FIND '"object_type":' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_pos = lv_pos + 14.
      lv_object_type = lv_json+lv_pos.
      SHIFT lv_object_type LEFT UP TO '"'.
      SHIFT lv_object_type LEFT.
      FIND '"' IN lv_object_type MATCH OFFSET lv_pos.
      IF sy-subrc = 0.
        lv_object_type = lv_object_type(lv_pos).
      ENDIF.
    ENDIF.

    " Parse object_name from JSON
    FIND '"object_name":' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_pos = lv_pos + 13.
      lv_object_name = lv_json+lv_pos.
      SHIFT lv_object_name LEFT UP TO '"'.
      SHIFT lv_object_name LEFT.
      FIND '"' IN lv_object_name MATCH OFFSET lv_pos.
      IF sy-subrc = 0.
        lv_object_name = lv_object_name(lv_pos).
      ENDIF.
    ENDIF.

    DATA: lv_json_resp TYPE string.
    IF lv_object_type IS INITIAL OR lv_object_name IS INITIAL.
      lv_json_resp = '{"success":"","object_type":"","object_name":"","error_count":1,"errors":[{"line":"1","column":"1","text":"Object type and name are required"}]}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    DATA(lo_agent) = NEW zcl_abapgit_agent( ).
    DATA(ls_result) = lo_agent->zif_abapgit_agent~syntax_check(
      iv_object_type = lv_object_type
      iv_object_name = lv_object_name ).

    DATA(lv_success) = COND string( WHEN ls_result-success = abap_true THEN 'X' ELSE '' ).
    DATA(lv_error_count) = ls_result-error_count.

    " Build JSON response manually
    CONCATENATE '{"success":"' lv_success '","object_type":"' ls_result-object_type '","object_name":"' ls_result-object_name '","error_count":' lv_error_count ',"errors":[' INTO lv_json_resp.

    DATA lv_first TYPE abap_bool VALUE abap_true.
    LOOP AT ls_result-errors ASSIGNING FIELD-SYMBOL(<ls_err>).
      IF lv_first = abap_false.
        CONCATENATE lv_json_resp ',' INTO lv_json_resp.
      ENDIF.
      lv_first = abap_false.
      DATA lv_err_json TYPE string.
      CONCATENATE '{"line":"' <ls_err>-line '","column":"' <ls_err>-column '","text":"' <ls_err>-text '","word":"' <ls_err>-word '"}' INTO lv_err_json.
      CONCATENATE lv_json_resp lv_err_json INTO lv_json_resp.
    ENDLOOP.

    CONCATENATE lv_json_resp ']}' INTO lv_json_resp.

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
