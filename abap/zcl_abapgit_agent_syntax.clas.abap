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
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    DATA lv_object_type TYPE string.
    DATA lv_object_name TYPE string.
    DATA lv_pos TYPE i.

    FIND 'object_type' IN lv_json MATCH OFFSET lv_pos.
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

    FIND 'object_name' IN lv_json MATCH OFFSET lv_pos.
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

    DATA lv_json_resp TYPE string.
    IF lv_object_type IS INITIAL OR lv_object_name IS INITIAL.
      lv_json_resp = '{"success":"","object_type":"","object_name":"","error_count":1,"errors":[{"line":"1","column":"1","text":"Object type and name are required"}]}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    DATA lo_agent TYPE REF TO zcl_abapgit_agent.
    CREATE OBJECT lo_agent.

    DATA ls_result TYPE zif_abapgit_agent=>ty_syntax_result.
    ls_result = lo_agent->zif_abapgit_agent~syntax_check(
      iv_object_type = lv_object_type
      iv_object_name = lv_object_name ).

    DATA lv_success TYPE string.
    IF ls_result-success = abap_true.
      lv_success = 'X'.
    ELSE.
      lv_success = ''.
    ENDIF.

    DATA lv_error_count TYPE string.
    lv_error_count = ls_result-error_count.

    " Build JSON response with errors
    DATA lv_errors_json TYPE string.
    lv_errors_json = '['.

    DATA lv_first TYPE abap_bool VALUE abap_true.
    LOOP AT ls_result-errors ASSIGNING FIELD-SYMBOL(<ls_err>).
      IF lv_first = abap_false.
        CONCATENATE lv_errors_json ',' INTO lv_errors_json.
      ENDIF.
      lv_first = abap_false.
      DATA lv_err_json TYPE string.
      CONCATENATE '{"line":"' <ls_err>-line '","column":"' <ls_err>-column '","text":"' <ls_err>-text '","word":"' <ls_err>-word '"}' INTO lv_err_json.
      CONCATENATE lv_errors_json lv_err_json INTO lv_errors_json.
    ENDLOOP.

    CONCATENATE lv_errors_json ']' INTO lv_errors_json.

    CONCATENATE '{"success":"' lv_success '","object_type":"' lv_object_type '","object_name":"' lv_object_name '","error_count":' lv_error_count ',"errors":' lv_errors_json '}' INTO lv_json_resp.

    DATA lo_response_entity TYPE REF TO if_rest_entity.
    lo_response_entity = mo_response->create_entity( ).
    lo_response_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
