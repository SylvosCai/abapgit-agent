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

    " Parse JSON using /UI2/CL_JSON
    DATA: BEGIN OF ls_request,
            object_type TYPE string,
            object_name TYPE string,
          END OF ls_request.

    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_request ).

    DATA lv_object_type TYPE string.
    DATA lv_object_name TYPE string.
    lv_object_type = ls_request-object_type.
    lv_object_name = ls_request-object_name.

    DATA lv_json_resp TYPE string.
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

    " Build JSON response manually for reliability
    lv_json_resp = |{{"success":"{ lv_success }",|.
    lv_json_resp = |"object_type":"{ ls_result-object_type }",|.
    lv_json_resp = |"object_name":"{ ls_result-object_name }",|.
    lv_json_resp = |"error_count":{ lv_error_count },"errors":[|.

    DATA lv_first TYPE abap_bool VALUE abap_true.
    LOOP AT ls_result-errors ASSIGNING FIELD-SYMBOL(<ls_err>).
      IF lv_first = abap_false.
        lv_json_resp = lv_json_resp && ','.
      ENDIF.
      lv_first = abap_false.
      lv_json_resp = lv_json_resp && |{{"line":"{ <ls_err>-line }","column":"{ <ls_err>-column }","text":"{ <ls_err>-text }","word":"{ <ls_err>-word }"}}}|.
    ENDLOOP.

    lv_json_resp = lv_json_resp && ']}'.

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
