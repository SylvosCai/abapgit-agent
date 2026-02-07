*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_syntax DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor.
    METHODS: if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zcl_abapgit_agent_syntax_agent.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_agent.
  ENDMETHOD.

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

    DATA ls_result TYPE zcl_abapgit_agent_syntax_agent=>ty_syntax_result.
    ls_result = mo_agent->syntax_check(
      iv_object_type = lv_object_type
      iv_object_name = lv_object_name ).

    DATA lv_success TYPE string.
    IF ls_result-success = abap_true.
      lv_success = 'X'.
    ELSE.
      lv_success = ''.
    ENDIF.

    DATA: BEGIN OF ls_response,
            success TYPE string,
            object_type TYPE string,
            object_name TYPE string,
            error_count TYPE i,
            errors TYPE zcl_abapgit_agent_syntax_agent=>ty_syntax_errors,
          END OF ls_response.

    ls_response-success = lv_success.
    ls_response-object_type = ls_result-object_type.
    ls_response-object_name = ls_result-object_name.
    ls_response-error_count = ls_result-error_count.
    ls_response-errors = ls_result-errors.

    lv_json_resp = /ui2/cl_json=>serialize( data = ls_response ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
