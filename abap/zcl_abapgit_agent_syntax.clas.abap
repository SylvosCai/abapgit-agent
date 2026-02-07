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

    TYPES: BEGIN OF ty_request,
             object_type TYPE string,
             object_name TYPE string,
           END OF ty_request.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_agent.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    DATA ls_request TYPE ty_request.
    DATA lv_json_resp TYPE string.
    DATA lo_response_entity TYPE REF TO cl_rest_entity.

    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Parse JSON using /UI2/CL_JSON
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    IF ls_request-object_type IS INITIAL OR ls_request-object_name IS INITIAL.
      lv_json_resp = '{"success":"","object_type":"","object_name":"","error_count":1,"errors":[{"line":"1","column":"1","text":"Object type and name are required"}]}'.
      lo_response_entity = mo_response->create_entity( ).
      lo_response_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_response_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Call syntax check agent - returns JSON string directly
    lv_json_resp = mo_agent->syntax_check(
      iv_object_type = ls_request-object_type
      iv_object_name = ls_request-object_name ).

    lo_response_entity = mo_response->create_entity( ).
    lo_response_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
