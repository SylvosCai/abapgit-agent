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
    super->constructor( ).
    CREATE OBJECT mo_agent.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    DATA lv_object_type TYPE string.
    DATA lv_object_name TYPE string.
    DATA lv_pos TYPE i.

    " Parse object_type from JSON: "object_type":"<value>"
    FIND '"object_type":"' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_pos = lv_pos + 15.
      lv_object_type = lv_json+lv_pos.
      SHIFT lv_object_type LEFT UP TO '"'.
      IF sy-subrc = 0.
        lv_object_type = lv_object_type+1.
        FIND '"' IN lv_object_type MATCH OFFSET lv_pos.
        IF sy-subrc = 0.
          lv_object_type = lv_object_type(lv_pos).
        ENDIF.
      ENDIF.
    ENDIF.

    " Parse object_name from JSON: "object_name":"<value>"
    FIND '"object_name":"' IN lv_json MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_pos = lv_pos + 14.
      lv_object_name = lv_json+lv_pos.
      SHIFT lv_object_name LEFT UP TO '"'.
      IF sy-subrc = 0.
        lv_object_name = lv_object_name+1.
        FIND '"' IN lv_object_name MATCH OFFSET lv_pos.
        IF sy-subrc = 0.
          lv_object_name = lv_object_name(lv_pos).
        ENDIF.
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

    " Call syntax check agent - returns JSON string directly
    lv_json_resp = mo_agent->syntax_check(
      iv_object_type = lv_object_type
      iv_object_name = lv_object_name ).

    DATA(lo_response_entity) = mo_response->create_entity( ).
    lo_response_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
