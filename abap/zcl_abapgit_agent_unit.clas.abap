*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_unit DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor,
      if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zcl_abapgit_agent_unit_agent.

ENDCLASS.

CLASS zcl_abapgit_agent_unit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_agent.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    DATA lv_package TYPE devclass.
    DATA lv_json_resp TYPE string.
    DATA lo_entity TYPE REF TO cl_rest_entity.

    lv_json = mo_request->get_entity( )->get_string_data( ).
    lv_package = lv_json.

    lv_json_resp = mo_agent->run_tests( iv_package = lv_package ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
