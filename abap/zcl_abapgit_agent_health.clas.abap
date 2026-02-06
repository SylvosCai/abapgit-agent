*"*"use source
*"*"Local Interface:
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_health DEFINITION PUBLIC FINAL
                               INHERITING FROM cl_rest_resource
                               CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~get REDEFINITION.

ENDCLASS.

CLASS zcl_abapgit_agent_health IMPLEMENTATION.

  METHOD if_rest_resource~get.
    DATA lv_json TYPE string.
    lv_json = '{"status":"OK","version":"1.0.0"}'.

    mo_response->set_entity( mo_response->create_entity( ) ).
    mo_response->get_entity( )->set_content_type( 'application/json' ).
    mo_response->get_entity( )->set_cdata( lv_json ).
    mo_response->set_status( cl_rest_status_code=>ok ).
  ENDMETHOD.

ENDCLASS.
