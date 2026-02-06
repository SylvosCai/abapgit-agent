*"*"use source
*"* Local Interface:
*"  IMPORTING
*"    REQUEST TYPE REF TO if_rest_request
*"  METHOD(if_rest_handler~handle_request)
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_health DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_rest_handler.

ENDCLASS.

CLASS zcl_abapgit_agent_health IMPLEMENTATION.

  METHOD if_rest_handler~handle_request.
    response->set_entity( response->create_entity( ) ).
    response->get_entity( )->set_content_type( 'application/json' ).
    response->get_entity( )->set_cdata( '{"status":"OK","version":"1.0"}' ).
    response->set_status( cl_rest_status_code=>ok ).
  ENDMETHOD.

ENDCLASS.
