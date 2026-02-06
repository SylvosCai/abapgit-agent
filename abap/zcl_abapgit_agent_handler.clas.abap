*"*"use source
*"* Local Interface:
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_handler DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor.

  PRIVATE SECTION.
    DATA mo_router TYPE REF TO cl_rest_router.

    METHODS: attach_routes.

ENDCLASS.

CLASS zcl_abapgit_agent_handler IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_router.
    attach_routes( ).
  ENDMETHOD.

  METHOD attach_routes.
    mo_router->attach( iv_template = '/pull' iv_handler_class = 'ZCL_ABAPGIT_AGENT_PULL' ).
    mo_router->attach( iv_template = '/status' iv_handler_class = 'ZCL_ABAPGIT_AGENT_STATUS' ).
    mo_router->attach( iv_template = '/health' iv_handler_class = 'ZCL_ABAPGIT_AGENT_HEALTH' ).
  ENDMETHOD.

ENDCLASS.
