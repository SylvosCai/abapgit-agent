*"*"use source
*"* Local Interface:
*"----------------------------------------------------------------------
CLASS zcl_abapgit_agent_handler DEFINITION PUBLIC INHERITING FROM cl_rest_http_handler CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: if_rest_application~get_root_handler REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_abapgit_agent_handler IMPLEMENTATION.

  METHOD if_rest_application~get_root_handler.
    DATA lo_router TYPE REF TO cl_rest_router.
    CREATE OBJECT lo_router.

    lo_router->attach( iv_template = '/pull' iv_handler_class = 'ZCL_ABAPGIT_AGENT_PULL' ).
    lo_router->attach( iv_template = '/health' iv_handler_class = 'ZCL_ABAPGIT_AGENT_HEALTH' ).
    lo_router->attach( iv_template = '/syntax-check' iv_handler_class = 'ZCL_ABAPGIT_AGENT_SYNTAX' ).
    lo_router->attach( iv_template = '/syntax-check-source' iv_handler_class = 'ZCL_ABAPGIT_AGENT_SYNTAX_SRC' ).
    lo_router->attach( iv_template = '/unit' iv_handler_class = 'ZCL_ABAPGIT_AGENT_UNIT' ).

    ro_root_handler = lo_router.
  ENDMETHOD.

ENDCLASS.
