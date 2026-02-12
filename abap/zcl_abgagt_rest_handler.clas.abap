*"*"use source
*"* Local Interface:
*"----------------------------------------------------------------------
CLASS zcl_abgagt_rest_handler DEFINITION PUBLIC INHERITING FROM cl_rest_http_handler CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: if_rest_application~get_root_handler REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_abgagt_rest_handler IMPLEMENTATION.

  METHOD if_rest_application~get_root_handler.
    DATA lo_router TYPE REF TO cl_rest_router.
    CREATE OBJECT lo_router.

    lo_router->attach( iv_template = '/pull' iv_handler_class = 'ZCL_ABGAGT_RESOURCE_PULL' ).
    lo_router->attach( iv_template = '/health' iv_handler_class = 'ZCL_ABGAGT_RESOURCE_HEALTH' ).
    lo_router->attach( iv_template = '/inspect' iv_handler_class = 'ZCL_ABGAGT_RESOURCE_INSPECT' ).
    lo_router->attach( iv_template = '/unit' iv_handler_class = 'ZCL_ABGAGT_RESOURCE_UNIT' ).
    lo_router->attach( iv_template = '/create' iv_handler_class = 'ZCL_ABGAGT_RESOURCE_CREATE' ).

    ro_root_handler = lo_router.
  ENDMETHOD.

ENDCLASS.
