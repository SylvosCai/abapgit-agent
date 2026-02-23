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
    " Dynamic routing: single catch-all handler that dynamically routes
    " to the appropriate resource class based on URL path
    " e.g., /pull -> ZCL_ABGAGT_RESOURCE_PULL
    "       /health -> ZCL_ABGAGT_RESOURCE_HEALTH
    DATA lo_router TYPE REF TO cl_rest_router.
    CREATE OBJECT lo_router.

    lo_router->attach( iv_template = '/{command}' iv_handler_class = 'ZCL_ABGAGT_RESOURCE_DYNAMIC' ).

    ro_root_handler = lo_router.
  ENDMETHOD.

ENDCLASS.
