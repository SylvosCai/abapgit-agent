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
    " Dynamic routing: extract command from URL path
    " e.g., /sap/bc/z_abapgit_agent/health -> health
    " Then register the specific resource class dynamically
    DATA lo_router TYPE REF TO cl_rest_router.
    DATA lv_path TYPE string.
    DATA lt_parts TYPE TABLE OF string.
    DATA lv_command TYPE string.
    DATA lv_class_name TYPE seoclsname.

    CREATE OBJECT lo_router.

    " Get the current request path from the server
    lv_path = mo_server->request->get_header_field( name = '~path' ).

    " Extract command from path (last segment)
    SPLIT lv_path AT '/' INTO TABLE lt_parts.
    READ TABLE lt_parts INDEX lines( lt_parts ) INTO lv_command.

    " Build resource class name: ZCL_ABGAGT_RESOURCE_{COMMAND}
    lv_class_name = |ZCL_ABGAGT_RESOURCE_{ to_upper( lv_command ) }|.

    " Register the specific resource class
    lo_router->attach( iv_template = |/{ lv_command }| iv_handler_class = lv_class_name ).

    ro_root_handler = lo_router.
  ENDMETHOD.

ENDCLASS.
