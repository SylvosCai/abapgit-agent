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
    " Dynamic routing: extract command from URL path (mo_server->m_path_translated)
    " e.g., /sap/bc/z_abapgit_agent/health -> health
    " Then register the specific resource class dynamically
    DATA lo_router TYPE REF TO cl_rest_router.
    CREATE OBJECT lo_router.

    " Get the current request path from the server
    DATA lv_path TYPE string.
    lv_path = mo_server->m_path_translated.

    " Extract command from path (last segment)
    SPLIT lv_path AT '/' INTO TABLE DATA(lt_parts).
    READ TABLE lt_parts INDEX lines( lt_parts ) INTO DATA(lv_command).

    " Build resource class name: ZCL_ABGAGT_RESOURCE_{COMMAND}
    DATA lv_class_name TYPE string.
    lv_class_name = |ZCL_ABGAGT_RESOURCE_{ to_upper( lv_command ) }|.

    " Register the specific resource class
    lo_router->attach( iv_template = |/{ lv_command }| iv_handler_class = lv_class_name ).

    ro_root_handler = lo_router.
  ENDMETHOD.

ENDCLASS.
