*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_health DEFINITION PUBLIC FINAL
                               INHERITING FROM cl_rest_resource
                               CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~get REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_health IMPLEMENTATION.

  METHOD if_rest_resource~get.
    DATA lv_json TYPE string.
    lv_json = '{"status":"OK","version":"1.4.7"}'.

    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
