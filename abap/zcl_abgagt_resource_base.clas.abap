*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_base DEFINITION PUBLIC ABSTRACT
                           INHERITING FROM cl_rest_resource
                           CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS return_error
      IMPORTING iv_error TYPE string.

    METHODS return_success
      IMPORTING iv_result TYPE string.

ENDCLASS.

CLASS zcl_abgagt_resource_base IMPLEMENTATION.

  METHOD return_error.
    DATA lv_json_resp TYPE string.
    CONCATENATE
      '{"success":false,"error":"' iv_error '"}'
      INTO lv_json_resp.

    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
  ENDMETHOD.

  METHOD return_success.
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( iv_result ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
