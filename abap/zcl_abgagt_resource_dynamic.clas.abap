*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_dynamic DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

  PROTECTED SECTION.
    METHODS get_resource_class_name
      IMPORTING iv_command       TYPE string
      RETURNING VALUE(rv_class) TYPE string.

    METHODS create_command_request
      IMPORTING iv_command TYPE string
      RETURNING VALUE(rr_data) TYPE REF TO data.

    METHODS handle_request
      IMPORTING iv_command TYPE string
                iv_json    TYPE string.

ENDCLASS.

CLASS zcl_abgagt_resource_dynamic IMPLEMENTATION.

  METHOD if_rest_resource~post.
    " Extract command from URL path
    DATA lv_path TYPE string.
    lv_path = mo_server->m_path_translated.

    " Path format: /sap/bc/z_abapgit_agent/{command}
    " Extract command from path
    SPLIT lv_path AT '/' INTO TABLE DATA(lt_parts).
    READ TABLE lt_parts INDEX lines( lt_parts ) INTO DATA(lv_command).

    IF lv_command IS INITIAL.
      return_error( 'Command not specified in URL path' ).
      RETURN.
    ENDIF.

    " Get request body
    DATA(lv_json) = mo_request->get_entity( )->get_string_data( ).

    " Handle the request
    handle_request( iv_command = lv_command iv_json = lv_json ).
  ENDMETHOD.

  METHOD handle_request.
    " Build resource class name: ZCL_ABGAGT_RESOURCE_{COMMAND}
    DATA(lv_class_name) = get_resource_class_name( iv_command ).

    " Try to instantiate the specific resource class
    DATA lo_specific_resource TYPE REF TO cl_rest_resource.

    TRY.
        CREATE OBJECT lo_specific_resource TYPE (lv_class_name).
      CATCH cx_sy_create_object_error.
        return_error( |Command '{ iv_command }' not supported| ).
        RETURN.
    ENDTRY.

    " Set request and response on the specific resource
    lo_specific_resource->set_request( mo_request ).
    lo_specific_resource->set_response( mo_response ).

    " Call POST method on the specific resource
    lo_specific_resource->if_rest_resource~post( ).
  ENDMETHOD.

  METHOD get_resource_class_name.
    " Convert command to uppercase and build class name
    " e.g., pull -> ZCL_ABGAGT_RESOURCE_PULL
    DATA lv_upper_command TYPE string.
    lv_upper_command = to_upper( iv_command ).

    CONCATENATE 'ZCL_ABGAGT_RESOURCE_' lv_upper_command INTO rv_class.
  ENDMETHOD.

  METHOD create_command_request.
    " This method can be extended to create specific request data types
    " based on command if needed. For now, returns generic data reference.
    " The specific resource classes handle their own request parsing.
    RETURN.
  ENDMETHOD.

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

ENDCLASS.
