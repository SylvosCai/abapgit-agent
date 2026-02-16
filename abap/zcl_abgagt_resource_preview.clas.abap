*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_preview DEFINITION PUBLIC FINAL
                              INHERITING FROM cl_rest_resource
                              CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_preview IMPLEMENTATION.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Parse JSON using /ui2/cl_json
    DATA: BEGIN OF ls_request,
            objects TYPE string_table,
            type TYPE string,
            limit TYPE i,
          END OF ls_request.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    DATA lv_json_resp TYPE string.

    IF ls_request-objects IS INITIAL.
      lv_json_resp = '{"success":false,"command":"PREVIEW","error":"Objects parameter is required"}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Get command from factory
    DATA(lo_factory) = zcl_abgagt_cmd_factory=>get_instance( ).
    DATA(lo_command) = lo_factory->get_command( zif_abgagt_command=>gc_preview ).

    IF lo_command IS NOT BOUND.
      lv_json_resp = '{"success":false,"command":"PREVIEW","error":"PREVIEW command not found"}'.
      lo_entity = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Execute command with is_param
    DATA ls_params TYPE zcl_abgagt_command_preview=>ty_preview_params.
    ls_params-objects = ls_request-objects.
    ls_params-type = ls_request-type.
    ls_params-limit = ls_request-limit.

    DATA(lv_result) = lo_command->execute( is_param = ls_params ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_result ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
