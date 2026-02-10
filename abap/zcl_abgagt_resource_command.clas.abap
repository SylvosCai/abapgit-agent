*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_command DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_command IMPLEMENTATION.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Parse JSON request using /ui2/cl_json
    DATA: BEGIN OF ls_request,
            command TYPE string,
            url TYPE string,
            branch TYPE string,
            username TYPE string,
            password TYPE string,
            files TYPE string_table,
            package TYPE string,
            objects TYPE STANDARD TABLE OF string,
          END OF ls_request.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    DATA lv_json_resp TYPE string.

    " Validate command
    IF ls_request-command IS INITIAL.
      lv_json_resp = '{"success":"","error":"Command is required"}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Get command from factory
    DATA(lo_factory) = zcl_abgagt_cmd_factory=>get_instance( ).
    DATA(lo_command) = lo_factory->get_command( ls_request-command ).

    IF lo_command IS NOT BOUND.
      lv_json_resp = '{"success":"","error":"Unknown command: ' && ls_request-command && '"}'.
      lo_entity = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Build parameters based on command type
    DATA lt_files TYPE string_table.
    DATA lv_param_json TYPE string.

    CASE ls_request-command.
      WHEN zif_abgagt_command=>gc_pull.
        DATA: BEGIN OF ls_pull_params,
                url TYPE string,
                branch TYPE string,
                username TYPE string,
                password TYPE string,
                files TYPE string_table,
              END OF ls_pull_params.
        ls_pull_params-url = ls_request-url.
        ls_pull_params-branch = ls_request-branch.
        ls_pull_params-username = ls_request-username.
        ls_pull_params-password = ls_request-password.
        ls_pull_params-files = ls_request-files.
        lv_param_json = /ui2/cl_json=>serialize( data = ls_pull_params ).
        APPEND lv_param_json TO lt_files.

      WHEN zif_abgagt_command=>gc_inspect.
        DATA: BEGIN OF ls_inspect_params,
                source_name TYPE string,
              END OF ls_inspect_params.
        IF lines( ls_request-files ) > 0.
          READ TABLE ls_request-files INDEX 1 INTO ls_inspect_params-source_name.
        ENDIF.
        lv_param_json = /ui2/cl_json=>serialize( data = ls_inspect_params ).
        APPEND lv_param_json TO lt_files.

      WHEN zif_abgagt_command=>gc_unit.
        DATA: BEGIN OF ls_unit_params,
                package TYPE string,
                objects TYPE STANDARD TABLE OF string,
                files TYPE string_table,
              END OF ls_unit_params.
        ls_unit_params-package = ls_request-package.
        ls_unit_params-files = ls_request-files.
        ls_unit_params-objects = ls_request-objects.
        lv_param_json = /ui2/cl_json=>serialize( data = ls_unit_params ).
        APPEND lv_param_json TO lt_files.

      WHEN OTHERS.
        lv_json_resp = '{"success":"","error":"Unsupported command: ' && ls_request-command && '"}'.
        lo_entity = mo_response->create_entity( ).
        lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
        lo_entity->set_string_data( lv_json_resp ).
        mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
        RETURN.
    ENDCASE.

    " Execute command
    DATA(lv_result) = lo_command->execute( lt_files ).

    " Return result directly
    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_result ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
