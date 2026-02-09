*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_inspect DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor.
    METHODS: if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zcl_abgagt_agent.

    TYPES: BEGIN OF ty_request,
             source_name TYPE string,
           END OF ty_request.

ENDCLASS.

CLASS zcl_abgagt_resource_inspect IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_agent.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    DATA ls_request TYPE ty_request.

    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Deserialize JSON using /ui2/cl_json
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    DATA lv_json_resp TYPE string.
    IF ls_request-source_name IS INITIAL.
      lv_json_resp = '{"success":"","object_type":"","object_name":"","error_count":1,"errors":[{"line":"1","column":"1","text":"Source name is required"}]}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Call inspect method on main agent (passes file name, internally parses)
    DATA ls_result TYPE zif_abgagt_agent=>ty_inspect_result.
    ls_result = mo_agent->zif_abgagt_agent~inspect(
      iv_file = ls_request-source_name ).

    " Convert success to 'X' or '' for JSON
    DATA lv_success TYPE string.
    IF ls_result-success = abap_true.
      lv_success = 'X'.
    ENDIF.

    " Build response structure
    DATA: BEGIN OF ls_response,
            success TYPE string,
            object_type TYPE string,
            object_name TYPE string,
            error_count TYPE i,
            errors TYPE zif_abgagt_agent=>ty_errors,
          END OF ls_response.

    ls_response-success = lv_success.
    ls_response-object_type = ls_result-object_type.
    ls_response-object_name = ls_result-object_name.
    ls_response-error_count = ls_result-error_count.
    ls_response-errors = ls_result-errors.

    " Serialize to JSON using /ui2/cl_json
    lv_json_resp = /ui2/cl_json=>serialize( data = ls_response ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
