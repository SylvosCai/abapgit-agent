*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_unit DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor.
    METHODS: if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zcl_abapgit_agent_unit_agent.

    TYPES: BEGIN OF ty_object,
             object_type TYPE string,
             object_name TYPE string,
           END OF ty_object.

    TYPES ty_object_list TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_request,
             package TYPE devclass,
             objects TYPE ty_object_list,
           END OF ty_request.

ENDCLASS.

CLASS zcl_abapgit_agent_unit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_agent.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    DATA ls_request TYPE ty_request.
    DATA lv_json_resp TYPE string.
    DATA lo_entity TYPE REF TO cl_rest_entity.

    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Parse JSON using /ui2/cl_json
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    " Validate request
    IF ls_request-package IS INITIAL AND ls_request-objects IS INITIAL.
      lv_json_resp = '{"success":"","test_count":0,"passed_count":0,"failed_count":0,"message":"Package or objects required"}'.
      lo_entity = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Call unit test agent
    DATA ls_result TYPE zcl_abapgit_agent_unit_agent=>ty_result.
    ls_result = mo_agent->run_tests(
      iv_package = ls_request-package
      it_objects = ls_request-objects ).

    " Convert success to 'X' or '' for JSON
    DATA lv_success TYPE string.
    IF ls_result-success = abap_true.
      lv_success = 'X'.
    ENDIF.

    " Build response structure
    DATA: BEGIN OF ls_response,
            success TYPE string,
            test_count TYPE i,
            passed_count TYPE i,
            failed_count TYPE i,
            message TYPE string,
            results TYPE zcl_abapgit_agent_unit_agent=>ty_test_results,
          END OF ls_response.

    ls_response-success = lv_success.
    ls_response-test_count = ls_result-test_count.
    ls_response-passed_count = ls_result-passed_count.
    ls_response-failed_count = ls_result-failed_count.
    ls_response-message = ls_result-message.
    ls_response-results = ls_result-results.

    " Serialize to JSON using /ui2/cl_json
    lv_json_resp = /ui2/cl_json=>serialize( data = ls_response ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
