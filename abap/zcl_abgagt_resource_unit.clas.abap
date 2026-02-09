*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_unit DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor,
      if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_object,
             object_type TYPE string,
             object_name TYPE string,
           END OF ty_object.

    TYPES ty_object_list TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_request,
             package TYPE devclass,
             objects TYPE ty_object_list,
             files TYPE string_table,
           END OF ty_request.

    TYPES: BEGIN OF ty_result_item,
             object_name TYPE string,
             test_method TYPE string,
             status TYPE string,
             message TYPE string,
             passed TYPE abap_bool,
           END OF ty_result_item.

    TYPES ty_results TYPE STANDARD TABLE OF ty_result_item WITH NON-UNIQUE DEFAULT KEY.

    DATA mo_agent TYPE REF TO zcl_abgagt_agent.

ENDCLASS.

CLASS zcl_abgagt_resource_unit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_agent.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    DATA ls_request TYPE ty_request.

    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Deserialize JSON
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    DATA lv_json_resp TYPE string.

    " Parse files to objects if provided using agent API
    IF ls_request-files IS NOT INITIAL.
      LOOP AT ls_request-files INTO DATA(lv_file).
        DATA lv_obj_type TYPE string.
        DATA lv_obj_name TYPE string.
        mo_agent->parse_file_to_object(
          EXPORTING iv_file = lv_file
          IMPORTING ev_obj_type = lv_obj_type
                    ev_obj_name = lv_obj_name ).
        IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
          APPEND INITIAL LINE TO ls_request-objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
          <ls_obj>-object_type = lv_obj_type.
          <ls_obj>-object_name = lv_obj_name.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ls_request-package IS INITIAL AND ls_request-objects IS INITIAL.
      lv_json_resp = '{"success":"","message":"Package or objects required"}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Call run_tests method on main agent
    DATA ls_agent_result TYPE zif_abgagt_agent=>ty_unit_result.
    ls_agent_result = mo_agent->zif_abgagt_agent~run_tests(
      iv_package = ls_request-package
      it_objects = ls_request-objects ).

    " Build response matching CLI expectations
    DATA: BEGIN OF ls_response,
            success TYPE string,
            test_count TYPE i,
            passed_count TYPE i,
            failed_count TYPE i,
            message TYPE string,
            results TYPE zif_abgagt_agent=>ty_test_results,
          END OF ls_response.

    " Convert success
    IF ls_agent_result-success = abap_true.
      ls_response-success = 'X'.
    ENDIF.

    ls_response-test_count = ls_agent_result-test_count.
    ls_response-passed_count = ls_agent_result-passed_count.
    ls_response-failed_count = ls_agent_result-failed_count.
    ls_response-message = ls_agent_result-message.

    " Convert results
    LOOP AT ls_agent_result-results ASSIGNING FIELD-SYMBOL(<ls_result>).
      DATA ls_item TYPE zif_abgagt_agent=>ty_test_result.
      ls_item-object_name = <ls_result>-object_name.
      ls_item-test_method = <ls_result>-test_method.
      ls_item-status = <ls_result>-status.
      ls_item-message = <ls_result>-message.
      ls_item-line = <ls_result>-line.
      APPEND ls_item TO ls_response-results.
    ENDLOOP.

    lv_json_resp = /ui2/cl_json=>serialize( data = ls_response ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
