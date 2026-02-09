*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_unit DEFINITION PUBLIC FINAL
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
             files TYPE STANDARD TABLE OF string,
           END OF ty_request.

    TYPES: BEGIN OF ty_result_item,
             object_name TYPE string,
             test_method TYPE string,
             status TYPE string,
             message TYPE string,
             passed TYPE abap_bool,
           END OF ty_result_item.

    TYPES ty_results TYPE STANDARD TABLE OF ty_result_item WITH NON-UNIQUE DEFAULT KEY.

    DATA mo_agent TYPE REF TO zcl_abapgit_agent_unit_agent.

    METHODS parse_file_to_object
      IMPORTING
        iv_file TYPE string
      EXPORTING
        ev_obj_type TYPE string
        ev_obj_name TYPE string.

ENDCLASS.

CLASS zcl_abapgit_agent_unit IMPLEMENTATION.

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

    " Parse files to objects if provided
    IF ls_request-files IS NOT INITIAL.
      LOOP AT ls_request-files INTO DATA(lv_file).
        DATA lv_obj_type TYPE string.
        DATA lv_obj_name TYPE string.
        parse_file_to_object(
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

    " Call unit test agent
    DATA ls_agent_result TYPE zcl_abapgit_agent_unit_agent=>ty_result.
    ls_agent_result = mo_agent->run_tests(
      iv_package = ls_request-package
      it_objects = ls_request-objects ).

    " Build response matching CLI expectations
    DATA: BEGIN OF ls_response,
            success TYPE string,
            test_count TYPE i,
            passed_count TYPE i,
            failed_count TYPE i,
            message TYPE string,
            results TYPE ty_results,
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
      DATA ls_item TYPE ty_result_item.
      ls_item-object_name = <ls_result>-object_name.
      ls_item-test_method = <ls_result>-test_method.
      ls_item-status = <ls_result>-status.
      ls_item-message = <ls_result>-message.
      IF <ls_result>-status = 'PASSED'.
        ls_item-passed = abap_true.
      ELSE.
        ls_item-passed = abap_false.
      ENDIF.
      APPEND ls_item TO ls_response-results.
    ENDLOOP.

    lv_json_resp = /ui2/cl_json=>serialize( data = ls_response ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

  METHOD parse_file_to_object.
    " Parse file path to extract obj_type and obj_name
    " Example: "zcl_my_test.clas.abap" -> CLAS, ZCL_MY_TEST

    DATA lv_upper TYPE string.
    lv_upper = iv_file.
    TRANSLATE lv_upper TO UPPER CASE.

    " Split filename by '.' to get parts
    DATA lt_parts TYPE TABLE OF string.
    SPLIT lv_upper AT '.' INTO TABLE lt_parts.
    DATA lv_part_count TYPE i.
    lv_part_count = lines( lt_parts ).

    IF lv_part_count < 3.
      RETURN.
    ENDIF.

    " Last part should be 'ABAP' for verification
    READ TABLE lt_parts INDEX lv_part_count INTO DATA(lv_last).
    IF lv_last <> 'ABAP'.
      RETURN.
    ENDIF.

    " First part is obj_name (may contain path), second part is obj_type
    DATA lv_obj_name TYPE string.
    DATA lv_obj_type_raw TYPE string.
    READ TABLE lt_parts INDEX 1 INTO lv_obj_name.
    READ TABLE lt_parts INDEX 2 INTO lv_obj_type_raw.

    " Convert file extension to object type
    IF lv_obj_type_raw = 'CLASS'.
      ev_obj_type = 'CLAS'.
    ELSE.
      ev_obj_type = lv_obj_type_raw.
    ENDIF.

    " Extract file name from obj_name (remove path prefix)
    DATA lv_len TYPE i.
    lv_len = strlen( lv_obj_name ).
    DATA lv_offs TYPE i.
    lv_offs = find( val = reverse( lv_obj_name ) sub = '/' ).
    IF lv_offs > 0.
      DATA lv_start TYPE i.
      lv_start = lv_offs + 1.
      lv_obj_name = lv_obj_name+lv_start.
    ENDIF.

    " Remove leading '/' if present
    IF lv_obj_name(1) = '/'.
      lv_obj_name = lv_obj_name+1.
    ENDIF.

    ev_obj_name = lv_obj_name.
  ENDMETHOD.

ENDCLASS.
