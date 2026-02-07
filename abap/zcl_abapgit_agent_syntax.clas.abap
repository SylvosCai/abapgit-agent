*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_syntax DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor.
    METHODS: if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zcl_abapgit_agent_syntax_agent.

    TYPES: BEGIN OF ty_request,
             object_type TYPE string,
             object_name TYPE string,
           END OF ty_request.

    TYPES: BEGIN OF ty_error,
             line TYPE string,
             column TYPE string,
             text TYPE string,
             word TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_response,
             success TYPE string,
             object_type TYPE string,
             object_name TYPE string,
             error_count TYPE i,
             errors TYPE ty_errors,
           END OF ty_response.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_agent.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    DATA ls_request TYPE ty_request.
    DATA ls_response TYPE ty_response.
    DATA lv_json_resp TYPE string.
    DATA ls_error TYPE ty_error.

    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Parse JSON using /UI2/CL_JSON with CHANGING parameter
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    IF ls_request-object_type IS INITIAL OR ls_request-object_name IS INITIAL.
      ls_response-success = ''.
      ls_response-object_type = ls_request-object_type.
      ls_response-object_name = ls_request-object_name.
      ls_response-error_count = 1.
      ls_error-line = '1'.
      ls_error-column = '1'.
      ls_error-text = 'Object type and name are required'.
      ls_error-word = ''.
      APPEND ls_error TO ls_response-errors.
      lv_json_resp = /ui2/cl_json=>serialize( data = ls_response ).
      io_entity = mo_response->create_entity( ).
      io_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      io_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Call syntax check agent - returns JSON string directly
    lv_json_resp = mo_agent->syntax_check(
      iv_object_type = ls_request-object_type
      iv_object_name = ls_request-object_name ).

    io_entity = mo_response->create_entity( ).
    io_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    io_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
