*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_syntax_src DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor.
    METHODS: if_rest_resource~post REDEFINITION.

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zcl_abapgit_agent_src_agent.

    TYPES: BEGIN OF ty_request,
             source_code TYPE string,
           END OF ty_request.

    TYPES: BEGIN OF ty_error,
             line TYPE string,
             column TYPE string,
             text TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax_src IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_agent.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    DATA ls_request TYPE ty_request.

    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Parse JSON using /ui2/cl_json
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    DATA lv_json_resp TYPE string.
    DATA lv_source_code TYPE string.
    lv_source_code = ls_request-source_code.
    CONDENSE lv_source_code.

    IF lv_source_code IS INITIAL.
      lv_json_resp = '{"success":"","error_count":1,"errors":[{"line":"1","column":"1","text":"Source name is required"}]}'.
      DATA(lo_entity) = mo_response->create_entity( ).
      lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_entity->set_string_data( lv_json_resp ).
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    " Call syntax check agent
    DATA ls_result TYPE zcl_abapgit_agent_src_agent=>ty_result.
    ls_result = mo_agent->syntax_check_source( iv_source_code = lv_source_code ).

    " Convert success to 'X' or '' for JSON
    DATA lv_success TYPE string.
    IF ls_result-success = abap_true.
      lv_success = 'X'.
    ENDIF.

    " Build response structure
    DATA: BEGIN OF ls_response,
            success TYPE string,
            error_count TYPE i,
            errors TYPE ty_errors,
          END OF ls_response.

    ls_response-success = lv_success.
    ls_response-error_count = ls_result-error_count.
    IF ls_result-error_count > 0.
      APPEND VALUE #( line = ls_result-line
                      column = ls_result-column
                      text = ls_result-text ) TO ls_response-errors.
    ENDIF.

    " Serialize to JSON
    lv_json_resp = /ui2/cl_json=>serialize( data = ls_response ).

    lo_entity = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
