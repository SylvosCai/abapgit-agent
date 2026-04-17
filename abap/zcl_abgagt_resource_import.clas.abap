*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_import DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PUBLIC SECTION.
    " Override GET to handle job status polling
    METHODS if_rest_resource~get REDEFINITION.

  PROTECTED SECTION.
    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.
    METHODS get_error_message REDEFINITION.

    " Override to return HTTP 202 Accepted for async job
    METHODS return_success REDEFINITION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_import_request,
             url TYPE string,
             branch TYPE string,
             message TYPE string,
             username TYPE string,
             password TYPE string,
           END OF ty_import_request.

    METHODS get_job_status
      IMPORTING
        iv_job_number TYPE btcjobcnt
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS zcl_abgagt_resource_import IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_import.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Import'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE ty_import_request.
  ENDMETHOD.

  METHOD validate_request.
    DATA: ls_request TYPE ty_import_request.
    ls_request = is_request.
    rv_valid = xsdbool( ls_request-url IS NOT INITIAL ).
  ENDMETHOD.

  METHOD get_error_message.
    DATA: ls_request TYPE ty_import_request.
    ls_request = is_request.
    IF ls_request-url IS INITIAL.
      rv_message = 'URL is required'.
    ENDIF.
  ENDMETHOD.

  METHOD return_success.
    " Override to return HTTP 202 Accepted (async job started)
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( iv_result ).
    mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).
  ENDMETHOD.

  METHOD if_rest_resource~get.
    " Get job number from query parameter
    DATA: lv_job_num_str TYPE string,
          lv_job_number TYPE btcjobcnt,
          lv_result TYPE string.

    TRY.
        lv_job_num_str = mo_request->get_uri_query_parameter( 'jobNumber' ).
        lv_job_number = lv_job_num_str.

        IF lv_job_number IS INITIAL.
          lv_result = '{"success":"","error":"jobNumber parameter required"}'.
          mo_response->create_entity( )->set_string_data( lv_result ).
          mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
          RETURN.
        ENDIF.

        " Get job status
        lv_result = get_job_status( lv_job_number ).

        " Return status
        mo_response->create_entity( )->set_string_data( lv_result ).
        mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH cx_root INTO DATA(lx_error).
        lv_result = '{"success":"","error":"' && lx_error->get_text( ) && '"}'.
        mo_response->create_entity( )->set_string_data( lv_result ).
        mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_job_status.
    DATA: ls_status TYPE zif_abgagt_job_status_mgr=>ty_job_status,
          lo_mgr    TYPE REF TO zif_abgagt_job_status_mgr.

    TRY.
        " Get status from shared buffer using interface
        lo_mgr = NEW zcl_abgagt_bg_status_mgr( ).
        ls_status = lo_mgr->get_status( iv_job_number ).

        IF ls_status IS INITIAL.
          " No status in memory - check if job exists
          rv_result = '{"success":"","error":"Job not found"}'.
          RETURN.
        ENDIF.

        " Convert status to JSON
        rv_result = /ui2/cl_json=>serialize(
          data          = ls_status
          compress      = abap_false
          pretty_name   = /ui2/cl_json=>pretty_mode-camel_case ).

      CATCH cx_root INTO DATA(lx_error).
        rv_result = '{"success":"","error":"' && lx_error->get_text( ) && '"}'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
