*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_base DEFINITION PUBLIC ABSTRACT
                           INHERITING FROM cl_rest_resource
                           CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS if_rest_resource~post REDEFINITION.
    METHODS if_rest_resource~get REDEFINITION.

  PROTECTED SECTION.

    METHODS get_command_constant ABSTRACT
      RETURNING VALUE(rv_constant) TYPE string.

    METHODS get_command_name ABSTRACT
      RETURNING VALUE(rv_name) TYPE string.

    METHODS create_request_data ABSTRACT
      RETURNING VALUE(rr_request_data) TYPE REF TO data.

    METHODS get_bg_config
      RETURNING VALUE(rs_config) TYPE zif_abgagt_bg_decision=>ty_bg_config.

    METHODS parse_request
      IMPORTING iv_json TYPE string
      EXPORTING es_request TYPE any.

    METHODS validate_request
      IMPORTING is_request TYPE data
      RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS get_error_message
      IMPORTING is_request TYPE data
      RETURNING VALUE(rv_message) TYPE string.

    METHODS return_error
      IMPORTING iv_error TYPE string.

    METHODS return_success
      IMPORTING iv_result TYPE string
                iv_http_status TYPE i DEFAULT cl_rest_status_code=>gc_success_ok.

    METHODS return_job_scheduled
      IMPORTING is_job_info TYPE zif_abgagt_bg_scheduler=>ty_job_info.

ENDCLASS.

CLASS zcl_abgagt_resource_base IMPLEMENTATION.

  METHOD if_rest_resource~post.
    " Get request body
    DATA lv_json TYPE string.
    DATA lr_request TYPE REF TO data.
    DATA lo_decision TYPE REF TO zif_abgagt_bg_decision.
    DATA lo_scheduler TYPE REF TO zif_abgagt_bg_scheduler.
    DATA lo_status_mgr TYPE REF TO zif_abgagt_job_status_mgr.
    DATA ls_job_info TYPE zif_abgagt_bg_scheduler=>ty_job_info.
    DATA ls_status TYPE zif_abgagt_job_status_mgr=>ty_job_status.
    DATA lv_timestamp TYPE timestamp.

    lv_json = mo_request->get_entity( )->get_string_data( ).
    lr_request = create_request_data( ).

    ASSIGN lr_request->* TO FIELD-SYMBOL(<ls_request>).

    TRY.
        " Parse request
        parse_request(
          EXPORTING iv_json = lv_json
          IMPORTING es_request = <ls_request> ).

        " Validate request
        IF validate_request( <ls_request> ) = abap_false.
          return_error( get_error_message( <ls_request> ) ).
          RETURN.
        ENDIF.

        " Get command from factory
        DATA(lo_factory) = zcl_abgagt_cmd_factory=>get_instance( ).
        DATA(lv_constant) = get_command_constant( ).
        DATA(lo_command) = lo_factory->get_command( lv_constant ).

        IF lo_command IS NOT BOUND.
          return_error( get_command_name( ) && ' command not found' ).
          RETURN.
        ENDIF.

        " Decision: Should run in background?
        " ========================================
        CREATE OBJECT lo_decision TYPE zcl_abgagt_bg_decision.

        DATA(lv_run_in_bg) = lo_decision->should_run_in_background(
          io_command      = lo_command
          is_request_data = <ls_request>
          is_config       = get_bg_config( )
        ).

        IF lv_run_in_bg = abap_true.
          " Background Execution Path
          " ========================================
          CREATE OBJECT lo_scheduler TYPE zcl_abgagt_bg_scheduler.

          ls_job_info = lo_scheduler->schedule_command(
            iv_command_type = lv_constant
            is_command_data = <ls_request>
          ).

          " Initialize job status immediately after scheduling
          CREATE OBJECT lo_status_mgr TYPE zcl_abgagt_bg_status_mgr.
          GET TIME STAMP FIELD lv_timestamp.

          CLEAR ls_status.
          ls_status-job_name    = ls_job_info-job_name.
          ls_status-job_number  = ls_job_info-job_number.
          ls_status-status      = 'scheduled'.
          ls_status-stage       = 'INITIALIZATION'.
          ls_status-message     = 'Job scheduled, waiting to start'.
          ls_status-progress    = 0.
          ls_status-started_at  = lv_timestamp.
          ls_status-updated_at  = lv_timestamp.
          lo_status_mgr->update_status( ls_status ).

          " Return 202 Accepted with job info
          return_job_scheduled( ls_job_info ).

        ELSE.
          " ========================================
          " Synchronous Execution Path
          " ========================================
          DATA(lv_result) = lo_command->execute( is_param = <ls_request> ).
          return_success( lv_result ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_exception).
        return_error( lx_exception->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD parse_request.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = iv_json
      CHANGING
        data = es_request ).

  ENDMETHOD.

  METHOD validate_request.
    " Default implementation - to be overridden by subclass
    rv_valid = abap_true.
  ENDMETHOD.

  METHOD get_error_message.
    " Default implementation - to be overridden by subclass
  ENDMETHOD.

  METHOD return_error.
    DATA lv_json_resp TYPE string.

    DATA(lv_command_name) = get_command_name( ).
    CONCATENATE
      '{"success":false,"command":"' lv_command_name '","error":"' iv_error '"}'
      INTO lv_json_resp.

    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
  ENDMETHOD.

  METHOD return_success.
    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( iv_result ).
    mo_response->set_status( iv_http_status ).
  ENDMETHOD.

  METHOD get_bg_config.
    " Default configuration - subclasses can override
    " to force background or synchronous execution
    CLEAR rs_config.
  ENDMETHOD.

  METHOD return_job_scheduled.
    " Return HTTP 202 Accepted with job information
    DATA lv_json_resp TYPE string.
    DATA lv_job_number_str TYPE string.
    DATA(lv_command_name) = get_command_name( ).

    " Convert job number to string with leading zeros
    lv_job_number_str = is_job_info-job_number.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_job_number_str
      IMPORTING
        output = lv_job_number_str.

    lv_json_resp = '{"success":true,' &&
                   '"command":"' && lv_command_name && '",' &&
                   '"status":"accepted",' &&
                   '"jobName":"' && is_job_info-job_name && '",' &&
                   '"jobNumber":"' && lv_job_number_str && '",' &&
                   '"message":"Command scheduled for background execution"' &&
                   '}'.

    DATA(lo_entity) = mo_response->create_entity( ).
    lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_entity->set_string_data( lv_json_resp ).
    mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).
  ENDMETHOD.

  METHOD if_rest_resource~get.
    " Handle job status queries via GET with jobNumber parameter
    DATA lv_job_number TYPE btcjobcnt.
    DATA lo_status_mgr TYPE REF TO zif_abgagt_job_status_mgr.
    DATA ls_status TYPE zif_abgagt_job_status_mgr=>ty_job_status.
    DATA lv_json_resp TYPE string.

    TRY.
        " Get jobNumber from query parameter
        DATA(lo_request) = mo_request.
        DATA(lv_query) = lo_request->get_uri_query_parameter( iv_name = 'jobNumber' ).

        IF lv_query IS INITIAL.
          return_error( 'jobNumber query parameter is required' ).
          RETURN.
        ENDIF.

        lv_job_number = lv_query.

        " Get status from status manager
        CREATE OBJECT lo_status_mgr TYPE zcl_abgagt_bg_status_mgr.
        ls_status = lo_status_mgr->get_status( lv_job_number ).

        IF ls_status-job_number IS INITIAL.
          return_error( 'Job not found' ).
          RETURN.
        ENDIF.

        " Serialize status to JSON
        /ui2/cl_json=>serialize(
          EXPORTING
            data        = ls_status
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          RECEIVING
            r_json      = lv_json_resp
        ).

        " Return status
        DATA(lo_entity) = mo_response->create_entity( ).
        lo_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
        lo_entity->set_string_data( lv_json_resp ).
        mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH cx_root INTO DATA(lx_error).
        return_error( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
