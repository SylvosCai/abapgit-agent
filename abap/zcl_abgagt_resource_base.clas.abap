*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_base DEFINITION ABSTRACT PUBLIC
                           INHERITING FROM cl_rest_resource
                           CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS if_rest_resource~post REDEFINITION.

  PROTECTED SECTION.

    DATA ms_request TYPE any.

    METHODS get_command_constant ABSTRACT
      IMPORTING iv_command TYPE string
      RETURNING VALUE(rv_constant) TYPE string.

    METHODS get_command_name ABSTRACT
      RETURNING VALUE(rv_name) TYPE string.

    METHODS parse_request ABSTRACT
      IMPORTING iv_json TYPE string
      CHANGING cs_request TYPE any.

    METHODS validate_request ABSTRACT
      IMPORTING is_request TYPE any
      RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS get_error_message ABSTRACT
      IMPORTING is_request TYPE any
      RETURNING VALUE(rv_message) TYPE string.

    METHODS return_error
      IMPORTING iv_error TYPE string.

    METHODS return_success
      IMPORTING iv_result TYPE string.

ENDCLASS.

CLASS zcl_abgagt_resource_base IMPLEMENTATION.

  METHOD if_rest_resource~post.
    " Get request body - pass to subclass to parse
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Let subclass handle parsing - get the parsed result type from subclass
    parse_request(
      EXPORTING iv_json = lv_json
      CHANGING cs_request = ms_request ).

    " Validate request
    IF validate_request( ms_request ) = abap_false.
      return_error( get_error_message( ms_request ) ).
      RETURN.
    ENDIF.

    " Get command from factory
    DATA(lo_factory) = zcl_abgagt_cmd_factory=>get_instance( ).
    DATA(lv_constant) = get_command_constant( '' ).
    DATA(lo_command) = lo_factory->get_command( lv_constant ).

    IF lo_command IS NOT BOUND.
      return_error( get_command_name( ) && ' command not found' ).
      RETURN.
    ENDIF.

    " Execute command
    TRY.
        DATA(lv_result) = lo_command->execute( is_param = ms_request ).
        return_success( lv_result ).
      CATCH cx_root INTO DATA(lx_exception).
        return_error( lx_exception->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD return_error.
    DATA lv_json_resp TYPE string.
    CONCATENATE
      '{"success":false,"command":"' get_command_name( ) '","error":"' iv_error '"}'
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
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
