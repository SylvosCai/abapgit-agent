*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_base DEFINITION PUBLIC ABSTRACT
                           INHERITING FROM cl_rest_resource
                           CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS if_rest_resource~post REDEFINITION.

  PROTECTED SECTION.

    METHODS get_command_constant ABSTRACT
      IMPORTING iv_command TYPE string
      RETURNING VALUE(rv_constant) TYPE string.

    METHODS get_command_name ABSTRACT
      RETURNING VALUE(rv_name) TYPE string.

    METHODS create_request_data ABSTRACT
      RETURNING VALUE(rr_request_data) TYPE REF TO data.

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
      IMPORTING iv_result TYPE string.

ENDCLASS.

CLASS zcl_abgagt_resource_base IMPLEMENTATION.

  METHOD if_rest_resource~post.
    " Get request body
    DATA lv_json TYPE string.
    DATA lr_request TYPE REF TO data.

    lv_json = mo_request->get_entity( )->get_string_data( ).
    lr_request = create_request_data( ).

    ASSIGN lr_request->* TO FIELD-SYMBOL(<ls_request>).

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
    DATA(lv_constant) = get_command_constant( '' ).
    DATA(lo_command) = lo_factory->get_command( lv_constant ).

    IF lo_command IS NOT BOUND.
      return_error( get_command_name( ) && ' command not found' ).
      RETURN.
    ENDIF.

    " Execute command
    TRY.
        DATA(lv_result) = lo_command->execute( is_param = <ls_request> ).
        return_success( lv_result ).
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
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
