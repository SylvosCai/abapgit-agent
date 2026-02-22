*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_view DEFINITION PUBLIC FINAL
                             INHERITING FROM zcl_abgagt_resource_base
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_view IMPLEMENTATION.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.
    lv_json = mo_request->get_entity( )->get_string_data( ).

    " Parse JSON using /ui2/cl_json
    DATA: BEGIN OF ls_request,
            objects TYPE string_table,
            type TYPE string,
          END OF ls_request.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_json
      CHANGING
        data = ls_request ).

    IF ls_request-objects IS INITIAL.
      return_error( 'Objects parameter is required' ).
      RETURN.
    ENDIF.

    " Get command from factory
    DATA(lo_factory) = zcl_abgagt_cmd_factory=>get_instance( ).
    DATA(lo_command) = lo_factory->get_command( zif_abgagt_command=>gc_view ).

    IF lo_command IS NOT BOUND.
      return_error( 'VIEW command not found' ).
      RETURN.
    ENDIF.

    " Execute command with is_param
    DATA ls_params TYPE zcl_abgagt_command_view=>ty_view_params.
    ls_params-objects = ls_request-objects.
    ls_params-type = ls_request-type.

    DATA(lv_result) = lo_command->execute( is_param = ls_params ).

    return_success( lv_result ).
  ENDMETHOD.

ENDCLASS.
