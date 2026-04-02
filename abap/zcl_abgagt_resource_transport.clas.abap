*"*"use source
*"*"Local Interface:
*"**********************************************************************
"! <p class="shorttext synchronized">ABAP Git Agent - REST Resource Transport</p>
CLASS zcl_abgagt_resource_transport DEFINITION PUBLIC FINAL
                                 INHERITING FROM zcl_abgagt_resource_base
                                 CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~get REDEFINITION.

  PROTECTED SECTION.
    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS validate_request REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_transport IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_transport.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Transport'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_transport=>ty_transport_params.
  ENDMETHOD.

  METHOD validate_request.
    " Transport command handles its own validation per action
    rv_valid = abap_true.
  ENDMETHOD.

  METHOD if_rest_resource~get.
    " Handle LIST action via GET with optional scope query parameter
    DATA ls_params TYPE zcl_abgagt_command_transport=>ty_transport_params.
    DATA lo_factory TYPE REF TO zif_abgagt_cmd_factory.
    DATA lo_command TYPE REF TO zif_abgagt_command.
    DATA lx_error TYPE REF TO cx_root.

    TRY.
        ls_params-action = 'LIST'.
        ls_params-scope  = mo_request->get_uri_query_parameter( iv_name = 'scope' ).
        IF ls_params-scope IS INITIAL.
          ls_params-scope = 'mine'.
        ENDIF.

        lo_factory = zcl_abgagt_cmd_factory=>get_instance( ).
        lo_command = lo_factory->get_command( zif_abgagt_command=>gc_transport ).

        IF lo_command IS NOT BOUND.
          return_error( 'Transport command not found' ).
          RETURN.
        ENDIF.

        return_success( lo_command->execute( is_param = ls_params ) ).

      CATCH cx_static_check cx_dynamic_check INTO lx_error.
        return_error( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
