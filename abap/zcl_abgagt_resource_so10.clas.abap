CLASS zcl_abgagt_resource_so10 DEFINITION PUBLIC FINAL
  INHERITING FROM zcl_abgagt_resource_base CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name REDEFINITION.
    METHODS create_request_data REDEFINITION.
    METHODS get_bg_config REDEFINITION.
ENDCLASS.

CLASS zcl_abgagt_resource_so10 IMPLEMENTATION.
  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_so10.
  ENDMETHOD.
  METHOD get_command_name.
    rv_name = 'SO10'.
  ENDMETHOD.
  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_so10=>ty_params.
  ENDMETHOD.
  METHOD get_bg_config.
    rs_config = super->get_bg_config( ).
    rs_config-force_sync = abap_true.
  ENDMETHOD.
ENDCLASS.
