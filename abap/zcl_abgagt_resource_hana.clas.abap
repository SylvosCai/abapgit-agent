*"*"use source
*"*"Local Interface:
*"**********************************************************************
"! <p class="shorttext synchronized">ABAP Git Agent - REST Resource HANA</p>
CLASS zcl_abgagt_resource_hana DEFINITION PUBLIC FINAL
                                INHERITING FROM zcl_abgagt_resource_base
                                CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_command_constant REDEFINITION.
    METHODS get_command_name     REDEFINITION.
    METHODS create_request_data  REDEFINITION.
    METHODS validate_request     REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_hana IMPLEMENTATION.

  METHOD get_command_constant.
    rv_constant = zif_abgagt_command=>gc_hana.
  ENDMETHOD.

  METHOD get_command_name.
    rv_name = 'Hana'.
  ENDMETHOD.

  METHOD create_request_data.
    CREATE DATA rr_request_data TYPE zcl_abgagt_command_hana=>ty_request.
  ENDMETHOD.

  METHOD validate_request.
    rv_valid = abap_true.
  ENDMETHOD.

ENDCLASS.
