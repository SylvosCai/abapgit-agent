*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_inspect DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
ENDCLASS.

CLASS zcl_abgagt_command_inspect IMPLEMENTATION.
  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_inspect.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    rv_result = 'test'.
  ENDMETHOD.
ENDCLASS.
