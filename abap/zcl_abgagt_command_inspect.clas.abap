*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_inspect DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_error,
             line TYPE string,
             column TYPE string,
             text TYPE string,
             word TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.
ENDCLASS.

CLASS zcl_abgagt_command_inspect IMPLEMENTATION.
  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_inspect.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    rv_result = 'test'.
  ENDMETHOD.
ENDCLASS.
