*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_preview DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA lv_json TYPE string.
    DATA lv_name TYPE string.

    SELECT SINGLE fieldname FROM dd03l
      INTO lv_name
      WHERE tabname = 'TADIR'
        AND as4local = 'A'
        AND position = 1.

    lv_json = '{"success":"X","command":"PREVIEW","message":"' && lv_name && '"}'.

    rv_result = lv_json.
  ENDMETHOD.

ENDCLASS.
