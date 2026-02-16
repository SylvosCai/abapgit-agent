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
    DATA: lv_json TYPE string.
    DATA: ls_dd03l TYPE dd03l.
    DATA: lt_dd03l TYPE STANDARD TABLE OF dd03l.
    DATA: lv_count TYPE i.

    SELECT * FROM dd03l
      UP TO 10 ROWS
      INTO CORRESPONDING FIELDS OF ls_dd03l
      WHERE tabname = 'TADIR'
        AND as4local = 'A'.

      APPEND ls_dd03l TO lt_dd03l.
    ENDSELECT.

    lv_count = lines( lt_dd03l ).

    lv_json = '{"success":true,"command":"PREVIEW","message":"Retrieved ' && lv_count && ' fields"}'.

    rv_result = lv_json.
  ENDMETHOD.

ENDCLASS.
