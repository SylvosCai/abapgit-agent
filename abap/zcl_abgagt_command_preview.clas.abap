*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_preview DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES:
    BEGIN OF ty_field,
      fieldname TYPE string,
      datatype TYPE string,
      leng TYPE string,
    END OF ty_field.

    TYPES ty_fields TYPE STANDARD TABLE OF ty_field WITH DEFAULT KEY.

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA lv_json TYPE string.

    SELECT fieldname, datatype, leng
      FROM dd03l
      UP TO 10 ROWS
      INTO CORRESPONDING FIELDS OF @DATA(ls_dd03l)
      WHERE tabname = 'TADIR'
        AND as4local = 'A'.

    ENDSELECT.

    lv_json = '{"success":"X","command":"PREVIEW","message":"OK"}'.

    rv_result = lv_json.
  ENDMETHOD.

ENDCLASS.
