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
    DATA: ls_dd03l TYPE dd03l.
    DATA: lt_dd03l TYPE STANDARD TABLE OF dd03l.
    DATA: lv_count TYPE i.
    DATA: lv_json TYPE string.
    DATA: lv_sep TYPE string.

    SELECT fieldname, datatype, leng
      FROM dd03l
      UP TO 20 ROWS
      INTO CORRESPONDING FIELDS OF ls_dd03l
      WHERE tabname = 'TADIR'
        AND as4local = 'A'
      ORDER BY position.

      APPEND ls_dd03l TO lt_dd03l.
    ENDSELECT.

    lv_count = lines( lt_dd03l ).

    lv_json = '{"success":true,"command":"PREVIEW","message":"' && lv_count && ' fields found","fields":['.

    LOOP AT lt_dd03l INTO ls_dd03l.
      IF sy-tabix > 1.
        CONCATENATE lv_json ',' INTO lv_json.
      ENDIF.
      CONCATENATE lv_json '{"fieldname":"' ls_dd03l-fieldname '","datatype":"' ls_dd03l-datatype '","leng":"' ls_dd03l-leng '"}' INTO lv_json.
    ENDLOOP.

    CONCATENATE lv_json ']}' INTO lv_json.

    rv_result = lv_json.
  ENDMETHOD.

ENDCLASS.
