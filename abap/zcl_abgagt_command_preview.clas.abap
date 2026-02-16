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
    DATA lv_count TYPE i.

    SELECT fieldname AS field, datatype AS type
      FROM dd03l
      INTO TABLE @DATA(lt_fields)
      WHERE tabname = 'TADIR'
        AND as4local = 'A'
      ORDER BY position.

    lv_count = lines( lt_fields ).

    lv_json = '{"success":"X","command":"PREVIEW","message":"' && lv_count && ' fields","fields":['.

    LOOP AT lt_fields INTO DATA(ls_field).
      IF sy-tabix > 1.
        CONCATENATE lv_json ',' INTO lv_json.
      ENDIF.
      CONCATENATE lv_json '{"field":"' ls_field-field '","type":"' ls_field-type '"}' INTO lv_json.
    ENDLOOP.

    CONCATENATE lv_json ']}' INTO lv_json.

    rv_result = lv_json.
  ENDMETHOD.

ENDCLASS.
