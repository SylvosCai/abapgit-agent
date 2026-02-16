*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_preview DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_field_out,
             fieldname TYPE string,
             datatype TYPE string,
             leng TYPE string,
           END OF ty_field_out.

    TYPES ty_fields_out TYPE STANDARD TABLE OF ty_field_out WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             command TYPE string,
             message TYPE string,
             fields TYPE ty_fields_out,
             error TYPE string,
           END OF ty_result.

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_result TYPE ty_result.
    DATA: ls_dd03l TYPE dd03l.
    DATA: lt_dd03l TYPE STANDARD TABLE OF dd03l.
    DATA: ls_field TYPE ty_field_out.
    DATA: lt_fields TYPE ty_fields_out.
    DATA: lv_count TYPE i.
    DATA: lv_json TYPE string.

    " Query field metadata from DD03L
    SELECT fieldname, datatype, leng
      FROM dd03l
      UP TO 20 ROWS
      INTO CORRESPONDING FIELDS OF ls_dd03l
      WHERE tabname = 'TADIR'
        AND as4local = 'A'
      ORDER BY position.

      ls_field-fieldname = ls_dd03l-fieldname.
      ls_field-datatype = ls_dd03l-datatype.
      ls_field-leng = ls_dd03l-leng.
      APPEND ls_field TO lt_fields.
    ENDSELECT.

    lv_count = lines( lt_fields ).

    ls_result-success = abap_true.
    ls_result-command = 'PREVIEW'.
    ls_result-message = lv_count.
    ls_result-fields = lt_fields.

    lv_json = /ui2/cl_json=>serialize( data = ls_result ).

    rv_result = lv_json.
  ENDMETHOD.

ENDCLASS.
