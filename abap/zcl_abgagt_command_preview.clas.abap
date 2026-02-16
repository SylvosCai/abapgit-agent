*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_preview DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             command TYPE string,
             message TYPE string,
             data TYPE string,
             error TYPE string,
           END OF ty_result.

    TYPES: BEGIN OF ty_row,
             f1 TYPE string,
             f2 TYPE string,
             f3 TYPE string,
           END OF ty_row.

    TYPES ty_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_result TYPE ty_result.
    DATA: ls_row TYPE ty_row.
    DATA: lt_rows TYPE ty_rows.
    DATA: lv_json TYPE string.

    ls_result-command = zif_abgagt_command=>gc_preview.

    " Query first 10 records from TADIR
    SELECT obj_name, object, devclass
      FROM tadir
      UP TO 10 ROWS
      INTO CORRESPONDING FIELDS OF @ls_row.

      APPEND ls_row TO lt_rows.
    ENDSELECT.

    ls_result-success = abap_true.
    ls_result-message = 'Data retrieved'.
    ls_result-data = 'TADIR'.

    lv_json = /ui2/cl_json=>serialize( data = ls_result ).

    rv_result = lv_json.
  ENDMETHOD.

ENDCLASS.
