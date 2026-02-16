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

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_result TYPE ty_result.
    DATA: lv_tabname TYPE dd03l-tabname.
    DATA: lv_json TYPE string.

    ls_result-command = zif_abgagt_command=>gc_preview.

    " Get table name from parameter - use simple hardcoded for now
    lv_tabname = 'TADIR'.

    " Query first 10 records with minimal fields
    SELECT SINGLE
      obj_name AS f1,
      object   AS f2,
      devclass AS f3
      FROM tadir
      UP TO 10 ROWS
      INTO @DATA(ls_data).

    ls_result-success = abap_true.
    ls_result-message = 'Data retrieved'.
    ls_result-data = ls_data-f1.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

ENDCLASS.
