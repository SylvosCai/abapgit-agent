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
    DATA: lv_f1 TYPE string.
    DATA: lv_f2 TYPE string.
    DATA: lv_f3 TYPE string.

    ls_result-command = zif_abgagt_command=>gc_preview.

    " Simple test - return table name
    lv_f1 = 'TADIR'.
    lv_f2 = 'TABL'.
    lv_f3 = 'Table'.

    ls_result-success = abap_true.
    ls_result-message = lv_f3.
    ls_result-data = lv_f1.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

ENDCLASS.
