*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_preview DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_field,
             field TYPE string,
             type TYPE string,
             length TYPE i,
           END OF ty_field.

    TYPES ty_fields TYPE STANDARD TABLE OF ty_field WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_obj,
             name TYPE string,
             f1 TYPE string,
             f2 TYPE string,
             f3 TYPE string,
           END OF ty_obj.

    TYPES ty_objs TYPE STANDARD TABLE OF ty_obj WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             command TYPE string,
             message TYPE string,
             obj1 TYPE string,
             obj2 TYPE string,
             error TYPE string,
           END OF ty_result.

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_result TYPE ty_result,
          lv_object TYPE string.

    ls_result-command = zif_abgagt_command=>gc_preview.

    " Get first object from input
    DATA lv_json TYPE string.
    lv_json = '{'.
    CONCATENATE lv_json '"objects":["TADIR"]' INTO lv_json.

    ls_result-success = abap_true.
    ls_result-message = 'Preview command working'.
    ls_result-obj1 = 'Field metadata will be added'.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

ENDCLASS.
