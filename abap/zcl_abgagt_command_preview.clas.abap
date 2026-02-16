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

    TYPES: BEGIN OF ty_preview_object,
             name TYPE string,
             type TYPE string,
             type_text TYPE string,
             fields TYPE ty_fields,
             not_found TYPE abap_bool,
           END OF ty_preview_object.

    TYPES ty_preview_objects TYPE STANDARD TABLE OF ty_preview_object WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             command TYPE string,
             message TYPE string,
             objects TYPE ty_preview_objects,
             error TYPE string,
           END OF ty_result.

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_result TYPE ty_result,
          lv_object TYPE string,
          ls_obj TYPE ty_preview_object,
          lt_fields TYPE ty_fields.

    ls_result-command = zif_abgagt_command=>gc_preview.

    " Parse input parameter
    FIELD-SYMBOLS <ls_param> TYPE any.
    ASSIGN is_param TO <ls_param>.
    IF <ls_param> IS ASSIGNED.
      ASSIGN COMPONENT 'OBJECTS' OF STRUCTURE <ls_param> TO FIELD-SYMBOL(<lv_objects>).
      IF <lv_objects> IS ASSIGNED.
        LOOP AT <lv_objects> INTO lv_object.
          ls_obj-name = lv_object.
          ls_obj-type = 'TABL'.
          ls_obj-type_text = 'Table'.

          " Get fields from DD03L
          SELECT fieldname, datatype, leng
            FROM dd03l
            INTO CORRESPONDING FIELDS OF TABLE lt_fields
            WHERE tabname = lv_object
              AND as4local = 'A'
            ORDER BY position.

          IF lt_fields IS INITIAL.
            ls_obj-not_found = abap_true.
          ELSE.
            ls_obj-fields = lt_fields.
          ENDIF.

          APPEND ls_obj TO ls_result-objects.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF ls_result-objects IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error = 'No objects provided'.
    ELSE.
      ls_result-success = abap_true.
      ls_result-message = 'Retrieved field metadata'.
    ENDIF.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

ENDCLASS.
