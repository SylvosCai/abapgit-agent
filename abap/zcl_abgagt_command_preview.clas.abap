*"*"use source
*"*"Local Interface:
*"**********************************************************************
" PREVIEW command implementation - preview table/CDS view data
CLASS zcl_abgagt_command_preview DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_preview_params,
             objects TYPE string_table,
             type TYPE string,
             limit TYPE i,
           END OF ty_preview_params.

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
             row_count TYPE i,
             total_rows TYPE i,
             fields TYPE ty_fields,
             not_found TYPE abap_bool,
           END OF ty_preview_object.

    TYPES ty_preview_objects TYPE STANDARD TABLE OF ty_preview_object WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_summary,
             total_objects TYPE i,
             total_rows TYPE i,
           END OF ty_summary.

    TYPES: BEGIN OF ty_preview_result,
             success TYPE abap_bool,
             command TYPE string,
             message TYPE string,
             objects TYPE ty_preview_objects,
             summary TYPE ty_summary,
             error TYPE string,
           END OF ty_preview_result.

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_preview_params,
          ls_result TYPE ty_preview_result,
          lt_objects TYPE ty_preview_objects,
          lv_object TYPE string,
          ls_obj TYPE ty_preview_object,
          lt_fields TYPE ty_fields.

    ls_result-command = zif_abgagt_command=>gc_preview.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-objects IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error = 'Objects parameter is required'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Default limit
    IF ls_params-limit IS INITIAL OR ls_params-limit < 1.
      ls_params-limit = 10.
    ENDIF.

    LOOP AT ls_params-objects INTO lv_object.
      CLEAR: ls_obj, lt_fields.

      ls_obj-name = lv_object.

      " Detect or use provided type
      DATA(lv_type) = ls_params-type.
      IF lv_type IS INITIAL.
        " Default to table for now
        lv_type = 'TABL'.
      ENDIF.

      ls_obj-type = lv_type.

      " Set type text
      CASE lv_type.
        WHEN 'TABL'.
          ls_obj-type_text = 'Table'.
          " Get table fields
          SELECT fieldname, datatype, leng
            FROM dd03l
            INTO CORRESPONDING FIELDS OF TABLE lt_fields
            WHERE tabname = lv_object
              AND as4local = 'A'
            ORDER BY position.
        WHEN 'DDLS'.
          ls_obj-type_text = 'CDS View'.
          " Get CDS view fields (same as table)
          SELECT fieldname, datatype, leng
            FROM dd03l
            INTO CORRESPONDING FIELDS OF TABLE lt_fields
            WHERE tabname = lv_object
              AND as4local = 'A'
            ORDER BY position.
        WHEN OTHERS.
          ls_obj-type_text = lv_type.
      ENDCASE.

      ls_obj-fields = lt_fields.
      ls_obj-row_count = 0.
      ls_obj-total_rows = 0.

      IF lt_fields IS INITIAL.
        ls_obj-not_found = abap_true.
      ENDIF.

      APPEND ls_obj TO lt_objects.
    ENDLOOP.

    ls_result-success = abap_true.
    ls_result-message = 'Retrieved field metadata from object(s)'.
    ls_result-objects = lt_objects.
    ls_result-summary-total_objects = lines( lt_objects ).
    ls_result-summary-total_rows = 0.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

ENDCLASS.
