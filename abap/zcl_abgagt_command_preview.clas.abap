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

    TYPES: BEGIN OF ty_row_data,
             json TYPE string,
           END OF ty_row_data.

    TYPES ty_rows TYPE STANDARD TABLE OF ty_row_data WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_preview_object,
             name TYPE string,
             type TYPE string,
             type_text TYPE string,
             row_count TYPE i,
             total_rows TYPE i,
             rows TYPE ty_rows,
             fields TYPE ty_fields,
             not_found TYPE abap_bool,
             access_denied TYPE abap_bool,
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

    METHODS detect_object_type
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rv_type) TYPE string.

    METHODS get_table_fields
      IMPORTING iv_table TYPE string
      RETURNING VALUE(rt_fields) TYPE ty_fields.

    METHODS query_table_data
      IMPORTING iv_table TYPE string
      EXPORTING et_rows TYPE ty_rows.

    METHODS query_cds_view_data
      IMPORTING iv_view TYPE string
                iv_limit TYPE i
      EXPORTING et_rows TYPE ty_rows.

    METHODS build_summary
      IMPORTING it_objects TYPE ty_preview_objects
      RETURNING VALUE(rs_summary) TYPE ty_summary.

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
          lt_fields TYPE ty_fields,
          lt_rows TYPE ty_rows,
          lo_struct TYPE REF TO cl_abap_structdescr,
          lv_count TYPE i.

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
      CLEAR: ls_obj, lt_fields, lt_rows.

      ls_obj-name = lv_object.

      " Detect or use provided type
      DATA(lv_type) = ls_params-type.
      IF lv_type IS INITIAL.
        lv_type = detect_object_type( lv_object ).
      ENDIF.

      " Check if object was not found
      IF lv_type IS INITIAL.
        ls_obj-not_found = abap_true.
        ls_obj-type_text = 'Unknown'.
      ELSE.
        ls_obj-type = lv_type.

        " Set type text
        CASE lv_type.
          WHEN 'TABL'.
            ls_obj-type_text = 'Table'.
            " Get table fields
            lt_fields = get_table_fields( lv_object ).
            IF lt_fields IS NOT INITIAL.
              " Try to query data
              query_table_data(
                EXPORTING iv_table = lv_object
                IMPORTING et_rows = lt_rows ).
            ENDIF.

          WHEN 'DDLS'.
            ls_obj-type_text = 'CDS View'.
            " Get CDS view fields (same as table)
            lt_fields = get_table_fields( lv_object ).
            IF lt_fields IS NOT INITIAL.
              " Try to query CDS view data
              query_cds_view_data(
                EXPORTING iv_view = lv_object
                IMPORTING et_rows = lt_rows ).
            ENDIF.

          WHEN OTHERS.
            ls_obj-type_text = lv_type.
            ls_obj-access_denied = abap_true.
        ENDCASE.

        ls_obj-fields = lt_fields.
        ls_obj-rows = lt_rows.
        ls_obj-row_count = lines( lt_rows ).
        ls_obj-total_rows = ls_obj-row_count.  " For now, just show returned count
      ENDIF.

      APPEND ls_obj TO lt_objects.
    ENDLOOP.

    ls_result-success = abap_true.
    ls_result-message = 'Retrieved data from object(s)'.
    ls_result-objects = lt_objects.
    ls_result-summary = build_summary( lt_objects ).

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD detect_object_type.
    " Query TADIR to find actual object type
    SELECT SINGLE object FROM tadir
      INTO rv_type
      WHERE obj_name = iv_name
        AND object IN ('TABL', 'DDLS').

    " If not found in TADIR, check if it's a CDS view pattern (ZC_*)
    IF rv_type IS INITIAL.
      IF iv_name(2) = 'ZC' OR iv_name(2) = 'zy'.
        rv_type = 'DDLS'.
      ELSE.
        " Default to table for unknown
        rv_type = 'TABL'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_table_fields.
    " Get field metadata from DD03L
    SELECT fieldname, datatype, leng
      FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE rt_fields
      WHERE tabname = iv_table
        AND as4local = 'A'
      ORDER BY position.
  ENDMETHOD.

  METHOD query_table_data.
    " Simple implementation - returns field metadata only
    " Dynamic SQL queries require more complex handling
    DATA lv_count TYPE i.

    " Try basic select - this may fail for some tables due to authorization
    TRY.
        SELECT COUNT( * ) FROM (iv_table) INTO @lv_count.
      CATCH cx_sy_dynamic_osql_syntax.
        " Table may not exist or access denied
        lv_count = 0.
    ENDTRY.

    " For now, return empty rows - data retrieval requires more complex handling
    et_rows = VALUE #( ).
  ENDMETHOD.

  METHOD query_cds_view_data.
    " Simple implementation - returns field metadata only
    " Dynamic SQL queries require more complex handling
    DATA lv_count TYPE i.

    " Try basic select
    TRY.
        SELECT COUNT( * ) FROM (iv_view) INTO @lv_count.
      CATCH cx_sy_dynamic_osql_syntax.
        " View may not exist or access denied
        lv_count = 0.
    ENDTRY.

    " For now, return empty rows - data retrieval requires more complex handling
    et_rows = VALUE #( ).
  ENDMETHOD.

  METHOD build_summary.
    rs_summary-total_objects = lines( it_objects ).
    rs_summary-total_rows = 0.

    LOOP AT it_objects INTO DATA(ls_obj).
      rs_summary-total_rows = rs_summary-total_rows + ls_obj-row_count.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
