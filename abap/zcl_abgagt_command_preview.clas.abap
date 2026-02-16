*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_preview DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    " Parameter types matching CLI request
    TYPES: BEGIN OF ty_preview_params,
             objects TYPE string_table,
             type TYPE string,
             limit TYPE i,
             where TYPE string,
             columns TYPE string_table,
           END OF ty_preview_params.

    " Field metadata
    TYPES: BEGIN OF ty_field,
             field TYPE string,
             type TYPE string,
             length TYPE int4,
             decimals TYPE int4,
           END OF ty_field.

    TYPES ty_fields TYPE STANDARD TABLE OF ty_field WITH DEFAULT KEY.

    " Row data (dynamic)
    TYPES: BEGIN OF ty_row_data,
             row_index TYPE i,
             data TYPE REF TO data,
           END OF ty_row_data.

    " Table/CVS view data result
    TYPES: BEGIN OF ty_preview_object,
             name TYPE string,
             type TYPE string,
             type_text TYPE string,
             row_count TYPE i,
             total_rows TYPE i,
             rows TYPE string,  " JSON string of rows
             fields TYPE ty_fields,
             columns_displayed TYPE i,
             columns_hidden TYPE string_table,
           END OF ty_preview_object.

    TYPES ty_preview_objects TYPE STANDARD TABLE OF ty_preview_object WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_preview_result,
             success TYPE abap_bool,
             command TYPE string,
             objects TYPE ty_preview_objects,
             summary TYPE REF TO data,  " Dynamic summary
             error TYPE string,
           END OF ty_preview_result.

    TYPES: BEGIN OF ty_summary,
             total_objects TYPE i,
             total_rows TYPE i,
           END OF ty_summary.

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    " Parse parameters
    DATA: ls_params TYPE ty_preview_params,
          ls_result TYPE ty_preview_result.

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

    " Process each object
    DATA lt_objects TYPE ty_preview_objects.
    DATA lv_object TYPE string.
    DATA lv_total_rows TYPE i.

    LOOP AT ls_params-objects INTO lv_object.
      DATA(ls_obj) = get_table_data(
        iv_name    = lv_object
        iv_type    = ls_params-type
        iv_limit   = ls_params-limit
        iv_where   = ls_params-where
        it_columns = ls_params-columns ).

      APPEND ls_obj TO lt_objects.
      lv_total_rows = lv_total_rows + ls_obj-row_count.
    ENDLOOP.

    " Build result
    ls_result-success = abap_true.
    ls_result-message = 'Retrieved data'.
    ls_result-objects = lt_objects.

    " Build summary
    DATA ls_summary TYPE ty_summary.
    ls_summary-total_objects = lines( lt_objects ).
    ls_summary-total_rows = lv_total_rows.

    " We need to serialize with summary inline
    DATA: BEGIN OF ls_final,
            success TYPE abap_bool,
            command TYPE string,
            message TYPE string,
            objects TYPE ty_preview_objects,
            summary TYPE ty_summary,
            error TYPE string,
          END OF ls_final.

    ls_final = CORRESPONDING #( ls_result ).
    ls_final-summary = ls_summary.

    rv_result = /ui2/cl_json=>serialize( data = ls_final ).
  ENDMETHOD.

  METHOD get_table_data.
    DATA: ls_result TYPE ty_preview_object,
          lv_tabname TYPE string,
          lv_type TYPE string.

    ls_result-name = iv_name.
    lv_tabname = to_upper( iv_name ).
    lv_type = to_upper( iv_type ).

    " Auto-detect type if not specified
    IF lv_type IS INITIAL.
      lv_type = detect_object_type( lv_tabname ).
    ENDIF.

    ls_result-type = lv_type.

    CASE lv_type.
      WHEN 'TABL' OR 'DDLS' OR ''.
        ls_result-type_text = COND #( WHEN lv_type = 'DDLS' THEN 'CDS View' ELSE 'Table' ).

        " Get table metadata and data
        DATA(lv_error) = fetch_table_data(
          EXPORTING
            iv_tabname = lv_tabname
            iv_limit   = iv_limit
            iv_where   = iv_where
            it_columns = it_columns
          IMPORTING
            rs_result  = ls_result ).

        IF lv_error IS NOT INITIAL.
          ls_result-type_text = lv_type.
        ENDIF.

      WHEN OTHERS.
        ls_result-type_text = lv_type.
        ls_result-row_count = 0.
    ENDCASE.

    rs_result = ls_result.
  ENDMETHOD.

  METHOD detect_object_type.
    " Check if table or CDS view exists in TADIR
    SELECT SINGLE object FROM tadir
      INTO rv_type
      WHERE obj_name = to_upper( iv_name )
        AND object IN ('TABL', 'DDLS').

    IF sy-subrc <> 0.
      " Default to table if not found in TADIR
      rv_type = 'TABL'.
    ENDIF.
  ENDMETHOD.

  METHOD fetch_table_data.
    " Importing parameters
    DATA: lv_tabname TYPE string,
          iv_limit TYPE i,
          iv_where TYPE string,
          it_columns TYPE string_table.
    lv_tabname = iv_tabname.
    iv_limit = iv_limit.
    iv_where = iv_where.
    it_columns = it_columns.

    DATA: lv_error TYPE string,
          lr_data TYPE REF TO data,
          lo_tabdescr TYPE REF TO cl_abap_tabledescr,
          lo_strucdescr TYPE REF TO cl_abap_structdescr,
          lt_components TYPE abap_component_tab,
          lv_field_list TYPE string,
          lv_limit TYPE i.

    FIELD-SYMBOLS <lt_data> TYPE STANDARD TABLE.

    lv_limit = iv_limit.
    IF lv_limit <= 0.
      lv_limit = 10.
    ENDIF.

    TRY.
        " Create dynamic table type using RTTS
        lo_tabdescr ?= cl_abap_tabledescr=>describe_by_name( lv_tabname ).
        lo_strucdescr ?= lo_tabdescr->get_table_line_type( ).
        lt_components = lo_strucdescr->get_components( ).

        " Handle column selection
        DATA lt_cols TYPE string_table.
        lt_cols = it_columns.

        IF lt_cols IS NOT INITIAL.
          " Build field list from specified columns
          LOOP AT lt_components INTO DATA(ls_comp).
            READ TABLE lt_cols WITH KEY table_line = ls_comp-name TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              IF lv_field_list IS INITIAL.
                lv_field_list = ls_comp-name.
              ELSE.
                lv_field_list = lv_field_list && ',' && ls_comp-name.
              ENDIF.
            ELSE.
              " Track hidden columns
              APPEND ls_comp-name TO rs_result-columns_hidden.
            ENDIF.
          ENDLOOP.
        ELSE.
          " All columns
          LOOP AT lt_components INTO ls_comp.
            IF lv_field_list IS INITIAL.
              lv_field_list = ls_comp-name.
            ELSE.
              lv_field_list = lv_field_list && ',' && ls_comp-name.
            ENDIF.
          ENDLOOP.
        ENDIF.

        " Create data reference for the internal table
        CREATE DATA lr_data TYPE HANDLE lo_tabdescr.
        ASSIGN lr_data->* TO <lt_data>.

        " Dynamic SELECT with field list
        IF iv_where IS INITIAL.
          SELECT (lv_field_list) FROM (lv_tabname)
            INTO TABLE @<lt_data>
            UP TO @lv_limit ROWS.
        ELSE.
          SELECT (lv_field_list) FROM (lv_tabname)
            INTO TABLE @<lt_data>
            WHERE (iv_where)
            UP TO @lv_limit ROWS.
        ENDIF.

        " Get row count
        rs_result-row_count = lines( <lt_data> ).
        rs_result-total_rows = rs_result-row_count.

        " Get field metadata for selected columns only
        rs_result-fields = get_field_metadata_for_columns(
          it_components = lt_components
          it_columns    = lt_cols ).

        " Serialize data to JSON
        rs_result-rows = /ui2/cl_json=>serialize( data = <lt_data> ).

        " Set column info
        rs_result-columns_displayed = lines( rs_result-fields ).

      CATCH cx_sy_dynamic_osql_syntax
            cx_sy_dynamic_osql_field
            cx_sy_open_sql_db
            cx_root INTO DATA(lx_error).
        lv_error = lx_error->get_text( ).
        rs_result-row_count = 0.
    ENDTRY.

    rv_error = lv_error.
  ENDMETHOD.

  METHOD get_field_metadata.
    DATA lt_fields TYPE ty_fields.
    DATA ls_field TYPE ty_field.

    LOOP AT it_components ASSIGNING FIELD-SYMBOL(<comp>).
      ls_field-field = <comp>-name.

      " Get type info from the component
      IF <comp>-type IS BOUND.
        DATA(lo_type) = <comp>-type.
        ls_field-type = lo_type->type_kind.
        ls_field-length = lo_type->length.
        ls_field-decimals = lo_type->decimals.
      ENDIF.

      APPEND ls_field TO lt_fields.
    ENDLOOP.

    rt_fields = lt_fields.
  ENDMETHOD.

  METHOD get_field_metadata_for_columns.
    " Importing parameters
    DATA: it_components TYPE abap_component_tab,
          it_columns TYPE string_table.

    DATA lt_fields TYPE ty_fields.
    DATA ls_field TYPE ty_field.

    " If no columns specified, return all
    IF it_columns IS INITIAL.
      rt_fields = get_field_metadata( it_components ).
      RETURN.
    ENDIF.

    " Filter to selected columns only
    LOOP AT it_components ASSIGNING FIELD-SYMBOL(<comp>).
      READ TABLE it_columns WITH KEY table_line = <comp>-name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_field-field = <comp>-name.
        IF <comp>-type IS BOUND.
          DATA(lo_type) = <comp>-type.
          ls_field-type = lo_type->type_kind.
          ls_field-length = lo_type->length.
          ls_field-decimals = lo_type->decimals.
        ENDIF.
        APPEND ls_field TO lt_fields.
      ENDIF.
    ENDLOOP.

    rt_fields = lt_fields.
  ENDMETHOD.

ENDCLASS.
