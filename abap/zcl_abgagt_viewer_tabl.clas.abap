*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_tabl DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_table_field,
             fieldname TYPE dd03l-fieldname,
             position TYPE dd03l-position,
             datatype TYPE dd03l-datatype,
             leng TYPE dd03l-leng,
             decimals TYPE dd03l-decimals,
           END OF ty_table_field.

    TYPES ty_table_fields TYPE STANDARD TABLE OF ty_table_field.

    METHODS get_table_fields
      IMPORTING iv_tabname TYPE string
      RETURNING VALUE(rt_result) TYPE ty_table_fields.

ENDCLASS.

CLASS zcl_abgagt_viewer_tabl IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass,
          lt_fields TYPE ty_table_fields.

    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = iv_name
        AND object = 'TABL'.
    IF sy-subrc = 0.
      rs_info-name = iv_name.
      rs_info-type = 'TABL'.
      rs_info-type_text = 'Table'.
      rs_info-description = |Table { iv_name } in { lv_devclass }|.
    ENDIF.

    " Get table fields and serialize to JSON
    lt_fields = get_table_fields( iv_name ).
    rs_info-details = /ui2/cl_json=>serialize( data = lt_fields ).
  ENDMETHOD.

  METHOD get_table_fields.
    DATA lv_tabname TYPE dd02l-tabname.
    lv_tabname = iv_tabname.

    SELECT fieldname position datatype leng decimals
      FROM dd03l
      WHERE tabname = lv_tabname
        AND as4local = 'A'
      ORDER BY position
      INTO CORRESPONDING FIELDS OF TABLE rt_result.
  ENDMETHOD.

ENDCLASS.
