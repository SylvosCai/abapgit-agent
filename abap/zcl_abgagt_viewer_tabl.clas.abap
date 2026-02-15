*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_tabl DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_component,
             fieldname TYPE string,
             type TYPE string,
             key TYPE abap_bool,
             description TYPE string,
           END OF ty_component.

    TYPES ty_components TYPE STANDARD TABLE OF ty_component.

    METHODS build_components_json
      IMPORTING iv_tabname TYPE string
      RETURNING VALUE(rv_json) TYPE string.

ENDCLASS.

CLASS zcl_abgagt_viewer_tabl IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass.

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

    " Build simplified components JSON
    rs_info-components = build_components_json( iv_name ).
  ENDMETHOD.

  METHOD build_components_json.
    DATA lv_tabname TYPE dd02l-tabname.
    lv_tabname = iv_tabname.

    DATA lt_fields TYPE TABLE OF dd03l.
    SELECT fieldname datatype keyflag FROM dd03l
      INTO TABLE lt_fields
      WHERE tabname = lv_tabname
        AND as4local = 'A'
      ORDER BY position.

    DATA lt_components TYPE ty_components.
    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      APPEND VALUE #(
        fieldname = <ls_field>-fieldname
        type = <ls_field>-datatype
        key = COND #( WHEN <ls_field>-keyflag = 'X' THEN abap_true ELSE abap_false )
        description = <ls_field>-fieldname
      ) TO lt_components.
    ENDLOOP.

    rv_json = /ui2/cl_json=>serialize( data = lt_components ).
  ENDMETHOD.

ENDCLASS.
