*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_tabl DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_tabl IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass,
          lv_tabname TYPE dd02l-tabname.

    lv_tabname = iv_name.

    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = lv_tabname
        AND object = 'TABL'.
    IF sy-subrc = 0.
      rs_info-name = iv_name.
      rs_info-type = 'TABL'.
      rs_info-type_text = 'Table'.
      rs_info-description = |Table { iv_name } in { lv_devclass }|.
    ENDIF.

    " Build components JSON using separate statements
    DATA lt_fields TYPE TABLE OF dd03l.
    SELECT fieldname datatype leng FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE lt_fields
      WHERE tabname = lv_tabname
        AND as4local = 'A'
      ORDER BY position.

    rs_info-components = /ui2/cl_json=>serialize( data = lt_fields ).
  ENDMETHOD.

ENDCLASS.
