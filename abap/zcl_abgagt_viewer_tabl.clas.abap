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

    " Build components table
    SELECT fieldname AS field keyflag AS key datatype AS type FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE rs_info-components
      WHERE tabname = iv_name
        AND as4local = 'A'
      ORDER BY position.

    " Add length separately
    LOOP AT rs_info-components ASSIGNING FIELD-SYMBOL(<ls_comp>).
      SELECT SINGLE leng FROM dd03l
        INTO <ls_comp>-length
        WHERE tabname = iv_name
          AND fieldname = <ls_comp>-field
          AND as4local = 'A'.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
