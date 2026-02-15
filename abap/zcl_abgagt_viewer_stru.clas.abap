*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_stru DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_stru IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass.

    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = iv_name
        AND object = 'TABL'.
    IF sy-subrc = 0.
      rs_info-name = iv_name.
      rs_info-type = 'STRU'.
      rs_info-type_text = 'Structure'.
      rs_info-description = |Structure { iv_name } in { lv_devclass }|.
    ENDIF.

    " Build components table with data element and description
    SELECT fieldname AS field keyflag AS key datatype AS type rollname AS dataelement
      FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE rs_info-components
      WHERE tabname = iv_name
        AND as4local = 'A'
      ORDER BY position.

    " Add length and description
    LOOP AT rs_info-components ASSIGNING FIELD-SYMBOL(<ls_comp>).
      DATA ls_dd03l TYPE dd03l.
      SELECT SINGLE * FROM dd03l INTO ls_dd03l
        WHERE tabname = iv_name
          AND fieldname = <ls_comp>-field
          AND as4local = 'A'.
      <ls_comp>-length = ls_dd03l-leng.

      " Get description from DD04T
      SELECT SINGLE ddtext FROM dd04t
        INTO <ls_comp>-description
        WHERE rollname = <ls_comp>-dataelement
          AND ddlanguage = 'E'
          AND as4local = 'A'.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
