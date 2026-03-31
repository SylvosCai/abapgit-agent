*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_tabl DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_tabl IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name  TYPE tadir-obj_name,
          lv_devclass  TYPE tadir-devclass,
          lt_comp      TYPE STANDARD TABLE OF zcl_abgagt_command_view=>ty_component WITH DEFAULT KEY,
          ls_comp      TYPE zcl_abgagt_command_view=>ty_component,
          ls_dd03l     TYPE dd03l,
          lv_tabname   TYPE dd03l-tabname,
          lv_fieldname TYPE dd03l-fieldname,
          lv_rollname  TYPE dd03l-rollname.

    SELECT SINGLE obj_name, devclass FROM tadir
      INTO (@lv_obj_name, @lv_devclass)
      WHERE obj_name = @iv_name
        AND object = 'TABL'.
    rs_info-name      = iv_name.
    rs_info-type      = 'TABL'.
    rs_info-type_text = 'Table'.
    IF sy-subrc = 0.
      rs_info-description = |Table { iv_name } in { lv_devclass }|.
    ELSE.
      rs_info-not_found = abap_true.
    ENDIF.

    " Build components table with data element and description
    lv_tabname = iv_name.
    SELECT fieldname AS field, keyflag AS key, datatype AS type, rollname AS dataelement
      FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE @lt_comp
      WHERE tabname = @lv_tabname
        AND as4local = 'A'
      ORDER BY position.

    " Add length and description
    LOOP AT lt_comp INTO ls_comp.
      lv_fieldname = ls_comp-field.
      SELECT SINGLE * FROM dd03l INTO @ls_dd03l
        WHERE tabname  = @lv_tabname
          AND fieldname = @lv_fieldname
          AND as4local  = 'A'.
      ls_comp-length = ls_dd03l-leng.

      " Get description from DD04T
      lv_rollname = ls_comp-dataelement.
      SELECT SINGLE ddtext FROM dd04t
        INTO @ls_comp-description
        WHERE rollname    = @lv_rollname
          AND ddlanguage  = 'E'
          AND as4local    = 'A'.
      MODIFY lt_comp FROM ls_comp.
    ENDLOOP.

    rs_info-components = lt_comp.
  ENDMETHOD.

ENDCLASS.
