*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_intf DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_intf IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass,
          lv_method TYPE string.

    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = iv_name
        AND object = 'INTF'.
    IF sy-subrc = 0.
      rs_info-name = iv_name.
      rs_info-type = 'INTF'.
      rs_info-type_text = 'Interface'.
      rs_info-description = |Interface { iv_name } in { lv_devclass }|.
    ENDIF.

    " Fetch interface methods from SEOU table
    DATA lt_methods TYPE STANDARD TABLE OF seou WITH DEFAULT KEY.
    SELECT methodname FROM seou
      INTO TABLE lt_methods
      WHERE clsname = iv_name.
    IF sy-subrc = 0.
      LOOP AT lt_methods INTO DATA(ls_method).
        lv_method = |PUBLIC { ls_method-methodname }|.
        APPEND lv_method TO rs_info-methods.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
