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
          lv_devclass TYPE tadir-devclass.

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
  ENDMETHOD.

ENDCLASS.
