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
          lt_source TYPE TABLE OF string,
          lv_line TYPE string.

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

    " Get interface source code using READ REPORT
    READ REPORT iv_name INTO lt_source.
    IF sy-subrc = 0.
      LOOP AT lt_source INTO lv_line.
        IF rs_info-source IS INITIAL.
          rs_info-source = lv_line.
        ELSE.
          rs_info-source = rs_info-source && |\n| && lv_line.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
