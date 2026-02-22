*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_clas DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_clas IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass,
          lv_prog TYPE program,
          lt_source TYPE TABLE OF string,
          lv_line TYPE string,
          lv_name TYPE tadir-obj_name,
          lv_name_len TYPE i,
          lv_clsname TYPE seoclsname.

    lv_name = iv_name.
    lv_name_len = strlen( lv_name ).

    " Check if this is a source include (method, test class, section)
    IF lv_name_len >= 32.
      " Try to read as program/include directly first
      lv_prog = lv_name.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        " This is a source include - get the class name from it
        lv_clsname = lv_name(30). " First 30 chars are the class name

        " Try to get class info from TADIR
        SELECT SINGLE obj_name devclass FROM tadir
          INTO (lv_obj_name, lv_devclass)
          WHERE obj_name = lv_clsname
            AND object = 'CLAS'.

        rs_info-name = lv_name.
        rs_info-type = 'CLAS'.
        rs_info-type_text = 'Class'.
        IF lv_devclass IS NOT INITIAL.
          rs_info-description = |Class { lv_clsname } in { lv_devclass } (Source Include)|.
        ELSE.
          rs_info-description = |Class { lv_clsname } (Source Include)|.
        ENDIF.

        " Read source
        LOOP AT lt_source INTO lv_line.
          IF rs_info-source IS INITIAL.
            rs_info-source = lv_line.
          ELSE.
            rs_info-source = rs_info-source && |\n| && lv_line.
          ENDIF.
        ENDLOOP.
        RETURN.
      ENDIF.
    ENDIF.

    " Standard class - get from TADIR
    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = iv_name
        AND object = 'CLAS'.
    IF sy-subrc = 0.
      rs_info-name = iv_name.
      rs_info-type = 'CLAS'.
      rs_info-type_text = 'Class'.
      rs_info-description = |Class { iv_name } in { lv_devclass }|.
    ELSE.
      rs_info-name = iv_name.
      rs_info-type = 'CLAS'.
      rs_info-type_text = 'Class'.
      rs_info-not_found = abap_true.
    ENDIF.

    " Get class public section program name using CL_OO_CLASSNAME_SERVICE
    lv_clsname = iv_name.
    CALL METHOD cl_oo_classname_service=>get_pubsec_name
      EXPORTING
        clsname = lv_clsname
      RECEIVING
        result  = lv_prog.

    " Read source from program
    READ REPORT lv_prog INTO lt_source.
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
