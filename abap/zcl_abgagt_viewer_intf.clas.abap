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
          lv_prog TYPE program,
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

    " Get interface section program name using CL_OO_CLASSNAME_SERVICE
    DATA lv_clsname TYPE seoclsname.
    lv_clsname = iv_name.
    CALL METHOD cl_oo_classname_service=>get_intfsec_name
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

    " Extract method information from SEOCOMPT (interfaces use same table)
    DATA lt_methods TYPE STANDARD TABLE OF seocompodf WITH DEFAULT KEY.

    SELECT cmpname descript pabuchname FROM seocompodf
      INTO TABLE @lt_methods
      WHERE clsname = @lv_clsname.

    LOOP AT lt_methods INTO DATA(ls_comp).
      DATA lv_method_name TYPE string.
      DATA lv_method_desc TYPE string.
      CLEAR lv_method_name.
      CLEAR lv_method_desc.
      lv_method_name = ls_comp-cmpname.
      lv_method_desc = ls_comp-descript.

      " Build method string: "PUBLIC METHOD_NAME - description"
      IF lv_method_desc IS NOT INITIAL.
        CONCATENATE 'PUBLIC' lv_method_name '-' lv_method_desc INTO lv_method_desc SEPARATED BY space.
      ELSE.
        CONCATENATE 'PUBLIC' lv_method_name INTO lv_method_desc SEPARATED BY space.
      ENDIF.
      APPEND lv_method_desc TO rs_info-methods.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
