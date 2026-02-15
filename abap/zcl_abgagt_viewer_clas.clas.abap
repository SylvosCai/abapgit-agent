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
          lv_line TYPE string.

    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = iv_name
        AND object = 'CLAS'.
    IF sy-subrc = 0.
      rs_info-name = iv_name.
      rs_info-type = 'CLAS'.
      rs_info-type_text = 'Class'.
      rs_info-description = |Class { iv_name } in { lv_devclass }|.
    ENDIF.

    " Get class public section program name using CL_OO_CLASSNAME_SERVICE
    DATA lv_clsname TYPE seoclsname.
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

    " Extract method information from SEOCOMPODF
    DATA lt_methods TYPE STANDARD TABLE OF seocompodf WITH DEFAULT KEY.
    DATA ls_method LIKE LINE OF lt_methods.

    SELECT cmpname exposure FROM seocompodf
      INTO CORRESPONDING FIELDS OF TABLE lt_methods
      WHERE clsname = lv_clsname
        AND exposure = '0'.

    LOOP AT lt_methods INTO DATA(ls_comp).
      DATA lv_method_name TYPE string.
      DATA lv_method_desc TYPE string.
      DATA lv_visibility TYPE string.
      lv_method_name = ls_comp-cmpname.

      " Map exposure to visibility
      CASE ls_comp-exposure.
        WHEN '0'. lv_visibility = 'PUBLIC'.
        WHEN '1'. lv_visibility = 'PROTECTED'.
        WHEN '2'. lv_visibility = 'PRIVATE'.
      ENDCASE.

      " Build method string: "PUBLIC METHOD_NAME"
      CONCATENATE lv_visibility lv_method_name INTO lv_method_desc SEPARATED BY space.
      APPEND lv_method_desc TO rs_info-methods.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
