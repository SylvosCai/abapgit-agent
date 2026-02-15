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
    DATA ls_method TYPE zcl_abgagt_command_view=>ty_method.
    DATA lt_methods TYPE STANDARD TABLE OF seocompodf WITH DEFAULT KEY.

    SELECT cmpname descript pabuchname FROM seocompodf
      INTO TABLE lt_methods
      WHERE clsname = lv_clsname.

    LOOP AT lt_methods INTO DATA(ls_comp).
      CLEAR ls_method.
      ls_method-name = ls_comp-cmpname.
      ls_method-description = ls_comp-descript.
      ls_method-visibility = 'PUBLIC'.

      " Get method parameters from SEOSUBCODF
      DATA lt_params TYPE STANDARD TABLE OF seosubcodf WITH DEFAULT KEY.
      SELECT fname type sconame parclsn defaultvalue FROM seosubcodf
        INTO TABLE lt_params
        WHERE clsname = lv_clsname
          AND cmpname = ls_comp-cmpname.

      LOOP AT lt_params INTO DATA(ls_param).
        DATA ls_param_out TYPE zcl_abgagt_command_view=>ty_method_param.
        ls_param_out-name = ls_param-fname.
        ls_param_out-type = ls_param-type.

        " Determine pass type
        IF ls_param-sconame IS INITIAL.
          ls_param_out-pass = 'IMPORTING'.
        ELSE.
          CASE ls_param-sconame.
            WHEN 'I'. ls_param_out-pass = 'IMPORTING'.
            WHEN 'E'. ls_param_out-pass = 'EXPORTING'.
            WHEN 'C'. ls_param_out-pass = 'CHANGING'.
            WHEN 'R'. ls_param_out-pass = 'RETURNING'.
          ENDCASE.
        ENDIF.

        " Check if optional
        IF ls_param-defaultvalue IS NOT INITIAL.
          ls_param_out-optional = abap_true.
        ENDIF.

        APPEND ls_param_out TO ls_method-parameters.
      ENDLOOP.

      " Check for returning parameter
      READ TABLE lt_params WITH KEY sconame = 'R' TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_method-return-name = 'RETURNING'.
        READ TABLE lt_params WITH KEY sconame = 'R' INTO DATA(ls_ret).
        IF sy-subrc = 0.
          ls_method-return-type = ls_ret-type.
        ENDIF.
      ENDIF.

      APPEND ls_method TO rs_info-methods.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
