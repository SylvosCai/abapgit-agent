*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_clas DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_clas IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lo_util TYPE REF TO zif_abgagt_util,
          ls_include_info TYPE zif_abgagt_util=>ty_include_info,
          ls_obj_info TYPE zif_abgagt_util=>ty_object_info,
          lv_prog TYPE program,
          lt_source TYPE TABLE OF string,
          lv_line TYPE string,
          lv_clsname TYPE seoclsname.

    rs_info-name = iv_name.
    rs_info-type = 'CLAS'.
    rs_info-type_text = 'Class'.

    " Use util to detect source include
    lo_util = zcl_abgagt_util=>get_instance( ).
    ls_include_info = lo_util->detect_include_info( iv_name ).

    IF ls_include_info-is_source_include = abap_true.
      " Source include detected - get devclass using unpadded obj_name
      ls_obj_info = lo_util->get_object_info_from_tadir( ls_include_info-obj_name ).
      rs_info-devclass = ls_obj_info-devclass.
      IF rs_info-devclass IS NOT INITIAL.
        rs_info-description = |Class { ls_include_info-obj_name } in { rs_info-devclass } (Source Include)|.
      ELSE.
        rs_info-description = |Class { ls_include_info-obj_name } (Source Include)|.
      ENDIF.
    ELSE.
      " Standard class - get info from TADIR
      ls_obj_info = lo_util->get_object_info_from_tadir( iv_name ).
      rs_info-devclass = ls_obj_info-devclass.
      IF rs_info-devclass IS NOT INITIAL.
        rs_info-description = |Class { iv_name } in { rs_info-devclass }|.
      ELSE.
        rs_info-not_found = abap_true.
      ENDIF.
    ENDIF.

    " Get class public section program name
    lv_clsname = ls_include_info-obj_name.
    IF lv_clsname IS INITIAL.
      lv_clsname = iv_name.
    ENDIF.
    CALL METHOD cl_oo_classname_service=>get_pubsec_name
      EXPORTING
        clsname = lv_clsname
      RECEIVING
        result  = lv_prog.

    " Read source from program
    READ REPORT lv_prog INTO lt_source.
    LOOP AT lt_source INTO lv_line.
      IF rs_info-source IS INITIAL.
        rs_info-source = lv_line.
      ELSE.
        rs_info-source = rs_info-source && |\n| && lv_line.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
