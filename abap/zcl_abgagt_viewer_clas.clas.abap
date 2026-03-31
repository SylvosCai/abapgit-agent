*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_clas DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_clas IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: ls_include_info TYPE zif_abgagt_util=>ty_include_info,
          ls_obj_info TYPE zif_abgagt_util=>ty_object_info,
          lv_prog TYPE program,
          lt_source TYPE TABLE OF string,
          lv_clsname TYPE seoclsname,
          lv_obj_name TYPE tadir-obj_name.
    DATA lo_util TYPE REF TO zif_abgagt_util.
    DATA ls_section     TYPE zcl_abgagt_command_view=>ty_section.
    DATA lv_pubsec      TYPE program.
    DATA lv_include_pad TYPE program.
    DATA lv_pad30 TYPE program.
    DATA lt_methods TYPE STANDARD TABLE OF tmdir.
    DATA ls_method LIKE LINE OF lt_methods.
    DATA lv_cm_suffix TYPE string.
    DATA lv_line TYPE string.
    DATA lv_methodindx TYPE i.

    rs_info-name = iv_name.
    rs_info-type = 'CLAS'.
    rs_info-type_text = 'Class'.

    " Use util to detect source include
    lo_util = zcl_abgagt_util=>get_instance( ).
    ls_include_info = lo_util->detect_include_info( iv_name ).

    IF ls_include_info-is_source_include = abap_true.
      " Source include detected - get devclass using unpadded obj_name
      lv_obj_name = ls_include_info-obj_name.
      ls_obj_info = lo_util->get_object_info_from_tadir( lv_obj_name ).
      rs_info-devclass = ls_obj_info-devclass.
      IF rs_info-devclass IS NOT INITIAL.
        rs_info-description = |Class { ls_include_info-obj_name } in { rs_info-devclass } (Source Include)|.
      ELSE.
        rs_info-description = |Class { ls_include_info-obj_name } (Source Include)|.
      ENDIF.
    ELSE.
      " Standard class - get info from TADIR
      lv_obj_name = iv_name.
      ls_obj_info = lo_util->get_object_info_from_tadir( lv_obj_name ).
      rs_info-devclass = ls_obj_info-devclass.
      IF rs_info-devclass IS NOT INITIAL.
        rs_info-description = |Class { iv_name } in { rs_info-devclass }|.
      ELSE.
        rs_info-not_found = abap_true.
      ENDIF.
    ENDIF.

    " Get class name (strip include suffix if needed)
    lv_clsname = ls_include_info-obj_name.
    IF lv_clsname IS INITIAL.
      lv_clsname = iv_name.
    ENDIF.

    IF iv_full = abap_false.
      " Default: return public section source only
      cl_oo_classname_service=>get_pubsec_name( EXPORTING clsname = lv_clsname RECEIVING result = lv_prog ).

      READ REPORT lv_prog INTO lt_source.
      LOOP AT lt_source INTO lv_line.
        IF rs_info-source IS INITIAL.
          rs_info-source = lv_line.
        ELSE.
          rs_info-source = rs_info-source && |\n| && lv_line.
        ENDIF.
      ENDLOOP.
    ELSE.
      " Full mode: assemble all sections into rs_info-sections.
      " global_start (assembled-source line numbers) is computed client-side
      " by the Node.js CLI from the local .clas.abap file or ADT source fetch.

      " Build padded class name prefix (30 chars) for direct include reads
      lv_pad30 = lv_clsname.
      WHILE strlen( lv_pad30 ) < 30.
        lv_pad30 = lv_pad30 && '='.
      ENDWHILE.

      cl_oo_classname_service=>get_pubsec_name( EXPORTING clsname = lv_clsname RECEIVING result = lv_pubsec ).

      " Public section (CU)
      CLEAR ls_section.
      ls_section-suffix      = 'CU'.
      ls_section-description = 'Public Section'.
      CLEAR lt_source.
      READ REPORT lv_pubsec INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      " Protected section (CO)
      CLEAR ls_section.
      ls_section-suffix      = 'CO'.
      ls_section-description = 'Protected Section'.
      lv_prog = lv_pad30 && 'CO'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      " Private section (CP)
      CLEAR ls_section.
      ls_section-suffix      = 'CP'.
      ls_section-description = 'Private Section'.
      lv_prog = lv_pad30 && 'CP'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      " Method implementations (CM*) in methodindx order (= assembled-source order)
      SELECT methodname, methodindx FROM tmdir
        INTO TABLE @lt_methods
        WHERE classname = @lv_clsname
        ORDER BY methodindx.

      LOOP AT lt_methods INTO ls_method.
        lv_methodindx = ls_method-methodindx.
        lv_cm_suffix   = lo_util->convert_index_to_cm_suffix( lv_methodindx ).
        lv_include_pad = lv_pad30 && lv_cm_suffix.

        CLEAR ls_section.
        ls_section-suffix      = lv_cm_suffix.
        ls_section-description = 'Class Method'.
        ls_section-method_name = ls_method-methodname.
        IF ls_section-method_name IS INITIAL.
          ls_section-description = |Class Method [DEBUG:mname_empty idx={ ls_method-methodindx } clsname={ lv_clsname } mname_raw={ ls_method-methodname }]|.
        ENDIF.
        CLEAR lt_source.
        READ REPORT lv_include_pad INTO lt_source.
        IF sy-subrc = 0.
          ls_section-lines = lt_source.
        ENDIF.
        APPEND ls_section TO rs_info-sections.
      ENDLOOP.

      " Auxiliary sections: CCDEF, CCIMP, CCAU
      " These live in separate git files (not part of the assembled .clas.abap source).
      CLEAR ls_section.
      ls_section-suffix      = 'CCDEF'.
      ls_section-description = 'Local Definitions'.
      ls_section-file        = 'locals_def'.
      lv_prog = lv_pad30 && 'CCDEF'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      CLEAR ls_section.
      ls_section-suffix      = 'CCIMP'.
      ls_section-description = 'Local Implementations'.
      ls_section-file        = 'locals_imp'.
      lv_prog = lv_pad30 && 'CCIMP'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      CLEAR ls_section.
      ls_section-suffix      = 'CCAU'.
      ls_section-description = 'Unit Test'.
      ls_section-file        = 'testclasses'.
      lv_prog = lv_pad30 && 'CCAU'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
