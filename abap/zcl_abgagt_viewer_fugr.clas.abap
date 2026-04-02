*"*"use source
*"*"Local Interface:
**********************************************************************
CLASS zcl_abgagt_viewer_fugr DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_fugr IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    TYPES:
      BEGIN OF ty_func,
        funcname TYPE tfdir-funcname,
        fmode    TYPE tfdir-fmode,
        include  TYPE tfdir-include,
      END OF ty_func,
      BEGIN OF ty_text,
        spras    TYPE tftit-spras,
        funcname TYPE tftit-funcname,
        stext    TYPE tftit-stext,
      END OF ty_text.

    DATA: lv_devclass    TYPE tadir-devclass,
          lv_pname       TYPE tfdir-pname,
          lt_funcs       TYPE STANDARD TABLE OF ty_func,
          lt_texts       TYPE STANDARD TABLE OF ty_text,
          ls_func        TYPE ty_func,
          ls_txt         TYPE ty_text,
          lv_stext       TYPE tftit-stext,
          lv_fm_include  TYPE tfdir-include,
          lt_source      TYPE string_table,
          lv_read_prog   TYPE program.

    " Split iv_name at '/' to detect fm-scoped request: 'SUSR/AUTHORITY_CHECK'
    DATA(lv_sep) = find( val = iv_name sub = '/' ).
    DATA(lv_group_name) = COND string(
      WHEN lv_sep >= 0 THEN substring( val = iv_name off = 0 len = lv_sep )
      ELSE iv_name ).
    DATA(lv_fm_name) = COND string(
      WHEN lv_sep >= 0 THEN substring( val = iv_name off = lv_sep + 1 )
      ELSE '' ).

    rs_info-name      = lv_group_name.
    rs_info-type      = 'FUGR'.
    rs_info-type_text = 'Function Group'.

    " Get package from TADIR
    SELECT SINGLE devclass FROM tadir
      INTO @lv_devclass
      WHERE obj_name = @lv_group_name
        AND object   = 'FUGR'.
    IF sy-subrc <> 0.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.

    rs_info-devclass    = lv_devclass.
    rs_info-description = |Function Group { lv_group_name } in { lv_devclass }|.

    " Derive program name and group shortname: SAPL<FUGR>
    lv_pname = |SAPL{ lv_group_name }|.
    DATA(lv_group) = CONV string( lv_pname+4 ).

    " --full --fm: show source of a specific FM as a section
    IF iv_full = abap_true AND lv_fm_name IS NOT INITIAL.
      SELECT SINGLE include FROM tfdir
        INTO @lv_fm_include
        WHERE pname    = @lv_pname
          AND funcname = @lv_fm_name.
      IF sy-subrc = 0.
        DATA(lv_include_name) = |L{ lv_group }U{ lv_fm_include }|.
        lv_read_prog = lv_include_name.
        READ REPORT lv_read_prog INTO lt_source.
        IF sy-subrc = 0.
          APPEND VALUE zcl_abgagt_command_view=>ty_section(
            suffix      = lv_include_name
            description = |Function Module { lv_fm_name }|
            method_name = lv_fm_name
            lines       = lt_source
          ) TO rs_info-sections.
        ENDIF.
      ENDIF.
      RETURN.
    ENDIF.

    " Get function module list from TFDIR
    SELECT funcname, fmode, include
      INTO TABLE @lt_funcs
      FROM tfdir
      WHERE pname = @lv_pname
      ORDER BY funcname.

    " Fetch all short texts for the found function modules
    SELECT spras, funcname, stext
      INTO TABLE @lt_texts
      FROM tftit
      WHERE funcname IN ( SELECT funcname FROM tfdir WHERE pname = @lv_pname )
      ORDER BY funcname, spras.

    " Build output: prefer English text, fall back to first available
    LOOP AT lt_funcs INTO ls_func.
      CLEAR lv_stext.

      " Try English first
      READ TABLE lt_texts INTO ls_txt
        WITH KEY funcname = ls_func-funcname spras = 'E'.
      IF sy-subrc = 0.
        lv_stext = ls_txt-stext.
      ELSE.
        " Any available language
        READ TABLE lt_texts INTO ls_txt
          WITH KEY funcname = ls_func-funcname.
        IF sy-subrc = 0.
          lv_stext = ls_txt-stext.
        ENDIF.
      ENDIF.

      DATA(lv_rfc) = CONV string( '' ).
      CASE ls_func-fmode.
        WHEN 'R'.
          lv_rfc = ' [RFC]'.
        WHEN 'S'.
          lv_rfc = ' [RFC-Start]'.
        WHEN OTHERS.
          CLEAR lv_rfc.
      ENDCASE.

      DATA(lv_desc) = |{ ls_func-funcname }{ lv_rfc }: { lv_stext }|.
      IF iv_full = abap_true.
        " --full: append the include name so user can view/debug the FM source
        DATA(lv_fm_inc) = |L{ lv_group }U{ ls_func-include }|.
        lv_desc = lv_desc && |  ->  { lv_fm_inc }|.
      ENDIF.

      APPEND VALUE #(
        field       = ls_func-funcname
        key         = abap_false
        type        = 'CHAR'
        length      = 30
        dataelement = ''
        description = lv_desc
      ) TO rs_info-components.
    ENDLOOP.

    APPEND VALUE #(
      field       = 'COUNT'
      key         = abap_false
      type        = 'INT4'
      length      = 4
      dataelement = ''
      description = |Function modules: { lines( lt_funcs ) }|
    ) TO rs_info-components.
  ENDMETHOD.

ENDCLASS.
