*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_prog DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_prog IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lt_source TYPE TABLE OF string,
          lv_line TYPE string,
          lv_prog TYPE program.

    lv_prog = iv_name.
    rs_info-name = iv_name.
    rs_info-type = 'PROG'.
    rs_info-type_text = 'Program'.
    rs_info-description = |Program { iv_name }|.

    " Try to read the program/source include directly
    READ REPORT lv_prog INTO lt_source.
    IF sy-subrc = 0.
      IF iv_full = abap_true.
        DATA(ls_section) = VALUE zcl_abgagt_command_view=>ty_section(
          suffix      = 'PROG'
          description = 'Program Source'
          lines       = lt_source ).
        APPEND ls_section TO rs_info-sections.
      ELSE.
        LOOP AT lt_source INTO lv_line.
          IF rs_info-source IS INITIAL.
            rs_info-source = lv_line.
          ELSE.
            rs_info-source = rs_info-source && |\n| && lv_line.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      " Program not found
      rs_info-not_found = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
