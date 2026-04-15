"! <p class="shorttext synchronized">Syntax Checker for Enhancements (ENHO)</p>
"! Checks ENHO hook implementation source by wrapping the body in a
"! minimal REPORT skeleton and running SYNTAX-CHECK.
CLASS zcl_abgagt_syntax_chk_enho DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

  PRIVATE SECTION.
    DATA mv_fixpt TYPE string.

    "! Extract class name from full_name string
    "! full_name format: \TY:CLASS\ME:METHOD\SE:SECTION\EI
    "! @parameter iv_full_name | full_name string
    "! @parameter rv_class     | Extracted class name
    METHODS get_class_from_full_name
      IMPORTING iv_full_name   TYPE string
      RETURNING VALUE(rv_class) TYPE seoclsname.

ENDCLASS.

CLASS zcl_abgagt_syntax_chk_enho IMPLEMENTATION.

  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'ENHO'.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~set_fixpt.
    mv_fixpt = iv_fixpt.
  ENDMETHOD.

  METHOD get_class_from_full_name.
    " full_name: \TY:ZCL_FOO\ME:GET_VALUE\SE:BEGIN\EI
    DATA(lv_ty_pos) = find( val = iv_full_name sub = '\TY:' ).
    IF lv_ty_pos < 0.
      RETURN.
    ENDIF.
    DATA(lv_start) = lv_ty_pos + 4.
    DATA(lv_next)  = find( val = iv_full_name sub = '\' off = lv_start ).
    IF lv_next <= lv_start.
      RETURN.
    ENDIF.
    rv_class = substring( val = iv_full_name off = lv_start len = lv_next - lv_start ).
    TRANSLATE rv_class TO UPPER CASE.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~check.
    DATA: lv_class_name TYPE seoclsname,
          lv_classpool  TYPE syrepid,
          ls_dir        TYPE trdir,
          lv_msg        TYPE string,
          lv_line       TYPE i,
          lv_word       TYPE string,
          lt_body       TYPE string_table,
          lt_skeleton   TYPE string_table.
    DATA(lv_in_body) = abap_false.

    rs_result-object_type = 'ENHO'.
    rs_result-object_name = iv_name.

    IF it_source IS INITIAL.
      rs_result-success     = abap_false.
      rs_result-error_count = 1.
      rs_result-errors      = VALUE #( ( line = 1 text = 'No source provided' ) ).
      rs_result-message     = 'No source provided'.
      RETURN.
    ENDIF.

    " Line 1 is a comment: "Name: \TY:CLASS\ME:METHOD\SE:BEGIN\EI
    " Use it to find the target class name for TRDIR context lookup
    READ TABLE it_source INDEX 1 INTO DATA(lv_name_line).
    IF lv_name_line CP '"Name:*'.
      DATA(lv_full_name) = substring( val = lv_name_line off = 7 ).  " after '"Name: '
      lv_class_name = get_class_from_full_name( lv_full_name ).
    ENDIF.

    IF lv_class_name IS INITIAL.
      rs_result-success     = abap_false.
      rs_result-error_count = 1.
      rs_result-errors      = VALUE #( (
        line = 1
        text = 'Cannot determine target class from ENHO source (missing "Name: \TY:... comment)' ) ).
      rs_result-message     = 'Cannot determine target class for ENHO syntax check'.
      RETURN.
    ENDIF.

    " Extract the code between ENHANCEMENT ... and ENDENHANCEMENT.
    LOOP AT it_source INTO DATA(lv_src_line).
      IF lv_src_line CP 'ENHANCEMENT *'.
        lv_in_body = abap_true.
        CONTINUE.
      ENDIF.
      IF lv_src_line CP 'ENDENHANCEMENT*'.
        lv_in_body = abap_false.
        CONTINUE.
      ENDIF.
      IF lv_in_body = abap_true.
        APPEND lv_src_line TO lt_body.
      ENDIF.
    ENDLOOP.

    " Build a minimal executable program skeleton (REPORT, not CLASS-POOL).
    " Using REPORT avoids conflicts with the real target class already in the system.
    " The TRDIR from the real class pool provides uccheck/fixpt/unicode settings.
    APPEND 'REPORT ZENHO_SYN_CHK.' TO lt_skeleton.
    DATA(lv_header_lines) = lines( lt_skeleton ).  " 1 line before body
    APPEND LINES OF lt_body TO lt_skeleton.

    " Get TRDIR context from the real class pool for uccheck/fixpt settings
    lv_classpool = cl_oo_classname_service=>get_classpool_name( lv_class_name ).
    SELECT SINGLE * FROM trdir INTO ls_dir WHERE name = lv_classpool.
    IF sy-subrc <> 0.
      ls_dir-uccheck = 'X'.
    ENDIF.
    ls_dir-name = 'ZENHO_SYN_CHK'.
    ls_dir-subc = '1'.
    IF mv_fixpt = 'X'.
      ls_dir-fixpt = 'X'.
    ENDIF.

    SYNTAX-CHECK FOR lt_skeleton
      MESSAGE lv_msg
      LINE lv_line
      WORD lv_word
      DIRECTORY ENTRY ls_dir.

    IF sy-subrc = 0.
      rs_result-success     = abap_true.
      rs_result-error_count = 0.
      rs_result-message     = 'Syntax check passed'.
    ELSE.
      " Adjust line number back to body-relative
      DATA(lv_adjusted) = lv_line - lv_header_lines.
      IF lv_adjusted < 1.
        lv_adjusted = 1.
      ENDIF.
      rs_result-success     = abap_false.
      rs_result-error_count = 1.
      rs_result-errors      = VALUE #( (
        line = lv_adjusted
        text = lv_msg
        word = lv_word ) ).
      rs_result-message     = lv_msg.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
