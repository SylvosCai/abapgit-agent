"! <p class="shorttext synchronized">Syntax Checker for Function Groups</p>
"! Checks ABAP function module source code syntax using SYNTAX-CHECK statement.
"! Supports checking individual FM source files (FUNCTION...ENDFUNCTION.).
CLASS zcl_abgagt_syntax_chk_fugr DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

    "! Set the function module name being checked
    "! @parameter iv_fm_name | FM name (e.g. 'ZMY_MY_FUNCTION')
    METHODS set_fm_name
      IMPORTING iv_fm_name TYPE string.

  PRIVATE SECTION.
    DATA mv_fm_name TYPE string.
    DATA mv_fixpt TYPE string.

ENDCLASS.

CLASS zcl_abgagt_syntax_chk_fugr IMPLEMENTATION.

  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'FUGR'.
  ENDMETHOD.

  METHOD set_fm_name.
    mv_fm_name = iv_fm_name.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~set_fixpt.
    mv_fixpt = iv_fixpt.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~check.
    DATA: ls_dir  TYPE trdir,
          lv_msg  TYPE string,
          lv_line TYPE i,
          lv_word TYPE string.

    DATA(lv_group) = to_upper( iv_name ).

    rs_result-object_type = 'FUGR'.
    rs_result-object_name = lv_group.

    IF it_source IS INITIAL.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line = 1
        text = 'No source provided for syntax check' ) ).
      rs_result-message = 'No source provided'.
      RETURN.
    ENDIF.

    " Build skeleton: prepend FUNCTION-POOL line before FM source
    " FM source already contains: FUNCTION <name>. ... ENDFUNCTION.
    DATA(lt_skeleton) = VALUE string_table( ( |FUNCTION-POOL { to_lower( lv_group ) }.| ) ).
    APPEND LINES OF it_source TO lt_skeleton.

    " Set TRDIR entry for function pool context
    " SAPL<GROUP> is the include that lists all FM includes
    ls_dir-name    = |SAPL{ lv_group }|.
    ls_dir-subc    = 'F'.    " Function pool
    ls_dir-uccheck = 'X'.
    ls_dir-fixpt   = mv_fixpt.

    " Run syntax check
    SYNTAX-CHECK FOR lt_skeleton
      MESSAGE lv_msg
      LINE lv_line
      WORD lv_word
      DIRECTORY ENTRY ls_dir.

    IF sy-subrc = 0.
      rs_result-success = abap_true.
      rs_result-error_count = 0.
      rs_result-message = 'Syntax check passed'.
    ELSE.
      " Adjust line: subtract 1 for the prepended FUNCTION-POOL line
      DATA(lv_adjusted) = lv_line - 1.
      IF lv_adjusted < 1.
        lv_adjusted = 1.
      ENDIF.

      " include = lowercase FM name for CLI display (shows which FM file has the error)
      DATA(lv_include) = to_lower( mv_fm_name ).

      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line    = lv_adjusted
        text    = lv_msg
        word    = lv_word
        include = lv_include ) ).
      rs_result-message = lv_msg.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
