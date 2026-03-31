"! <p class="shorttext synchronized">Syntax Checker for Interfaces</p>
"! Checks ABAP interface source code syntax using SYNTAX-CHECK statement.
CLASS zcl_abgagt_syntax_chk_intf DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

  PRIVATE SECTION.
    DATA mv_fixpt TYPE string.

ENDCLASS.

CLASS zcl_abgagt_syntax_chk_intf IMPLEMENTATION.

  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'INTF'.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~set_fixpt.
    mv_fixpt = iv_fixpt.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~check.
    DATA: ls_dir       TYPE trdir,
          lv_msg       TYPE string,
          lv_line      TYPE i,
          lv_word      TYPE string,
          lv_intfpool  TYPE syrepid,
          lt_skeleton  TYPE string_table,
          lv_intfname  TYPE seoclsname.

    lv_intfname = to_upper( iv_name ).

    rs_result-object_type = 'INTF'.
    rs_result-object_name = lv_intfname.

    " Build interface skeleton: INTERFACE-POOL + source
    APPEND 'INTERFACE-POOL.' TO lt_skeleton.
    APPEND LINES OF it_source TO lt_skeleton.

    IF lt_skeleton IS INITIAL OR lines( lt_skeleton ) <= 1.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      DATA ls_err_no_src TYPE zif_abgagt_syntax_checker=>ty_error.
      ls_err_no_src-line = 1.
      ls_err_no_src-text = 'No source provided for syntax check'.
      APPEND ls_err_no_src TO rs_result-errors.
      rs_result-message = 'No source provided'.
      RETURN.
    ENDIF.

    " Get TRDIR entry for interface pool (for context)
    lv_intfpool = cl_oo_classname_service=>get_interfacepool_name( lv_intfname ).
    SELECT SINGLE * FROM trdir INTO ls_dir WHERE name = lv_intfpool.
    IF sy-subrc <> 0.
      ls_dir-name = lv_intfpool.
      ls_dir-subc = 'J'.  " Interface pool
      ls_dir-uccheck = 'X'.
      " Use FIXPT from XML metadata
      ls_dir-fixpt = mv_fixpt.
    ENDIF.

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
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      DATA ls_err_intf TYPE zif_abgagt_syntax_checker=>ty_error.
      ls_err_intf-line = lv_line.
      ls_err_intf-text = lv_msg.
      ls_err_intf-word = lv_word.
      APPEND ls_err_intf TO rs_result-errors.
      rs_result-message = lv_msg.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
