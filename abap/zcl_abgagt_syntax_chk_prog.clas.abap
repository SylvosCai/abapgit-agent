"! <p class="shorttext synchronized">Syntax Checker for Programs</p>
"! Checks ABAP program source code syntax using SYNTAX-CHECK statement.
CLASS zcl_abgagt_syntax_chk_prog DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

    "! Set unicode check mode
    "! @parameter iv_uccheck | Unicode check mode (X=Standard, 5=Cloud)
    METHODS set_uccheck
      IMPORTING iv_uccheck TYPE trdir-uccheck.

  PRIVATE SECTION.
    DATA mv_uccheck TYPE trdir-uccheck VALUE 'X'.

ENDCLASS.


CLASS zcl_abgagt_syntax_chk_prog IMPLEMENTATION.

  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'PROG'.
  ENDMETHOD.


  METHOD set_uccheck.
    mv_uccheck = iv_uccheck.
  ENDMETHOD.


  METHOD zif_abgagt_syntax_checker~check.
    DATA: ls_dir      TYPE trdir,
          lv_msg      TYPE string,
          lv_line     TYPE i,
          lv_word     TYPE string,
          lv_progname TYPE syrepid.

    lv_progname = to_upper( iv_name ).

    rs_result-object_type = 'PROG'.
    rs_result-object_name = lv_progname.

    IF it_source IS INITIAL.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line = 1
        text = 'No source provided for syntax check' ) ).
      rs_result-message = 'No source provided'.
      RETURN.
    ENDIF.

    " Set TRDIR entry for syntax check context
    ls_dir-name = lv_progname.
    ls_dir-uccheck = mv_uccheck.

    " Run syntax check
    SYNTAX-CHECK FOR it_source
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
      rs_result-errors = VALUE #( (
        line = lv_line
        text = lv_msg
        word = lv_word ) ).
      rs_result-message = lv_msg.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
