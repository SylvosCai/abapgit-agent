"! <p class="shorttext synchronized">Syntax Checker for DDLS (CDS Views)</p>
"! Checks DDLS (CDS View) source code syntax using CL_DD_DDL_HANDLER.
"! EXPERIMENTAL: Tests if DDL handler can validate source without DB object.
CLASS zcl_abgagt_syntax_chk_ddls DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

  PRIVATE SECTION.
    "! Convert DDL errors to syntax checker error format
    "! @parameter it_ddl_errors | DDL error table from cx_dd_ddl_check
    "! @parameter rt_errors | Converted errors
    METHODS convert_ddl_errors
      IMPORTING it_ddl_errors    TYPE ddl2ddicerrors
      RETURNING VALUE(rt_errors) TYPE zif_abgagt_syntax_checker=>ty_errors.

    "! Convert DDL warnings to syntax checker warning format
    "! @parameter it_ddl_warnings | DDL warning table
    "! @parameter rt_warnings | Converted warnings
    METHODS convert_ddl_warnings
      IMPORTING it_ddl_warnings    TYPE ddl2ddicwarnings
      RETURNING VALUE(rt_warnings) TYPE zif_abgagt_syntax_checker=>ty_warnings.

ENDCLASS.


CLASS zcl_abgagt_syntax_chk_ddls IMPLEMENTATION.

  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'DDLS'.
  ENDMETHOD.


  METHOD zif_abgagt_syntax_checker~check.
    DATA: lo_handler  TYPE REF TO if_dd_ddl_handler,
          ls_ddlsrcv  TYPE ddddlsrcv,
          lt_warnings TYPE ddl2ddicwarnings.

    rs_result-object_type = 'DDLS'.
    rs_result-object_name = iv_name.

    " Validate input
    IF it_source IS INITIAL.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line = 1
        text = 'No source provided for syntax check' ) ).
      rs_result-message = 'No source provided'.
      RETURN.
    ENDIF.

    " Create DDL handler
    TRY.
        lo_handler = cl_dd_ddl_handler_factory=>create( ).
      CATCH cx_root INTO DATA(lx_factory).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-errors = VALUE #( (
          line = 1
          text = |DDL handler creation failed: { lx_factory->get_text( ) }| ) ).
        rs_result-message = 'DDL handler unavailable'.
        RETURN.
    ENDTRY.

    " Build DDLSRCV structure from source lines
    " This is the KEY TEST: Can we check without prior read()?
    ls_ddlsrcv-ddlname = CONV ddlname( iv_name ).
    ls_ddlsrcv-source = concat_lines_of(
      table = it_source
      sep = cl_abap_char_utilities=>newline ).
    ls_ddlsrcv-ddlanguage = sy-langu.
    ls_ddlsrcv-source_type = 'V'.  " V = View (default, may be overridden by DDL)

    " Run DDL syntax check
    TRY.
        lo_handler->check(
          EXPORTING
            name = ls_ddlsrcv-ddlname
          IMPORTING
            warnings = lt_warnings
          CHANGING
            ddlsrcv_wa = ls_ddlsrcv ).

        " Success - no syntax errors (but may have warnings)
        rs_result-success = abap_true.
        rs_result-error_count = 0.
        rs_result-warnings = convert_ddl_warnings( lt_warnings ).

        IF rs_result-warnings IS INITIAL.
          rs_result-message = 'Syntax check passed'.
        ELSE.
          rs_result-message = |Syntax check passed with { lines( rs_result-warnings ) } warning(s)|.
        ENDIF.

      CATCH cx_dd_ddl_check INTO DATA(lx_error).
        " DDL check failed - extract errors
        DATA(lt_ddl_errors) = lx_error->get_errors( ).

        rs_result-success = abap_false.
        rs_result-error_count = lines( lt_ddl_errors ).
        rs_result-errors = convert_ddl_errors( lt_ddl_errors ).

        " Get first error message
        IF lt_ddl_errors IS NOT INITIAL.
          DATA(ls_err) = lt_ddl_errors[ 1 ].
          MESSAGE ID ls_err-arbgb TYPE 'E' NUMBER ls_err-msgnr
            WITH ls_err-var1 ls_err-var2 ls_err-var3 ls_err-var4
            INTO rs_result-message.
        ELSE.
          rs_result-message = lx_error->get_text( ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_other).
        " Unexpected error
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-errors = VALUE #( (
          line = 1
          text = |Unexpected error: { lx_other->get_text( ) }| ) ).
        rs_result-message = 'Unexpected error during DDL check'.
    ENDTRY.
  ENDMETHOD.


  METHOD convert_ddl_errors.
    DATA lv_msg TYPE string.

    rt_errors = VALUE #( ).

    LOOP AT it_ddl_errors INTO DATA(ls_err).
      " Build readable error message
      MESSAGE ID ls_err-arbgb TYPE 'E' NUMBER ls_err-msgnr
        WITH ls_err-var1 ls_err-var2 ls_err-var3 ls_err-var4
        INTO lv_msg.

      APPEND VALUE #(
        line = ls_err-line
        column = ls_err-column
        text = lv_msg
        word = CONV string( ls_err-var1 )
        include = ''
        method_name = ''
      ) TO rt_errors.
    ENDLOOP.
  ENDMETHOD.


  METHOD convert_ddl_warnings.
    DATA lv_msg TYPE string.

    rt_warnings = VALUE #( ).

    LOOP AT it_ddl_warnings INTO DATA(ls_warn).
      " Build readable warning message
      MESSAGE ID ls_warn-arbgb TYPE 'W' NUMBER ls_warn-msgnr
        WITH ls_warn-var1 ls_warn-var2 ls_warn-var3 ls_warn-var4
        INTO lv_msg.

      APPEND VALUE #(
        line = ls_warn-line
        column = ls_warn-column
        text = lv_msg
        severity = ls_warn-severity
        include = ''
      ) TO rt_warnings.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
