"! <p class="shorttext synchronized">Syntax Checker for DCLS (CDS Access Control)</p>
"! Checks DCLS (CDS access control) source code syntax using CL_ACM_DCL_HANDLER.
"! Handler is acquired dynamically so the checker still loads on systems without ACM.
CLASS zcl_abgagt_syntax_chk_dcls DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

  PRIVATE SECTION.

    "! Convert ACM parser errors (table) to syntax checker error format
    "! @parameter it_parser_errors | Standard table of CL_ACMDTAPI_DCL_PARSER=>TY_PARSER_ERROR_S
    "! @parameter rt_errors | Converted errors
    METHODS convert_parser_errors
      IMPORTING it_parser_errors TYPE STANDARD TABLE
      RETURNING VALUE(rt_errors) TYPE zif_abgagt_syntax_checker=>ty_errors.

ENDCLASS.

CLASS zcl_abgagt_syntax_chk_dcls IMPLEMENTATION.

  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'DCLS'.
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~set_fixpt.
    " DCLS doesn't use FIXPT flag - do nothing
  ENDMETHOD.

  METHOD zif_abgagt_syntax_checker~check.
    DATA: lo_handler TYPE REF TO object,
          lr_errors  TYPE REF TO data.

    FIELD-SYMBOLS <lt_errors> TYPE STANDARD TABLE.

    rs_result-object_type = 'DCLS'.
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

    " Acquire ACM DCL handler dynamically
    " Pattern lifted from ZCL_ABAPGIT_OBJECT_DCLS=>CONSTRUCTOR
    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_handler.
      CATCH cx_root INTO DATA(lx_factory).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-errors = VALUE #( (
          line = 1
          text = |ACM DCL handler unavailable on this system: { lx_factory->get_text( ) }| ) ).
        rs_result-message = 'ACM DCL handler unavailable'.
        RETURN.
    ENDTRY.

    " Build single source string (CL_ACM_DCL_HANDLER->CHECK takes ACM_DCLSOURCE, a string)
    DATA(lv_source) = concat_lines_of(
      table = it_source
      sep   = cl_abap_char_utilities=>newline ).

    " IV_DCLNAME is typed ACM_DCLNAME (CHAR 40) — convert via dynamic typed
    " variable so this class still loads on systems without ACM.
    DATA lr_dclname TYPE REF TO data.
    FIELD-SYMBOLS <lv_dclname> TYPE any.
    TRY.
        CREATE DATA lr_dclname TYPE ('ACM_DCLNAME').
        ASSIGN lr_dclname->* TO <lv_dclname>.
        <lv_dclname> = iv_name.
      CATCH cx_root INTO DATA(lx_name_type).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-errors = VALUE #( (
          line = 1
          text = |ACM_DCLNAME type unavailable: { lx_name_type->get_text( ) }| ) ).
        rs_result-message = 'ACM_DCLNAME type unavailable'.
        RETURN.
    ENDTRY.

    " Parser-error table type — created dynamically so a system without ACM
    " can still load this class definition.
    TRY.
        CREATE DATA lr_errors TYPE ('CL_ACMDTAPI_DCL_PARSER=>TY_PARSER_ERROR_T').
        ASSIGN lr_errors->* TO <lt_errors>.
      CATCH cx_root INTO DATA(lx_type).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-errors = VALUE #( (
          line = 1
          text = |ACM parser type unavailable: { lx_type->get_text( ) }| ) ).
        rs_result-message = 'ACM parser type unavailable'.
        RETURN.
    ENDTRY.

    " Run DCL syntax check
    TRY.
        CALL METHOD lo_handler->('CHECK')
          EXPORTING
            iv_dclname    = <lv_dclname>
            iv_dclsrc     = lv_source
            iv_check_only = abap_true
          IMPORTING
            et_parser_errors = <lt_errors>.

        IF lines( <lt_errors> ) = 0.
          rs_result-success = abap_true.
          rs_result-error_count = 0.
          rs_result-message = 'Syntax check passed'.
        ELSE.
          " CHECK can return non-empty et_parser_errors WITHOUT raising — treat as failure
          rs_result-success = abap_false.
          rs_result-error_count = lines( <lt_errors> ).
          rs_result-errors = convert_parser_errors( <lt_errors> ).
          rs_result-message = |Syntax check failed with { rs_result-error_count } error(s)|.
        ENDIF.

      CATCH cx_root INTO DATA(lx_check).
        " CX_ACM_DCL_CHECK or any other failure — try to extract errors first,
        " then fall back to the exception text.
        rs_result-success = abap_false.

        IF <lt_errors> IS ASSIGNED AND lines( <lt_errors> ) > 0.
          rs_result-error_count = lines( <lt_errors> ).
          rs_result-errors = convert_parser_errors( <lt_errors> ).
          rs_result-message = |Syntax check failed with { rs_result-error_count } error(s)|.
        ELSE.
          rs_result-error_count = 1.
          rs_result-errors = VALUE #( (
            line = 1
            text = lx_check->get_text( ) ) ).
          rs_result-message = lx_check->get_text( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD convert_parser_errors.
    DATA lv_msg TYPE string.

    FIELD-SYMBOLS <ls_err> TYPE any.
    FIELD-SYMBOLS <lv_line> TYPE i.
    FIELD-SYMBOLS <lv_column> TYPE i.
    FIELD-SYMBOLS <lv_text_for_debug> TYPE string.
    FIELD-SYMBOLS <ls_symsg> TYPE symsg.

    rt_errors = VALUE #( ).

    LOOP AT it_parser_errors ASSIGNING <ls_err>.
      CLEAR lv_msg.

      " Build readable text from symsg fields if present (INCLUDE TYPE symsg AS symsg).
      ASSIGN COMPONENT 'SYMSG' OF STRUCTURE <ls_err> TO <ls_symsg>.
      IF sy-subrc = 0 AND <ls_symsg>-msgid IS NOT INITIAL.
        MESSAGE ID <ls_symsg>-msgid TYPE 'E' NUMBER <ls_symsg>-msgno
          WITH <ls_symsg>-msgv1 <ls_symsg>-msgv2 <ls_symsg>-msgv3 <ls_symsg>-msgv4
          INTO lv_msg.
      ENDIF.

      " If MESSAGE didn't yield text, fall back to TEXT_FOR_DEBUG.
      IF lv_msg IS INITIAL.
        ASSIGN COMPONENT 'TEXT_FOR_DEBUG' OF STRUCTURE <ls_err> TO <lv_text_for_debug>.
        IF sy-subrc = 0.
          lv_msg = <lv_text_for_debug>.
        ENDIF.
      ENDIF.

      DATA lv_line   TYPE i.
      DATA lv_column TYPE i.
      CLEAR lv_line.
      CLEAR lv_column.
      ASSIGN COMPONENT 'LINE' OF STRUCTURE <ls_err> TO <lv_line>.
      IF sy-subrc = 0.
        lv_line = <lv_line>.
      ENDIF.
      ASSIGN COMPONENT 'COLUMN' OF STRUCTURE <ls_err> TO <lv_column>.
      IF sy-subrc = 0.
        lv_column = <lv_column>.
      ENDIF.

      APPEND VALUE #(
        line = lv_line
        column = lv_column
        text = lv_msg
        word = ''
        include = ''
        method_name = ''
      ) TO rt_errors.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
