"! PoC: SYNTAX-CHECK FOR ITAB
"! Demonstrates how to check ABAP syntax from source code without activation
"! Uses SYNTAX-CHECK FOR ITAB with error collection

CLASS zcl_abapgit_agent_syntax_poc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

  PRIVATE SECTION.
    METHODS: check_syntax IMPORTING source_code TYPE string_table
                      RETURNING VALUE(rt_errors) TYPE string_table.
ENDCLASS.


CLASS zcl_abapgit_agent_syntax_poc IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " Example 1: Valid source code
    DATA(lt_valid_code) = VALUE string_table(
      ( `CLASS zcl_demo DEFINITION.` )
      ( `  PUBLIC SECTION.` )
      ( `ENDCLASS.` ) ).

    " Example 2: Invalid source code (syntax error - missing period)
    DATA(lt_invalid_code) = VALUE string_table(
      ( `CLASS zcl_demo DEFINITION.` )
      ( `  PUBLIC SECTION.` )
      ( `DATA lv_test TYPE string` ) " Missing period
      ( `ENDCLASS.` ) ).

    " Example 3: Invalid source code (undefined variable)
    DATA(lt_undefined_var) = VALUE string_table(
      ( `CLASS zcl_demo DEFINITION.` )
      ( `  PUBLIC SECTION.` )
      ( `ENDCLASS.` )
      ( `CLASS zcl_demo IMPLEMENTATION.` )
      ( `  METHOD test.` )
      ( `    lv_undefined = 'test'.` ) " Variable not declared
      ( `  ENDMETHOD.` )
      ( `ENDCLASS.` ) ).

    out->write( |=== PoC: SYNTAX-CHECK FOR ITAB ===| ).
    out->write( |\n| ).

    " Test 1: Valid code
    out->write( |--- Test 1: Valid Source Code ---| ).
    DATA(lt_errors) = check_syntax( lt_valid_code ).
    IF lt_errors IS INITIAL.
      out->write( |Result: No syntax errors| COLOR COL_POSITIVE ).
    ELSE.
      out->write( |Result: Errors found: { lines( lt_errors ) }| COLOR COL_NEGATIVE ).
      LOOP AT lt_errors INTO DATA(lv_err).
        out->write( |  { lv_err }| ).
      ENDLOOP.
    ENDIF.
    out->write( |\n| ).

    " Test 2: Missing period
    out->write( |--- Test 2: Missing Period ---| ).
    lt_errors = check_syntax( lt_invalid_code ).
    IF lt_errors IS INITIAL.
      out->write( |Result: No syntax errors| COLOR COL_POSITIVE ).
    ELSE.
      out->write( |Result: Errors found: { lines( lt_errors ) }| COLOR COL_NEGATIVE ).
      LOOP AT lt_errors INTO lv_err.
        out->write( |  { lv_err }| ).
      ENDLOOP.
    ENDIF.
    out->write( |\n| ).

    " Test 3: Undefined variable
    out->write( |--- Test 3: Undefined Variable ---| ).
    lt_errors = check_syntax( lt_undefined_var ).
    IF lt_errors IS INITIAL.
      out->write( |Result: No syntax errors| COLOR COL_POSITIVE ).
    ELSE.
      out->write( |Result: Errors found: { lines( lt_errors ) }| COLOR COL_NEGATIVE ).
      LOOP AT lt_errors INTO lv_err.
        out->write( |  { lv_err }| ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD check_syntax.

    " SYNTAX-CHECK FOR ITAB parameters
    DATA: lv_word     TYPE string,           " Error message
          lv_line     TYPE i,                " Line number
          lt_errtab   TYPE TABLE OF scsynerr. " Error table

    " Perform syntax check with error collection
    " ID 'ERR' TABLE - collects all errors into internal table
    " Continues checking after errors (doesn't stop at first error)
    SYNTAX-CHECK FOR source_code
      MESSAGE lv_word
      LINE lv_line
      ID 'ERR' TABLE lt_errtab
      PROGRAM sy-repid.

    " sy-subrc = 0: No errors
    " sy-subrc = 4: Errors collected in lt_errtab
    IF sy-subrc <> 0.
      " Convert errors to readable format
      LOOP AT lt_errtab INTO DATA(ls_err).
        APPEND |Line { ls_err-line }, Col { ls_err-col }: { ls_err-errortext }| TO rt_errors.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
