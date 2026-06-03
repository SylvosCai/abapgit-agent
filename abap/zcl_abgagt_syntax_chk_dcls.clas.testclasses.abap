"! <p class="shorttext synchronized">Test DCLS Syntax Checker</p>
"! Verifies that ZCL_ABGAGT_SYNTAX_CHK_DCLS correctly reports syntax errors via
"! CL_ACM_DCL_HANDLER->CHECK and converts ACM parser errors into the standard
"! syntax-checker result structure (line, column, text).
"!
"! Note: CL_ACM_DCL_HANDLER->CHECK requires the granted-on DDL view to exist
"! on the system. Tests that need a "valid" role reference ZC_ABGAGT_DRP, which
"! is activated as part of the drop-test repo setup. If that DDL view is not
"! available, the valid-role test is skipped via early return — failure-path
"! tests still run (they don't depend on any DDL).
CLASS ltc_dcls_syntax_checker DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_syntax_chk_dcls.

    METHODS setup.

    "! get_object_type returns 'DCLS'
    METHODS test_object_type_is_dcls FOR TESTING.

    "! Empty source returns success=false with a non-empty message
    METHODS test_empty_source FOR TESTING.

    "! Valid DCL role against an existing DDL view passes (skipped if view absent)
    METHODS test_valid_role_passes FOR TESTING.

    "! Reference to non-existent CDS entity yields a parser error with line/column
    METHODS test_unknown_entity_fails FOR TESTING.

    "! Garbage keyword instead of DEFINE ROLE yields a parser error
    METHODS test_invalid_keyword_fails FOR TESTING.

    "! Helper: convert the granted-on view to a string_table source
    METHODS build_source
      IMPORTING iv_source        TYPE string
      RETURNING VALUE(rt_source) TYPE string_table.

    "! Returns true when the pre-existing test DDL view ZC_ABGAGT_DRP exists
    METHODS dependency_view_active
      RETURNING VALUE(rv_yes) TYPE abap_bool.

ENDCLASS.

CLASS ltc_dcls_syntax_checker IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD test_object_type_is_dcls.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->zif_abgagt_syntax_checker~get_object_type( )
      exp = 'DCLS'
      msg = 'get_object_type must return DCLS' ).
  ENDMETHOD.

  METHOD test_empty_source.
    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZTEST_EMPTY_DCL'
      it_source = VALUE string_table( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_type
      exp = 'DCLS'
      msg = 'object_type must be DCLS' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'empty source must yield success=false' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-message
      msg = 'empty source must produce a message' ).
  ENDMETHOD.

  METHOD test_valid_role_passes.
    " Skip if the dependency view is not active on this system —
    " the parser refuses to validate against a non-existent DDL.
    IF dependency_view_active( ) = abap_false.
      RETURN.
    ENDIF.

    DATA(lt_source) = build_source(
      |@EndUserText.label: 'Test'\n| &&
      |@MappingRole: true\n| &&
      |define role zc_test_valid_dcl \{\n| &&
      |  grant select on zc_abgagt_drp;\n| &&
      |\}| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZC_TEST_VALID_DCL'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_true
      msg = |valid DCL must pass: { ls_result-message }| ).

    cl_abap_unit_assert=>assert_initial(
      act = ls_result-error_count
      msg = 'valid DCL must report 0 errors' ).
  ENDMETHOD.

  METHOD test_unknown_entity_fails.
    " A grant on a non-existent CDS entity is a real parser error and does NOT
    " depend on any other object — safe to assert unconditionally.
    DATA(lt_source) = build_source(
      |@EndUserText.label: 'Bad'\n| &&
      |define role ZTEST_UNK_DCL \{\n| &&
      |  grant select on zc_view_that_does_not_exist;\n| &&
      |\}| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZTEST_UNK_DCL'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'unknown CDS entity must fail' ).

    cl_abap_unit_assert=>assert_differs(
      act = ls_result-error_count
      exp = 0
      msg = 'must report at least one error' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-errors
      msg = 'errors table must be populated' ).

    DATA(ls_first) = ls_result-errors[ 1 ].
    cl_abap_unit_assert=>assert_differs(
      act = ls_first-line
      exp = 0
      msg = 'first error must carry a non-zero line number' ).
  ENDMETHOD.

  METHOD test_invalid_keyword_fails.
    " Garbage in place of DEFINE ROLE — purely parser-level error.
    DATA(lt_source) = build_source(
      |INVALID_KEYWORD ZTEST_BAD_KW \{\n| &&
      |  grant select on zc_abgagt_drp;\n| &&
      |\}| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name   = 'ZTEST_BAD_KW'
      it_source = lt_source ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'invalid keyword must fail' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-errors
      msg = 'errors must be populated for invalid keyword' ).
  ENDMETHOD.

  METHOD build_source.
    DATA(lv_source) = iv_source.
    REPLACE ALL OCCURRENCES OF |\n| IN lv_source WITH cl_abap_char_utilities=>newline.
    SPLIT lv_source AT cl_abap_char_utilities=>newline INTO TABLE rt_source.
  ENDMETHOD.

  METHOD dependency_view_active.
    rv_yes = abap_false.
    SELECT SINGLE @abap_true FROM tadir
      INTO @rv_yes
      WHERE pgmid    = 'R3TR'
        AND object   = 'DDLS'
        AND obj_name = 'ZC_ABGAGT_DRP'.
  ENDMETHOD.

ENDCLASS.
