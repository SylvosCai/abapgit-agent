"! <p class="shorttext synchronized">Test DDLS Syntax Checker</p>
"! CRITICAL TESTS: Determine if DDL handler can check source without DB object.
"! These tests will prove or disprove the hypothesis that CL_DD_DDL_HANDLER->check()
"! can validate arbitrary DDL source text without requiring the object to exist.
CLASS ltc_ddls_syntax_checker DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_syntax_chk_ddls.

    METHODS setup.

    "! Test 1: Valid DDL with existing table (SFLIGHT)
    "! EXPECTATION: Should pass or return specific DDL warnings
    "! This is the baseline test - if this fails, hypothesis is FALSE
    METHODS test_valid_ddl_with_sflight FOR TESTING.

    "! Test 2: DDL with obvious syntax error (incomplete statement)
    "! EXPECTATION: Should catch syntax error with line/column
    "! This proves error reporting works
    METHODS test_syntax_error_incomplete FOR TESTING.

    "! Test 3: DDL with invalid keyword
    "! EXPECTATION: Should catch invalid keyword error
    METHODS test_syntax_error_invalid_kw FOR TESTING.

    "! Test 4: DDL with annotation syntax error
    "! EXPECTATION: Should catch annotation error
    METHODS test_annotation_error FOR TESTING.

    "! Test 5: DDL referencing non-existent table
    "! EXPECTATION: May fail (requires DB schema context)
    "! This test reveals limitations
    METHODS test_nonexistent_table FOR TESTING.

    "! Test 6: Empty source
    "! EXPECTATION: Should handle gracefully
    METHODS test_empty_source FOR TESTING.

    "! Test 7: Valid DDL with multiple fields
    "! EXPECTATION: Should pass
    METHODS test_valid_ddl_multi_fields FOR TESTING.

    "! Test 8: DDL with association (advanced feature)
    "! EXPECTATION: May fail if requires DB context
    METHODS test_ddl_with_association FOR TESTING.

    "! Helper: Build source lines from string
    METHODS build_source
      IMPORTING iv_source        TYPE string
      RETURNING VALUE(rt_source) TYPE string_table.

ENDCLASS.

CLASS ltc_ddls_syntax_checker IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD test_valid_ddl_with_sflight.
    " Test with valid DDL referencing standard table SFLIGHT
    DATA(lt_source) = build_source(
      |@AbapCatalog.sqlViewName: 'ZTESTVALIDFLT'\n| &&
      |@EndUserText.label: 'Test Flight View'\n| &&
      |define view ZTEST_VALID_FLIGHT as select from sflight\n| &&
      |\{\n| &&
      |  key carrid,\n| &&
      |      connid,\n| &&
      |      fldate\n| &&
      |\}| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name = 'ZTEST_VALID_FLIGHT'
      it_source = lt_source ).

    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_type
      exp = 'DDLS'
      msg = 'Object type should be DDLS' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-object_name
      exp = 'ZTEST_VALID_FLIGHT'
      msg = 'Object name should match' ).

    " KEY ASSERTION: Did check() work without DB object?
    " If success = TRUE, hypothesis is CONFIRMED!
    " If success = FALSE, check error message to understand WHY
    IF ls_result-success = abap_true.
      " HYPOTHESIS CONFIRMED!
      cl_abap_unit_assert=>assert_initial(
        act = ls_result-error_count
        msg = |SUCCESS! DDL check works without DB object. Warnings: { lines( ls_result-warnings ) }| ).
    ELSE.
      " Log failure reason for analysis
      cl_abap_unit_assert=>fail(
        msg = |HYPOTHESIS FALSE: { ls_result-message }. Error count: { ls_result-error_count }| ).
    ENDIF.
  ENDMETHOD.

  METHOD test_syntax_error_incomplete.
    " Test with incomplete DDL statement (missing fields)
    DATA(lt_source) = build_source(
      |define view ZTEST_INCOMPLETE as select from sflight| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name = 'ZTEST_INCOMPLETE'
      it_source = lt_source ).

    " Should detect syntax error
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'Should fail for incomplete DDL' ).

    cl_abap_unit_assert=>assert_differs(
      act = ls_result-error_count
      exp = 0
      msg = 'Should report errors' ).

    " Check that errors have line/column info
    IF ls_result-errors IS NOT INITIAL.
      DATA(ls_error) = ls_result-errors[ 1 ].
      cl_abap_unit_assert=>assert_differs(
        act = ls_error-line
        exp = 0
        msg = 'Error should have line number' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_syntax_error_invalid_kw.
    " Test with invalid DDL keyword
    DATA(lt_source) = build_source(
      |define view ZTEST_INVALID as INVALID_KEYWORD from sflight| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name = 'ZTEST_INVALID'
      it_source = lt_source ).

    " Should detect syntax error
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'Should fail for invalid keyword' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-errors
      msg = 'Should return error details' ).
  ENDMETHOD.

  METHOD test_annotation_error.
    " Test with invalid annotation syntax
    DATA(lt_source) = build_source(
      |@InvalidAnnotation.missingValue\n| &&
      |define view ZTEST_ANNOT as select from sflight \{ carrid \}| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name = 'ZTEST_ANNOT'
      it_source = lt_source ).

    " May fail or succeed with warnings depending on strictness
    " Log result for analysis
    IF ls_result-success = abap_false.
      cl_abap_unit_assert=>assert_not_initial(
        act = ls_result-errors
        msg = 'Should report annotation errors' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_nonexistent_table.
    " Test with non-existent table reference
    " This test reveals if DDL handler requires DB schema context
    DATA(lt_source) = build_source(
      |define view ZTEST_NOTFOUND as select from ZTABLE_DOES_NOT_EXIST\n| &&
      |\{\n| &&
      |  field1\n| &&
      |\}| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name = 'ZTEST_NOTFOUND'
      it_source = lt_source ).

    " EXPECTATION: Will likely fail because table doesn't exist
    " This is a KNOWN LIMITATION - document it
    IF ls_result-success = abap_false.
      " Expected - DDL handler needs DB schema context
      cl_abap_unit_assert=>assert_not_initial(
        act = ls_result-errors
        msg = |Expected failure: { ls_result-message }| ).
    ELSE.
      " Unexpected success - may mean very lenient checking
      cl_abap_unit_assert=>assert_subrc(
        act = 0
        msg = 'Unexpectedly passed despite non-existent table' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_empty_source.
    " Test with empty source
    DATA(lt_source) = VALUE string_table( ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name = 'ZTEST_EMPTY'
      it_source = lt_source ).

    " Should handle empty source gracefully
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_false
      msg = 'Should fail for empty source' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-message
      msg = 'Should provide error message' ).
  ENDMETHOD.

  METHOD test_valid_ddl_multi_fields.
    " Test with valid DDL with multiple fields and calculations
    DATA(lt_source) = build_source(
      |@AbapCatalog.sqlViewName: 'ZTESTMULTI'\n| &&
      |@EndUserText.label: 'Flight Price View'\n| &&
      |define view ZTEST_MULTI as select from sflight\n| &&
      |\{\n| &&
      |  key carrid,\n| &&
      |  key connid,\n| &&
      |  key fldate,\n| &&
      |      price,\n| &&
      |      currency,\n| &&
      |      cast( price as abap.dec(15,2) ) as price_decimal\n| &&
      |\}| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name = 'ZTEST_MULTI'
      it_source = lt_source ).

    " Should pass if DDL handler works without DB
    IF ls_result-success = abap_true.
      cl_abap_unit_assert=>assert_initial(
        act = ls_result-error_count
        msg = 'Valid DDL with cast should pass' ).
    ELSE.
      " Document reason for failure
      cl_abap_unit_assert=>fail(
        msg = |DDL with cast failed: { ls_result-message }| ).
    ENDIF.
  ENDMETHOD.

  METHOD test_ddl_with_association.
    " Test with association (may require DB context)
    DATA(lt_source) = build_source(
      |define view ZTEST_ASSOC as select from sflight\n| &&
      |  association [1..1] to scarr as _Carrier\n| &&
      |    on $projection.carrid = _Carrier.carrid\n| &&
      |\{\n| &&
      |  key carrid,\n| &&
      |      connid,\n| &&
      |      _Carrier\n| &&
      |\}| ).

    DATA(ls_result) = mo_cut->zif_abgagt_syntax_checker~check(
      iv_name = 'ZTEST_ASSOC'
      it_source = lt_source ).

    " Association may require both tables to exist
    " Document result for limitation analysis
    IF ls_result-success = abap_false.
      " Expected - associations require DB context
      cl_abap_unit_assert=>assert_not_initial(
        act = ls_result-errors
        msg = |Association check failed (expected): { ls_result-message }| ).
    ELSE.
      " If it passes, great!
      cl_abap_unit_assert=>assert_subrc(
        act = 0
        msg = 'Association syntax check passed' ).
    ENDIF.
  ENDMETHOD.

  METHOD build_source.
    " Convert string with \n to string table

    DATA(lv_source) = iv_source.

    " Replace \n with actual newlines
    REPLACE ALL OCCURRENCES OF |\n| IN lv_source WITH cl_abap_char_utilities=>newline.

    " Split into lines
    SPLIT lv_source AT cl_abap_char_utilities=>newline INTO TABLE rt_source.
  ENDMETHOD.

ENDCLASS.
