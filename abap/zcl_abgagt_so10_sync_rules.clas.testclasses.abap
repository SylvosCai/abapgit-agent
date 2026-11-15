CLASS ltcl_so10_sync_rules DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CONSTANTS c_a TYPE string VALUE 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'.
    CONSTANTS c_b TYPE string VALUE 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'.
    CONSTANTS c_c TYPE string VALUE 'cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc'.
    METHODS expect_error IMPORTING iv_message TYPE string iv_action TYPE string DEFAULT 'UPDATE'
      iv_exists TYPE abap_bool DEFAULT abap_true iv_found TYPE abap_bool DEFAULT abap_true
      iv_current TYPE string DEFAULT c_a iv_desired TYPE string DEFAULT c_b
      iv_owner TYPE string DEFAULT 'my-app' iv_ignore TYPE abap_bool DEFAULT abap_false
      iv_managed TYPE abap_bool DEFAULT abap_true.
    METHODS digest_vectors FOR TESTING RAISING zcx_abapgit_exception.
    METHODS whitespace_matters FOR TESTING RAISING zcx_abapgit_exception.
    METHODS create_no_baseline FOR TESTING RAISING zcx_abapgit_exception.
    METHODS identical_create_fails FOR TESTING.
    METHODS missing_update_fails FOR TESTING.
    METHODS identical_unknown_fails FOR TESTING.
    METHODS owned_idempotence FOR TESTING RAISING zcx_abapgit_exception.
    METHODS safe_fast_forward FOR TESTING RAISING zcx_abapgit_exception.
    METHODS local_and_system_edits FOR TESTING.
    METHODS ignore_keeps_ownership FOR TESTING.
    METHODS ignore_missing_baseline FOR TESTING RAISING zcx_abapgit_exception.
    METHODS standalone_requires_ignore FOR TESTING.
    METHODS copy_is_not_git_update FOR TESTING.
    METHODS malformed_hash_fails FOR TESTING.
ENDCLASS.

CLASS ltcl_so10_sync_rules IMPLEMENTATION.
  METHOD expect_error.
    TRY.
        zcl_abgagt_so10_sync_rules=>check_write(
          iv_action = iv_action iv_exists = iv_exists iv_repo_id = 'my-app'
          iv_current_sha = iv_current iv_desired_sha = iv_desired
          is_baseline = VALUE #( found = iv_found repo_id = iv_owner source_sha = c_a )
          iv_ignore = iv_ignore iv_managed = iv_managed ).
        cl_abap_unit_assert=>fail( 'Expected SO10 conflict/validation error' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS iv_message ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD digest_vectors.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abgagt_so10_sync_rules=>fingerprint( VALUE #( ) )
      exp = '4f53cda18c2baa0c0354bb5f9a3ecbe5ed12ab4d8e11ba873c2f11161202b945' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abgagt_so10_sync_rules=>fingerprint( VALUE #( ( tdformat = '*' tdline = 'hello' ) ) )
      exp = 'b65181c01b75573cde621898e49a135c346e0f83a843c3768ee8dbebed9ee312' ).
  ENDMETHOD.

  METHOD whitespace_matters.
    DATA(lv_first) = zcl_abgagt_so10_sync_rules=>fingerprint( VALUE #( ( tdformat = '*' tdline = 'A B' ) ) ).
    DATA(lv_leading) = zcl_abgagt_so10_sync_rules=>fingerprint( VALUE #( ( tdformat = '*' tdline = ' A B' ) ) ).
    DATA(lv_interior) = zcl_abgagt_so10_sync_rules=>fingerprint( VALUE #( ( tdformat = '*' tdline = 'A  B' ) ) ).
    cl_abap_unit_assert=>assert_differs( act = lv_first exp = lv_leading ).
    cl_abap_unit_assert=>assert_differs( act = lv_first exp = lv_interior ).
  ENDMETHOD.

  METHOD create_no_baseline.
    cl_abap_unit_assert=>assert_true( zcl_abgagt_so10_sync_rules=>check_write(
      iv_action = 'CREATE' iv_exists = abap_false iv_repo_id = 'my-app'
      iv_current_sha = '' iv_desired_sha = c_b ) ).
  ENDMETHOD.

  METHOD identical_create_fails.
    expect_error( iv_action = 'CREATE' iv_current = c_a iv_desired = c_a iv_message = 'already exists' ).
  ENDMETHOD.

  METHOD missing_update_fails.
    expect_error( iv_exists = abap_false iv_message = 'missing' iv_ignore = abap_true ).
  ENDMETHOD.

  METHOD identical_unknown_fails.
    expect_error( iv_found = abap_false iv_current = c_a iv_desired = c_a iv_message = 'BASELINE_MISSING' ).
  ENDMETHOD.

  METHOD owned_idempotence.
    cl_abap_unit_assert=>assert_false( zcl_abgagt_so10_sync_rules=>check_write(
      iv_action = 'UPDATE' iv_exists = abap_true iv_repo_id = 'my-app'
      iv_current_sha = c_b iv_desired_sha = c_b
      is_baseline = VALUE #( found = abap_true repo_id = 'my-app' source_sha = c_a ) ) ).
  ENDMETHOD.

  METHOD safe_fast_forward.
    cl_abap_unit_assert=>assert_true( zcl_abgagt_so10_sync_rules=>check_write(
      iv_action = 'UPDATE' iv_exists = abap_true iv_repo_id = 'my-app'
      iv_current_sha = c_a iv_desired_sha = c_b
      is_baseline = VALUE #( found = abap_true repo_id = 'my-app' source_sha = c_a ) ) ).
  ENDMETHOD.

  METHOD local_and_system_edits.
    expect_error( iv_current = c_b iv_desired = c_a iv_message = 'LOCAL_EDIT' ).
    expect_error( iv_current = c_c iv_desired = c_b iv_message = 'SYSTEM_EDIT' ).
  ENDMETHOD.

  METHOD ignore_keeps_ownership.
    expect_error( iv_owner = 'another-repo' iv_ignore = abap_true iv_message = 'another repository' ).
    expect_error( iv_action = 'CREATE' iv_ignore = abap_true iv_message = 'already exists' ).
  ENDMETHOD.

  METHOD ignore_missing_baseline.
    cl_abap_unit_assert=>assert_true( zcl_abgagt_so10_sync_rules=>check_write(
      iv_action = 'UPDATE' iv_exists = abap_true iv_repo_id = 'my-app'
      iv_current_sha = c_a iv_desired_sha = c_b iv_ignore = abap_true ) ).
  ENDMETHOD.

  METHOD standalone_requires_ignore.
    expect_error( iv_managed = abap_false iv_message = 'explicit conflict-ignore' ).
  ENDMETHOD.

  METHOD copy_is_not_git_update.
    expect_error( iv_action = 'COPY' iv_message = 'only to CREATE and UPDATE' ).
  ENDMETHOD.

  METHOD malformed_hash_fails.
    expect_error( iv_current = 'sha_40_weak' iv_ignore = abap_true iv_message = 'SHA-256' ).
  ENDMETHOD.
ENDCLASS.
