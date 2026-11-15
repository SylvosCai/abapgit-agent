CLASS ltcl_so10_sync_store DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA mo_environment TYPE REF TO if_osql_test_environment.
    DATA mo_store TYPE REF TO zif_abgagt_so10_sync_store.
    CONSTANTS c_a TYPE string VALUE 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'.
    CONSTANTS c_b TYPE string VALUE 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'.
    CLASS-METHODS class_setup RAISING cx_static_check.
    CLASS-METHODS class_teardown.
    METHODS setup.
    METHODS missing_baseline FOR TESTING RAISING zcx_abapgit_exception.
    METHODS full_key_and_clients FOR TESTING RAISING zcx_abapgit_exception.
    METHODS same_owner_advances FOR TESTING RAISING zcx_abapgit_exception.
    METHODS foreign_owner_protected FOR TESTING RAISING zcx_abapgit_exception.
    METHODS bad_key_never_written FOR TESTING.
    METHODS weak_hash_never_written FOR TESTING.
    METHODS diagnostic_markers FOR TESTING RAISING zcx_abapgit_exception.
    METHODS removes_exact_owned_baseline FOR TESTING RAISING zcx_abapgit_exception.
    METHODS removal_guard_preserves_row FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_so10_sync_store IMPLEMENTATION.
  METHOD removes_exact_owned_baseline.
    DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = 'TEXT' name = 'ZTEST' id = 'ST' language = 'E' ).
    mo_store->record( iv_data_client = '010' is_key = ls_key iv_repo_id = 'my-repo' iv_source_sha = c_a ).
    mo_store->record( iv_data_client = '020' is_key = ls_key iv_repo_id = 'my-repo' iv_source_sha = c_a ).
    mo_store->remove( iv_data_client = '010' is_key = ls_key iv_repo_id = 'my-repo' iv_source_sha = c_a ).
    DATA(ls_removed) = mo_store->load( iv_data_client = '010' is_key = ls_key ).
    DATA(ls_retained) = mo_store->load( iv_data_client = '020' is_key = ls_key ).
    cl_abap_unit_assert=>assert_false( ls_removed-found ).
    cl_abap_unit_assert=>assert_true( ls_retained-found ).
  ENDMETHOD.
  METHOD removal_guard_preserves_row.
    DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = 'TEXT' name = 'ZTEST' id = 'ST' language = 'E' ).
    mo_store->record( iv_data_client = '010' is_key = ls_key iv_repo_id = 'my-repo' iv_source_sha = c_a ).
    TRY.
        mo_store->remove( iv_data_client = '010' is_key = ls_key iv_repo_id = 'other-repo' iv_source_sha = c_a ).
        cl_abap_unit_assert=>fail( 'Foreign ownership must not remove the baseline' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
    TRY.
        mo_store->remove( iv_data_client = '010' is_key = ls_key iv_repo_id = 'my-repo' iv_source_sha = c_b ).
        cl_abap_unit_assert=>fail( 'A changed fingerprint must not remove the baseline' ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
    DATA(ls_retained) = mo_store->load( iv_data_client = '010' is_key = ls_key ).
    cl_abap_unit_assert=>assert_true( ls_retained-found ).
    cl_abap_unit_assert=>assert_equals( act = ls_retained-source_sha exp = c_a ).
  ENDMETHOD.
  METHOD class_setup.
    mo_environment = cl_osql_test_environment=>create( VALUE #( ( 'ZABGAGT_SO10META' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mo_environment->clear_doubles( ).
    mo_store = NEW zcl_abgagt_so10_sync_store( ).
  ENDMETHOD.

  METHOD missing_baseline.
    DATA(ls_baseline) = mo_store->load( iv_data_client = '010'
      is_key = VALUE #( object = 'TEXT' name = 'ZTEST' id = 'ST' language = 'E' ) ).
    cl_abap_unit_assert=>assert_false( ls_baseline-found ).
  ENDMETHOD.

  METHOD full_key_and_clients.
    DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key(
      object = 'TEXT' name = repeat( val = `Z` occ = 70 ) id = 'SYST' language = 'E' ).
    mo_store->record( iv_data_client = '010' is_key = ls_key iv_repo_id = 'one-repo' iv_source_sha = c_a ).
    mo_store->record( iv_data_client = '020' is_key = ls_key iv_repo_id = 'another-repo' iv_source_sha = c_b ).
    DATA(ls_first) = mo_store->load( iv_data_client = '010' is_key = ls_key ).
    DATA(ls_second) = mo_store->load( iv_data_client = '020' is_key = ls_key ).
    cl_abap_unit_assert=>assert_equals( act = ls_first-source_sha exp = c_a ).
    cl_abap_unit_assert=>assert_equals( act = ls_second-source_sha exp = c_b ).
    ls_key-id = 'ST'.
    DATA(ls_other_id) = mo_store->load( iv_data_client = '010' is_key = ls_key ).
    cl_abap_unit_assert=>assert_false( ls_other_id-found ).
    ls_key-id = 'SYST'.
    ls_key-language = 'D'.
    DATA(ls_other_language) = mo_store->load( iv_data_client = '010' is_key = ls_key ).
    cl_abap_unit_assert=>assert_false( ls_other_language-found ).
  ENDMETHOD.

  METHOD same_owner_advances.
    DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = 'TEXT' name = 'ZTEST' id = 'ST' language = 'E' ).
    mo_store->record( iv_data_client = '010' is_key = ls_key iv_repo_id = 'my-repo' iv_source_sha = c_a ).
    mo_store->record( iv_data_client = '010' is_key = ls_key iv_repo_id = 'my-repo' iv_source_sha = c_b ).
    DATA(ls_baseline) = mo_store->load( iv_data_client = '010' is_key = ls_key ).
    cl_abap_unit_assert=>assert_equals( act = ls_baseline-repo_id exp = 'my-repo' ).
    cl_abap_unit_assert=>assert_equals( act = ls_baseline-source_sha exp = c_b ).
  ENDMETHOD.

  METHOD foreign_owner_protected.
    DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = 'TEXT' name = 'ZTEST' id = 'ST' language = 'E' ).
    mo_store->record( iv_data_client = '010' is_key = ls_key iv_repo_id = 'one-repo' iv_source_sha = c_a ).
    TRY.
        mo_store->record( iv_data_client = '010' is_key = ls_key iv_repo_id = 'another-repo' iv_source_sha = c_b ).
        cl_abap_unit_assert=>fail( 'Expected repository ownership rejection' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'baseline' ) ).
    ENDTRY.
    DATA(ls_baseline) = mo_store->load( iv_data_client = '010' is_key = ls_key ).
    cl_abap_unit_assert=>assert_equals( act = ls_baseline-repo_id exp = 'one-repo' ).
    cl_abap_unit_assert=>assert_equals( act = ls_baseline-source_sha exp = c_a ).
  ENDMETHOD.

  METHOD bad_key_never_written.
    TRY.
        mo_store->record( iv_data_client = '010' iv_repo_id = 'my-repo' iv_source_sha = c_a
          is_key = VALUE #( object = 'TEXT' name = repeat( val = `Z` occ = 71 ) id = 'ST' language = 'E' ) ).
        cl_abap_unit_assert=>fail( 'Overlong key must fail before conversion' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'key' ) ).
    ENDTRY.
    SELECT COUNT( * ) FROM zabgagt_so10meta INTO @DATA(lv_count).
    cl_abap_unit_assert=>assert_equals( act = lv_count exp = 0 ).
  ENDMETHOD.

  METHOD diagnostic_markers.
    DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = 'TEXT' name = 'ZTEST' id = 'ST' language = 'E' ).
    mo_store->record( iv_data_client = '010' is_key = ls_key iv_repo_id = 'my-repo' iv_source_sha = c_a
      is_header = VALUE #( tdluser = 'TESTUSER' tdldate = '20200101' tdltime = '120000' ) ).
    DATA(ls_baseline) = mo_store->load( iv_data_client = '010' is_key = ls_key ).
    cl_abap_unit_assert=>assert_equals( act = ls_baseline-source_user exp = 'TESTUSER' ).
    cl_abap_unit_assert=>assert_equals( act = ls_baseline-source_date exp = '20200101' ).
    cl_abap_unit_assert=>assert_equals( act = ls_baseline-source_time exp = '120000' ).
    cl_abap_unit_assert=>assert_equals( act = ls_baseline-source_sha exp = c_a ).
  ENDMETHOD.

  METHOD weak_hash_never_written.
    TRY.
        mo_store->record( iv_data_client = '010' iv_repo_id = 'my-repo' iv_source_sha = 'sha_40_prefix'
          is_key = VALUE #( object = 'TEXT' name = 'ZTEST' id = 'ST' language = 'E' ) ).
        cl_abap_unit_assert=>fail( 'Weak fallback hashes must not be stored' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'fingerprint' ) ).
    ENDTRY.
    SELECT COUNT( * ) FROM zabgagt_so10meta INTO @DATA(lv_count).
    cl_abap_unit_assert=>assert_equals( act = lv_count exp = 0 ).
  ENDMETHOD.
ENDCLASS.
