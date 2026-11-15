CLASS lcl_so10_drop_arguments DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.
    METHODS constructor IMPORTING iv_client TYPE sy-mandt.
  PRIVATE SECTION.
    DATA mv_client TYPE sy-mandt.
ENDCLASS.
CLASS lcl_so10_drop_arguments IMPLEMENTATION.
  METHOD constructor.
    mv_client = iv_client.
  ENDMETHOD.
  METHOD if_ftd_invocation_answer~answer.
    DATA(lr_client) = arguments->get_importing_parameter( 'CLIENT' ).
    ASSIGN lr_client->* TO FIELD-SYMBOL(<lv_client>).
    cl_abap_unit_assert=>assert_equals( act = <lv_client> exp = mv_client ).
    DATA(lr_name) = arguments->get_importing_parameter( 'NAME' ).
    ASSIGN lr_name->* TO FIELD-SYMBOL(<lv_name>).
    cl_abap_unit_assert=>assert_equals( act = <lv_name> exp = 'ZSO10_TEST' ).
    DATA(lr_object) = arguments->get_importing_parameter( 'OBJECT' ).
    ASSIGN lr_object->* TO FIELD-SYMBOL(<lv_object>).
    cl_abap_unit_assert=>assert_equals( act = <lv_object> exp = 'TEXT' ).
    DATA(lr_id) = arguments->get_importing_parameter( 'ID' ).
    ASSIGN lr_id->* TO FIELD-SYMBOL(<lv_id>).
    cl_abap_unit_assert=>assert_equals( act = <lv_id> exp = 'ST' ).
    DATA(lr_language) = arguments->get_importing_parameter( 'LANGUAGE' ).
    ASSIGN lr_language->* TO FIELD-SYMBOL(<lv_language>).
    cl_abap_unit_assert=>assert_equals( act = <lv_language> exp = 'E' ).
    DATA(lr_direct) = arguments->get_importing_parameter( 'SAVEMODE_DIRECT' ).
    ASSIGN lr_direct->* TO FIELD-SYMBOL(<lv_direct>).
    cl_abap_unit_assert=>assert_equals( act = <lv_direct> exp = abap_true ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_so10_copy_client DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES if_ftd_invocation_answer.
    METHODS constructor IMPORTING iv_parameter TYPE abap_parmname iv_client TYPE sy-mandt.
  PRIVATE SECTION.
    DATA mv_parameter TYPE abap_parmname.
    DATA mv_client TYPE sy-mandt.
ENDCLASS.
CLASS lcl_so10_copy_client IMPLEMENTATION.
  METHOD constructor.
    mv_parameter = iv_parameter.
    mv_client = iv_client.
  ENDMETHOD.
  METHOD if_ftd_invocation_answer~answer.
    DATA(lr_client) = arguments->get_importing_parameter( mv_parameter ).
    ASSIGN lr_client->* TO FIELD-SYMBOL(<lv_client>).
    cl_abap_unit_assert=>assert_equals( act = <lv_client> exp = mv_client ).
    IF mv_parameter = 'MANDT'.
      DATA(lr_scope) = arguments->get_importing_parameter( '_SCOPE' ).
      ASSIGN lr_scope->* TO FIELD-SYMBOL(<lv_scope>).
      cl_abap_unit_assert=>assert_equals( act = <lv_scope> exp = '1' ).
      DATA(lr_name) = arguments->get_importing_parameter( 'TDNAME' ).
      ASSIGN lr_name->* TO FIELD-SYMBOL(<lv_name>).
      cl_abap_unit_assert=>assert_equals( act = <lv_name> exp = 'ZSO10_TEST' ).
      DATA(lr_object) = arguments->get_importing_parameter( 'TDOBJECT' ).
      ASSIGN lr_object->* TO FIELD-SYMBOL(<lv_object>).
      cl_abap_unit_assert=>assert_equals( act = <lv_object> exp = 'TEXT' ).
      DATA(lr_id) = arguments->get_importing_parameter( 'TDID' ).
      ASSIGN lr_id->* TO FIELD-SYMBOL(<lv_id>).
      cl_abap_unit_assert=>assert_equals( act = <lv_id> exp = 'ST' ).
      DATA(lr_language) = arguments->get_importing_parameter( 'TDSPRAS' ).
      ASSIGN lr_language->* TO FIELD-SYMBOL(<lv_language>).
      cl_abap_unit_assert=>assert_equals( act = <lv_language> exp = 'E' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_so10_adopt_failure DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_so10_sync_store.
    DATA fail_fingerprint TYPE abap_bool.
    DATA fail_record TYPE abap_bool VALUE abap_true.
    DATA baseline TYPE zcl_abgagt_so10_sync_rules=>ty_baseline.
    DATA load_calls TYPE i.
    DATA fingerprint_calls TYPE i.
    DATA record_calls TYPE i.
    DATA fail_remove TYPE abap_bool.
    DATA keep_removed TYPE abap_bool.
    DATA remove_calls TYPE i.
  PRIVATE SECTION.
    DATA mv_recorded TYPE abap_bool.
ENDCLASS.
CLASS lcl_so10_adopt_failure IMPLEMENTATION.
  METHOD zif_abgagt_so10_sync_store~load.
    load_calls += 1.
    rs_baseline = COND #( WHEN mv_recorded = abap_false THEN baseline ELSE VALUE #( ) ).
  ENDMETHOD.
  METHOD zif_abgagt_so10_sync_store~fingerprint.
    fingerprint_calls += 1.
    IF fail_fingerprint = abap_true.
      zcx_abapgit_exception=>raise( 'Injected fingerprint failure' ).
    ENDIF.
    rv_sha = zcl_abgagt_so10_sync_rules=>fingerprint( it_lines ).
  ENDMETHOD.
  METHOD zif_abgagt_so10_sync_store~remove.
    remove_calls += 1.
    IF fail_remove = abap_true.
      zcx_abapgit_exception=>raise( 'Injected baseline removal failure' ).
    ENDIF.
    mv_recorded = xsdbool( keep_removed = abap_false ).
  ENDMETHOD.
  METHOD zif_abgagt_so10_sync_store~record.
    record_calls += 1.
    IF fail_record = abap_true.
      zcx_abapgit_exception=>raise( 'Injected metadata failure' ).
    ENDIF.
    mv_recorded = abap_true.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_so10_native_validation DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS empty_save_rejected FOR TESTING.
    METHODS invalid_itf_rejected FOR TESTING.
    METHODS overlong_key_rejected FOR TESTING.
ENDCLASS.
CLASS ltcl_so10_native_safeguards DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA mo_functions TYPE REF TO if_function_test_environment.
    CLASS-DATA mo_sql TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup RAISING cx_static_check.
    CLASS-METHODS class_teardown.
    METHODS setup RAISING cx_static_check.
    METHODS expect_error IMPORTING iv_message TYPE string iv_action TYPE string DEFAULT 'CREATE'
      iv_save_calls TYPE i DEFAULT 0 iv_flush_calls TYPE i DEFAULT 0
      iv_overwrite TYPE abap_bool DEFAULT abap_false RAISING cx_static_check.
    METHODS closed_client_rejected FOR TESTING RAISING cx_static_check.
    METHODS unknown_policy_rejected FOR TESTING RAISING cx_static_check.
    METHODS existing_create_rejected FOR TESTING RAISING cx_static_check.
    METHODS authorization_denied FOR TESTING RAISING cx_static_check.
    METHODS foreign_lock_rejected FOR TESTING RAISING cx_static_check.
    METHODS missing_update_rejected FOR TESTING RAISING cx_static_check.
    METHODS save_failure_unlocks FOR TESTING RAISING cx_static_check.
    METHODS flush_failure_unlocks FOR TESTING RAISING cx_static_check.
    METHODS existing_copy_rejected FOR TESTING RAISING cx_static_check.
    METHODS copy_overwrite_saved FOR TESTING RAISING cx_static_check.
    METHODS copy_create_auth_denied FOR TESTING RAISING cx_static_check.
    METHODS copy_edit_auth_denied FOR TESTING RAISING cx_static_check.
    METHODS expect_record_error IMPORTING iv_message TYPE string iv_task TYPE string DEFAULT 'DEVK900002'
      iv_append_calls TYPE i DEFAULT 0 RAISING cx_static_check.
    METHODS records_task_key FOR TESTING RAISING cx_static_check.
    METHODS resolves_parent_request FOR TESTING RAISING cx_static_check.
    METHODS wrong_category_rejected FOR TESTING RAISING cx_static_check.
    METHODS released_task_rejected FOR TESTING RAISING cx_static_check.
    METHODS wrong_client_rejected FOR TESTING RAISING cx_static_check.
    METHODS foreign_owner_rejected FOR TESTING RAISING cx_static_check.
    METHODS ambiguous_tasks_rejected FOR TESTING RAISING cx_static_check.
    METHODS missing_text_no_record FOR TESTING RAISING cx_static_check.
    METHODS cts_failure_unlocks FOR TESTING RAISING cx_static_check.
    METHODS no_transport_policy FOR TESTING RAISING cx_static_check.
    METHODS foreign_record_lock FOR TESTING RAISING cx_static_check.
    METHODS missing_entry_detected FOR TESTING RAISING cx_static_check.
    METHODS read_observed_baseline FOR TESTING RAISING cx_static_check.
    METHODS missing_read_keeps_key FOR TESTING RAISING cx_static_check.
    METHODS read_other_client_denied FOR TESTING RAISING cx_static_check.
    METHODS read_other_client_scope FOR TESTING RAISING cx_static_check.
    METHODS malformed_read_client FOR TESTING RAISING cx_static_check.
    METHODS prepare_adopt.
    METHODS adopt IMPORTING iv_repo TYPE string DEFAULT 'project-a'
      iv_sha TYPE string DEFAULT 'b65181c01b75573cde621898e49a135c346e0f83a843c3768ee8dbebed9ee312'
      io_store TYPE REF TO zif_abgagt_so10_sync_store OPTIONAL
      RETURNING VALUE(rs_result) TYPE zif_abgagt_so10_api=>ty_write.
    METHODS adopt_records_only_baseline FOR TESTING.
    METHODS adopt_changed_source FOR TESTING.
    METHODS adopt_foreign_owner FOR TESTING.
    METHODS adopt_missing_source FOR TESTING.
    METHODS adopt_foreign_lock FOR TESTING.
    METHODS adopt_invalid_hash FOR TESTING.
    METHODS adopt_fingerprint_failure FOR TESTING.
    METHODS adopt_metadata_failure FOR TESTING.
    METHODS adopt_unverified_commit FOR TESTING.
    METHODS unsafe_key_rejected FOR TESTING RAISING cx_static_check.
    METHODS write IMPORTING iv_action TYPE string DEFAULT 'UPDATE' iv_repo TYPE string DEFAULT 'project-a'
      iv_managed TYPE abap_bool DEFAULT abap_true iv_ignore TYPE abap_bool DEFAULT abap_false
      it_lines TYPE zif_abgagt_so10_api=>ty_native_lines OPTIONAL
      io_store TYPE REF TO zif_abgagt_so10_sync_store OPTIONAL
      RETURNING VALUE(rs_result) TYPE zif_abgagt_so10_api=>ty_write.
    METHODS insert_baseline IMPORTING iv_repo TYPE string DEFAULT 'project-a'
      iv_sha TYPE string DEFAULT 'b65181c01b75573cde621898e49a135c346e0f83a843c3768ee8dbebed9ee312'.
    METHODS write_unknown_base FOR TESTING.
    METHODS write_noop_baseline FOR TESTING.
    METHODS write_gui_edit FOR TESTING RAISING zcx_abapgit_exception.
    METHODS write_both_changed FOR TESTING RAISING zcx_abapgit_exception.
    METHODS write_owner_ignore FOR TESTING.
    METHODS write_create_exists FOR TESTING.
    METHODS write_update_missing FOR TESTING.
    METHODS write_standalone_required FOR TESTING.
    METHODS write_standalone_noop FOR TESTING.
    METHODS prepare_write IMPORTING iv_after TYPE string DEFAULT 'next' iv_missing TYPE abap_bool DEFAULT abap_false.
    METHODS write_create_saved FOR TESTING.
    METHODS write_update_saved FOR TESTING RAISING zcx_abapgit_exception.
    METHODS write_record_failed_saved FOR TESTING RAISING zcx_abapgit_exception.
    METHODS write_verify_failed_saved FOR TESTING RAISING zcx_abapgit_exception.
    METHODS write_readback_unverified FOR TESTING RAISING zcx_abapgit_exception.
    METHODS write_standalone_saved FOR TESTING RAISING zcx_abapgit_exception.
    METHODS copy_fixture IMPORTING iv_reverse TYPE abap_bool DEFAULT abap_false
      iv_existing TYPE abap_bool DEFAULT abap_false iv_overwrite TYPE abap_bool DEFAULT abap_false
      iv_policy TYPE t000-cccoractiv DEFAULT space iv_source_missing TYPE abap_bool DEFAULT abap_false
      iv_admin_denied TYPE abap_bool DEFAULT abap_false iv_wrong_readback TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rs_result) TYPE zif_abgagt_so10_api=>ty_write.
    METHODS copy_forward_scope FOR TESTING.
    METHODS copy_reverse_scope FOR TESTING.
    METHODS copy_no_overwrite FOR TESTING.
    METHODS copy_target_policy FOR TESTING.
    METHODS copy_source_missing FOR TESTING.
    METHODS copy_admin_denied FOR TESTING.
    METHODS copy_equal_clients FOR TESTING.
    METHODS copy_readback_unknown FOR TESTING.
    METHODS drop_fixture IMPORTING iv_absent TYPE abap_bool DEFAULT abap_false
      iv_remaining TYPE abap_bool DEFAULT abap_false iv_other TYPE abap_bool DEFAULT abap_false
      io_store TYPE REF TO zif_abgagt_so10_sync_store OPTIONAL
      RETURNING VALUE(rs_result) TYPE zif_abgagt_so10_api=>ty_write.
    METHODS drop_owned_text FOR TESTING RAISING zcx_abapgit_exception.
    METHODS drop_unknown_base FOR TESTING.
    METHODS drop_foreign_owner FOR TESTING RAISING zcx_abapgit_exception.
    METHODS drop_changed_base FOR TESTING RAISING zcx_abapgit_exception.
    METHODS drop_changed_source FOR TESTING RAISING zcx_abapgit_exception.
    METHODS drop_missing_cleanup FOR TESTING RAISING zcx_abapgit_exception.
    METHODS drop_readback_unknown FOR TESTING RAISING zcx_abapgit_exception.
    METHODS drop_remove_failure FOR TESTING.
    METHODS drop_metadata_unknown FOR TESTING.
    METHODS drop_foreign_scope FOR TESTING RAISING zcx_abapgit_exception.
    METHODS drop_denied_locked FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.
CLASS ltcl_so10_native_safeguards IMPLEMENTATION.
  METHOD drop_fixture.
    DATA(lv_client) = CONV sy-mandt( COND string( WHEN iv_other = abap_false THEN CONV #( sy-mandt )
      WHEN sy-mandt = '123' THEN '124' ELSE '123' ) ).
    IF iv_other = abap_true.
      DATA lt_clients TYPE STANDARD TABLE OF t000 WITH EMPTY KEY.
      lt_clients = VALUE #( ( mandt = lv_client ) ).
      mo_sql->insert_test_data( lt_clients ).
      UPDATE zabgagt_so10meta SET data_client = @lv_client WHERE data_client = @sy-mandt.
      mo_functions->get_double( 'AUTHORITY_CHECK' )->configure_call( )->ignore_all_parameters( )->then_raise_classic_exception( 'USER_IS_AUTHORIZED' ).
    ENDIF.
    DATA(ls_header) = VALUE thead( tdobject = 'TEXT' tdname = 'ZSO10_TEST' tdid = 'ST' tdspras = 'E' ).
    DATA(lt_lines) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'hello' ) ).
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    DATA(lo_output) = lo_read->create_output_configuration( )->set_exporting_parameter( name = 'HEADER' value = ls_header
      )->set_table_parameter( name = 'LINES' value = lt_lines ).
    DATA(lo_sequence) = lo_read->configure_call( )->when( lo_read->create_input_configuration( )->set_importing_parameter( name = 'CLIENT' value = lv_client ) ).
    IF iv_absent = abap_true.
      lo_sequence->then_raise_classic_exception( 'NOT_FOUND' ).
    ELSE.
      lo_sequence->then_set_output( lo_output ).
      IF iv_remaining = abap_true.
        lo_sequence->then_set_output( lo_output ).
      ELSE.
        lo_sequence->then_raise_classic_exception( 'NOT_FOUND' ).
      ENDIF.
    ENDIF.
    mo_functions->get_double( 'DELETE_TEXT' )->configure_call( )->ignore_all_parameters( )->then_answer(
      NEW lcl_so10_drop_arguments( iv_client = lv_client ) ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->configure_call( )->ignore_all_parameters( )->then_answer(
      NEW lcl_so10_copy_client( iv_parameter = 'MANDT' iv_client = lv_client ) ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->configure_call( )->ignore_all_parameters( )->then_answer(
      NEW lcl_so10_copy_client( iv_parameter = 'MANDT' iv_client = lv_client ) ).
    rs_result = NEW zcl_abgagt_so10_api( io_sync_store = io_store )->zif_abgagt_so10_sync_api~drop(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
      it_lines = lt_lines iv_repo_id = 'project-a' iv_data_client = CONV #( lv_client ) ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_called_times( 0 ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_called_times( 0 ).
    cl_abap_unit_assert=>assert_false( rs_result-saved->* ).
  ENDMETHOD.
  METHOD drop_owned_text.
    insert_baseline( ).
    DATA(ls_result) = drop_fixture( ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-deleted->* ).
    cl_abap_unit_assert=>assert_true( ls_result-baseline_removed->* ).
    mo_functions->get_double( 'DELETE_TEXT' )->verify( )->is_called_times( 1 ).
    SELECT SINGLE repo_id FROM zabgagt_so10meta WHERE data_client = @sy-mandt INTO @DATA(lv_owner).
    cl_abap_unit_assert=>assert_equals( act = sy-subrc exp = 4 ).
  ENDMETHOD.
  METHOD drop_unknown_base.
    DATA(ls_result) = drop_fixture( ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-conflict_kind exp = 'BASELINE_MISSING' ).
    mo_functions->get_double( 'DELETE_TEXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.
  METHOD drop_foreign_owner.
    insert_baseline( iv_repo = 'other' ).
    DATA(ls_result) = drop_fixture( ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-conflict_kind exp = 'OWNERSHIP' ).
    mo_functions->get_double( 'DELETE_TEXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.
  METHOD drop_changed_base.
    insert_baseline( iv_sha = repeat( val = 'a' occ = 64 ) ).
    DATA(ls_result) = drop_fixture( ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-conflict_kind exp = 'SOURCE_CHANGED' ).
    mo_functions->get_double( 'DELETE_TEXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.
  METHOD drop_changed_source.
    prepare_adopt( ).
    insert_baseline( iv_sha = zcl_abgagt_so10_sync_rules=>fingerprint( VALUE #( ( tdformat = '*' tdline = 'different' ) ) ) ).
    DATA(ls_result) = NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_sync_api~drop(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
      it_lines = VALUE #( ( tdformat = '*' tdline = 'different' ) ) iv_repo_id = 'project-a' ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    mo_functions->get_double( 'DELETE_TEXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.
  METHOD drop_missing_cleanup.
    insert_baseline( ).
    DATA(ls_result) = drop_fixture( iv_absent = abap_true ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    mo_functions->get_double( 'DELETE_TEXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.
  METHOD drop_readback_unknown.
    insert_baseline( ).
    DATA(ls_result) = drop_fixture( iv_remaining = abap_true ).
    cl_abap_unit_assert=>assert_not_bound( ls_result-deleted ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_removed->* ).
  ENDMETHOD.
  METHOD drop_remove_failure.
    DATA(lo_store) = NEW lcl_so10_adopt_failure( ).
    lo_store->baseline = VALUE #( found = abap_true repo_id = 'project-a'
      source_sha = 'b65181c01b75573cde621898e49a135c346e0f83a843c3768ee8dbebed9ee312' ).
    lo_store->fail_remove = abap_true.
    DATA(ls_result) = drop_fixture( io_store = lo_store ).
    cl_abap_unit_assert=>assert_true( ls_result-deleted->* ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_removed->* ).
  ENDMETHOD.
  METHOD drop_metadata_unknown.
    DATA(lo_store) = NEW lcl_so10_adopt_failure( ).
    lo_store->baseline = VALUE #( found = abap_true repo_id = 'project-a'
      source_sha = 'b65181c01b75573cde621898e49a135c346e0f83a843c3768ee8dbebed9ee312' ).
    lo_store->keep_removed = abap_true.
    DATA(ls_result) = drop_fixture( io_store = lo_store ).
    cl_abap_unit_assert=>assert_true( ls_result-deleted->* ).
    cl_abap_unit_assert=>assert_not_bound( ls_result-baseline_removed ).
  ENDMETHOD.
  METHOD drop_foreign_scope.
    insert_baseline( ).
    DATA(ls_result) = drop_fixture( iv_other = abap_true ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    mo_functions->get_double( 'DELETE_TEXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD drop_denied_locked.
    insert_baseline( ).
    mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' )->configure_call( )->ignore_all_parameters( )->then_raise_classic_exception( 'NO_AUTHORITY' ).
    DATA(ls_result) = drop_fixture( ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    mo_functions->get_double( 'DELETE_TEXT' )->verify( )->is_called_times( 0 ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.
  METHOD copy_fixture.
    DATA(lv_other) = CONV sy-mandt( COND string( WHEN sy-mandt = '123' THEN '124' ELSE '123' ) ).
    DATA(lv_source) = COND sy-mandt( WHEN iv_reverse = abap_true THEN lv_other ELSE sy-mandt ).
    DATA(lv_target) = COND sy-mandt( WHEN iv_reverse = abap_true THEN sy-mandt ELSE lv_other ).
    DATA lt_clients TYPE STANDARD TABLE OF t000 WITH EMPTY KEY.
    lt_clients = VALUE #( ( mandt = lv_other cccoractiv = iv_policy ) ).
    mo_sql->insert_test_data( lt_clients ).
    DATA(lo_auth) = mo_functions->get_double( 'AUTHORITY_CHECK' ).
    lo_auth->configure_call( )->when( lo_auth->create_input_configuration( ) )->then_raise_classic_exception(
      COND #( WHEN iv_admin_denied = abap_true THEN 'USER_NOT_AUTHORIZED' ELSE 'USER_IS_AUTHORIZED' ) ).
    DATA(ls_header) = VALUE thead( mandt = lv_source tdobject = 'TEXT' tdname = 'ZSO10_TEST' tdid = 'ST' tdspras = 'E' ).
    DATA(lt_lines) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'copied content' ) ).
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    DATA(lo_source) = lo_read->create_input_configuration( )->set_importing_parameter( name = 'CLIENT' value = lv_source ).
    DATA(lo_source_config) = lo_read->configure_call( )->when( lo_source ).
    IF iv_source_missing = abap_true.
      lo_source_config->then_raise_classic_exception( 'NOT_FOUND' ).
    ELSE.
      lo_source_config->then_set_output( lo_read->create_output_configuration( )->set_exporting_parameter(
        name = 'HEADER' value = ls_header )->set_table_parameter( name = 'LINES' value = lt_lines ) ).
    ENDIF.
    ls_header-mandt = lv_target.
    DATA(lo_target) = lo_read->create_input_configuration( )->set_importing_parameter( name = 'CLIENT' value = lv_target ).
    DATA(lo_target_config) = lo_read->configure_call( )->when( lo_target ).
    DATA(lo_target_output) = lo_read->create_output_configuration( )->set_exporting_parameter(
      name = 'HEADER' value = ls_header )->set_table_parameter( name = 'LINES' value = lt_lines ).
    IF iv_existing = abap_true.
      lo_target_config->then_set_output( lo_target_output ).
    ELSE.
      lo_target_config->then_raise_classic_exception( 'NOT_FOUND' ).
    ENDIF.
    DATA(lt_after) = COND zif_abgagt_so10_api=>ty_native_lines( WHEN iv_wrong_readback = abap_true
      THEN VALUE #( ( tdformat = '*' tdline = 'wrong content' ) ) ELSE lt_lines ).
    lo_target_config->then_set_output( lo_read->create_output_configuration( )->set_exporting_parameter(
      name = 'HEADER' value = ls_header )->set_table_parameter( name = 'LINES' value = lt_after ) ).
    mo_functions->get_double( 'SAVE_TEXT' )->configure_call( )->ignore_all_parameters( )->then_answer(
      NEW lcl_so10_copy_client( iv_parameter = 'CLIENT' iv_client = lv_target ) ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->configure_call( )->ignore_all_parameters( )->then_answer(
      NEW lcl_so10_copy_client( iv_parameter = 'MANDT' iv_client = lv_target ) ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->configure_call( )->ignore_all_parameters( )->then_answer(
      NEW lcl_so10_copy_client( iv_parameter = 'MANDT' iv_client = lv_target ) ).
    DATA(lo_store) = NEW lcl_so10_adopt_failure( ).
    rs_result = NEW zcl_abgagt_so10_api( io_sync_store = lo_store )->zif_abgagt_so10_api~copy(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
      iv_source_client = CONV #( lv_source ) iv_target_client = CONV #( lv_target ) iv_overwrite = iv_overwrite ).
    cl_abap_unit_assert=>assert_equals( act = lo_store->load_calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lo_store->fingerprint_calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = lo_store->record_calls exp = 0 ).
    cl_abap_unit_assert=>assert_false( rs_result-baseline_recorded->* ).
  ENDMETHOD.
  METHOD copy_forward_scope.
    DATA(ls_result) = copy_fixture( ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-source_client exp = CONV string( sy-mandt ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-text-data_client
      exp = COND string( WHEN sy-mandt = '123' THEN '124' ELSE '123' ) ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_called_times( 3 ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_called_once( ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD copy_reverse_scope.
    DATA(ls_result) = copy_fixture( iv_reverse = abap_true iv_existing = abap_true iv_overwrite = abap_true ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-text-data_client exp = CONV string( sy-mandt ) ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_called_once( ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_called_times( 3 ).
  ENDMETHOD.
  METHOD copy_no_overwrite.
    DATA(ls_result) = copy_fixture( iv_existing = abap_true ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_true( xsdbool( ls_result-error CS 'use --overwrite' ) ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD copy_target_policy.
    DATA(ls_result) = copy_fixture( iv_policy = '2' ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_true( xsdbool( ls_result-error CS 'does not permit changes' ) ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD copy_source_missing.
    DATA(ls_result) = copy_fixture( iv_source_missing = abap_true ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_true( xsdbool( ls_result-error CS 'Source SO10 text not found' ) ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD copy_admin_denied.
    DATA(ls_result) = copy_fixture( iv_admin_denied = abap_true ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    mo_functions->get_double( 'INIT_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD copy_equal_clients.
    DATA(ls_result) = NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~copy(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
      iv_target_client = CONV #( sy-mandt ) ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    mo_functions->get_double( 'INIT_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD copy_readback_unknown.
    DATA(ls_result) = copy_fixture( iv_wrong_readback = abap_true ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_initial( ls_result-saved ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD prepare_write.
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    DATA(ls_header) = VALUE thead( mandt = sy-mandt tdobject = 'TEXT' tdname = 'ZSO10_TEST'
      tdid = 'ST' tdspras = 'E' tdluser = sy-uname tdldate = sy-datum tdltime = sy-uzeit ).
    DATA(lt_before) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'prior' ) ).
    DATA(lt_after) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = iv_after ) ).
    DATA(lo_before) = lo_read->create_output_configuration( )->set_exporting_parameter( name = 'HEADER' value = ls_header
      )->set_table_parameter( name = 'LINES' value = lt_before ).
    DATA(lo_after) = lo_read->create_output_configuration( )->set_exporting_parameter( name = 'HEADER' value = ls_header
      )->set_table_parameter( name = 'LINES' value = lt_after ).
    DATA(lo_config) = lo_read->configure_call( )->when( lo_read->create_input_configuration( ) ).
    IF iv_missing = abap_true.
      lo_config->then_raise_classic_exception( 'NOT_FOUND' )->then_set_output( lo_after ).
    ELSE.
      lo_config->then_set_output( lo_before )->then_set_output( lo_after ).
    ENDIF.
  ENDMETHOD.
  METHOD write_create_saved.
    prepare_write( iv_missing = abap_true ).
    DATA(ls_result) = write( iv_action = 'CREATE' it_lines = VALUE #( ( tdformat = '*' tdline = 'next' ) ) ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_true( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-text-base_sha exp = ls_result-text-source_sha ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_called_times( 2 ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_called_times( 1 ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_called_times( 1 ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD write_update_saved.
    prepare_write( ).
    DATA(lt_prior) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'prior' ) ).
    insert_baseline( iv_sha = zcl_abgagt_so10_sync_rules=>fingerprint( lt_prior ) ).
    DATA(ls_result) = write( it_lines = VALUE #( ( tdformat = '*' tdline = 'next' ) ) ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_true( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-text-base_sha exp = ls_result-text-source_sha ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_called_times( 1 ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD write_record_failed_saved.
    prepare_write( ).
    DATA(lt_prior) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'prior' ) ).
    DATA(lo_store) = NEW lcl_so10_adopt_failure( ).
    lo_store->baseline = VALUE #( found = abap_true repo_id = 'project-a'
      source_sha = zcl_abgagt_so10_sync_rules=>fingerprint( lt_prior ) ).
    DATA(ls_result) = write( it_lines = VALUE #( ( tdformat = '*' tdline = 'next' ) ) io_store = lo_store ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'METADATA_RECORD' ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_called_times( 1 ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD write_verify_failed_saved.
    prepare_write( ).
    DATA(lt_prior) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'prior' ) ).
    DATA(lo_store) = NEW lcl_so10_adopt_failure( ).
    lo_store->baseline = VALUE #( found = abap_true repo_id = 'project-a'
      source_sha = zcl_abgagt_so10_sync_rules=>fingerprint( lt_prior ) ).
    lo_store->fail_record = abap_false.
    DATA(ls_result) = write( it_lines = VALUE #( ( tdformat = '*' tdline = 'next' ) ) io_store = lo_store ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_not_bound( ls_result-baseline_recorded ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'METADATA_VERIFY' ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_called_times( 1 ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD write_readback_unverified.
    prepare_write( iv_after = 'unexpected' ).
    DATA(lt_prior) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'prior' ) ).
    DATA(lv_prior_sha) = zcl_abgagt_so10_sync_rules=>fingerprint( lt_prior ).
    insert_baseline( iv_sha = lv_prior_sha ).
    DATA(ls_result) = write( it_lines = VALUE #( ( tdformat = '*' tdline = 'next' ) ) ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_not_bound( ls_result-saved ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'TEXT_VERIFY' ).
    SELECT SINGLE source_sha FROM zabgagt_so10meta WHERE data_client = @sy-mandt
      AND text_object = 'TEXT' AND text_name = 'ZSO10_TEST' AND text_id = 'ST' AND text_language = 'E'
      INTO @DATA(lv_base_sha).
    cl_abap_unit_assert=>assert_equals( act = lv_base_sha exp = lv_prior_sha ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD write_standalone_saved.
    prepare_write( ).
    DATA(lt_prior) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'prior' ) ).
    DATA(lv_prior_sha) = zcl_abgagt_so10_sync_rules=>fingerprint( lt_prior ).
    insert_baseline( iv_repo = 'other-project' iv_sha = lv_prior_sha ).
    DATA(ls_result) = write( iv_repo = `` iv_managed = abap_false iv_ignore = abap_true
      it_lines = VALUE #( ( tdformat = '*' tdline = 'next' ) ) ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    SELECT SINGLE repo_id, source_sha FROM zabgagt_so10meta WHERE data_client = @sy-mandt
      AND text_object = 'TEXT' AND text_name = 'ZSO10_TEST' AND text_id = 'ST' AND text_language = 'E'
      INTO @DATA(ls_base).
    cl_abap_unit_assert=>assert_equals( act = ls_base-repo_id exp = 'other-project' ).
    cl_abap_unit_assert=>assert_equals( act = ls_base-source_sha exp = lv_prior_sha ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD write.
    DATA(lt_lines) = COND zif_abgagt_so10_api=>ty_native_lines( WHEN it_lines IS INITIAL
      THEN VALUE #( ( tdformat = '*' tdline = 'hello' ) ) ELSE it_lines ).
    rs_result = NEW zcl_abgagt_so10_api( io_sync_store = io_store )->zif_abgagt_so10_sync_api~write(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
      it_lines = lt_lines iv_action = iv_action iv_repo_id = iv_repo iv_managed = iv_managed iv_ignore = iv_ignore ).
  ENDMETHOD.
  METHOD insert_baseline.
    DATA lt_base TYPE STANDARD TABLE OF zabgagt_so10meta WITH EMPTY KEY.
    lt_base = VALUE #( ( data_client = sy-mandt text_object = 'TEXT' text_name = 'ZSO10_TEST'
      text_id = 'ST' text_language = 'E' repo_id = iv_repo source_sha = iv_sha source_user = sy-uname ) ).
    mo_sql->insert_test_data( lt_base ).
  ENDMETHOD.
  METHOD write_unknown_base.
    prepare_adopt( ).
    DATA(ls_result) = write( ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-conflict_kind exp = 'BASELINE_MISSING' ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD write_noop_baseline.
    prepare_adopt( ).
    insert_baseline( ).
    DATA(ls_result) = write( ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_true( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-text-base_sha exp = ls_result-text-source_sha ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD write_gui_edit.
    prepare_adopt( ).
    DATA(lt_prior) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'prior' ) ).
    insert_baseline( iv_sha = zcl_abgagt_so10_sync_rules=>fingerprint( lt_prior ) ).
    DATA(ls_result) = write( it_lines = lt_prior ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-conflict_kind exp = 'LOCAL_EDIT' ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD write_both_changed.
    prepare_adopt( ).
    DATA(lt_prior) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'prior' ) ).
    insert_baseline( iv_sha = zcl_abgagt_so10_sync_rules=>fingerprint( lt_prior ) ).
    DATA(ls_result) = write( it_lines = VALUE #( ( tdformat = '*' tdline = 'next' ) ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-conflict_kind exp = 'SYSTEM_EDIT' ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD write_owner_ignore.
    prepare_adopt( ).
    insert_baseline( iv_repo = 'other-project' ).
    DATA(ls_result) = write( iv_ignore = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-conflict_kind exp = 'OWNERSHIP' ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD write_create_exists.
    prepare_adopt( ).
    DATA(ls_result) = write( iv_action = 'CREATE' ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_result-error exp = '*already exists*' ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD write_update_missing.
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    lo_read->configure_call( )->when( lo_read->create_input_configuration( ) )->then_raise_classic_exception( 'NOT_FOUND' ).
    DATA(ls_result) = write( iv_ignore = abap_true ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_result-error exp = '*missing*' ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD write_standalone_required.
    prepare_adopt( ).
    DATA(ls_result) = write( iv_managed = abap_false ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_char_cp( act = ls_result-error exp = '*explicit conflict-ignore*' ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD write_standalone_noop.
    prepare_adopt( ).
    insert_baseline( iv_repo = 'other-project' iv_sha = repeat( val = 'a' occ = 64 ) ).
    DATA(ls_result) = write( iv_repo = `` iv_managed = abap_false iv_ignore = abap_true ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    SELECT SINGLE repo_id, source_sha FROM zabgagt_so10meta WHERE data_client = @sy-mandt
      AND text_object = 'TEXT' AND text_name = 'ZSO10_TEST' AND text_id = 'ST' AND text_language = 'E'
      INTO @DATA(ls_base).
    cl_abap_unit_assert=>assert_equals( act = ls_base-repo_id exp = 'other-project' ).
    cl_abap_unit_assert=>assert_equals( act = ls_base-source_sha exp = repeat( val = 'a' occ = 64 ) ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD class_setup.
    mo_sql = cl_osql_test_environment=>create( VALUE #(
      ( 'T000' ) ( 'E070' ) ( 'E070C' ) ( 'E071' ) ( 'ZABGAGT_SO10META' ) ) ).
    mo_functions = cl_function_test_environment=>create( VALUE #(
      ( 'INIT_TEXT' ) ( 'TR_SYS_PARAMS' ) ( 'CHECK_TEXT_AUTHORITY' ) ( 'AUTHORITY_CHECK' )
      ( 'ENQUEUE_ESSSTXT' ) ( 'DEQUEUE_ESSSTXT' ) ( 'READ_TEXT' )
      ( 'SAVE_TEXT' ) ( 'DELETE_TEXT' ) ( 'COMMIT_TEXT' ) ( 'TR_APPEND_TO_COMM_OBJS_KEYS' ) ) ).
  ENDMETHOD.
  METHOD class_teardown.
    IF mo_functions IS BOUND.
      mo_functions->disable_doubles( ).
    ENDIF.
    IF mo_sql IS BOUND.
      mo_sql->destroy( ).
    ENDIF.
  ENDMETHOD.
  METHOD setup.
    mo_functions->clear_doubles( ).
    mo_sql->clear_doubles( ).
    DATA lt_clients TYPE STANDARD TABLE OF t000 WITH EMPTY KEY.
    lt_clients = VALUE #( ( mandt = sy-mandt ) ).
    mo_sql->insert_test_data( lt_clients ).
    DATA lt_requests TYPE STANDARD TABLE OF e070 WITH EMPTY KEY.
    lt_requests = VALUE #( ( trkorr = 'DEVK900001' trfunction = 'W' trstatus = 'D' as4user = sy-uname )
      ( trkorr = 'DEVK900002' trfunction = 'Q' trstatus = 'D' strkorr = 'DEVK900001' as4user = sy-uname ) ).
    mo_sql->insert_test_data( lt_requests ).
    DATA lt_request_clients TYPE STANDARD TABLE OF e070c WITH EMPTY KEY.
    lt_request_clients = VALUE #( ( trkorr = 'DEVK900001' client = sy-mandt ) ).
    mo_sql->insert_test_data( lt_request_clients ).
    DATA lt_entries TYPE STANDARD TABLE OF e071 WITH EMPTY KEY.
    lt_entries = VALUE #( ( trkorr = 'DEVK900002' as4pos = '000001' pgmid = 'R3TR'
      object = 'TEXT' obj_name = 'TEXT,ZSO10_TEST,ST,E' ) ).
    mo_sql->insert_test_data( lt_entries ).
    DATA(lo_init) = mo_functions->get_double( 'INIT_TEXT' ).
    DATA(ls_header) = VALUE thead( tdobject = 'TEXT' tdname = 'ZSO10_TEST' tdid = 'ST' tdspras = 'E' ).
    DATA(lo_output) = lo_init->create_output_configuration( )->set_exporting_parameter(
      name = 'HEADER' value = ls_header ).
    lo_init->configure_call( )->when( lo_init->create_input_configuration( ) )->then_set_output( lo_output ).
  ENDMETHOD.
  METHOD unsafe_key_rejected.
    DATA(lo_api) = NEW zcl_abgagt_so10_api( ).
    DATA(lt_names) = VALUE string_table( ( |ZBAD | ) ( |ZBAD\t| ) ( |ZBAD\n| ) ( |Z*BAD| ) ).
    LOOP AT lt_names INTO DATA(lv_name).
      TRY.
          lo_api->zif_abgagt_so10_api~read( VALUE #( object = 'TEXT' name = lv_name id = 'ST' language = 'E' ) ).
          cl_abap_unit_assert=>fail( 'Unsafe name accepted' ).
        CATCH zcx_abapgit_exception INTO DATA(lx_name).
          cl_abap_unit_assert=>assert_true( xsdbool( lx_name->get_text( ) CS 'one exact text key' ) ).
      ENDTRY.
    ENDLOOP.
    DATA(lt_ids) = VALUE string_table( ( |S | ) ( |S?| ) ).
    LOOP AT lt_ids INTO DATA(lv_id).
      TRY.
          lo_api->zif_abgagt_so10_api~read( VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = lv_id language = 'E' ) ).
          cl_abap_unit_assert=>fail( 'Unsafe Text ID accepted' ).
        CATCH zcx_abapgit_exception INTO DATA(lx_id).
          cl_abap_unit_assert=>assert_true( xsdbool( lx_id->get_text( ) CS 'Invalid Text ID' ) ).
      ENDTRY.
    ENDLOOP.
    mo_functions->get_double( 'INIT_TEXT' )->verify( )->is_called_times( 0 ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.

  METHOD prepare_adopt.
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    DATA(lt_lines) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'hello' ) ).
    DATA(ls_header) = VALUE thead( tdobject = 'TEXT' tdname = 'ZSO10_TEST' tdid = 'ST' tdspras = 'E' ).
    DATA(lo_output) = lo_read->create_output_configuration( )->set_table_parameter( name = 'LINES' value = lt_lines
      )->set_exporting_parameter( name = 'HEADER' value = ls_header ).
    lo_read->configure_call( )->when( lo_read->create_input_configuration( ) )->then_set_output( lo_output ).
  ENDMETHOD.
  METHOD adopt.
    DATA(lo_api) = NEW zcl_abgagt_so10_api( io_sync_store = io_store ).
    rs_result = lo_api->zif_abgagt_so10_sync_api~adopt(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
      iv_repo_id = iv_repo iv_source_sha = iv_sha ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_called_times( 0 ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.
  METHOD adopt_records_only_baseline.
    prepare_adopt( ).
    DATA(ls_result) = adopt( ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_true( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-text-base_repo_id exp = 'project-a' ).
    SELECT SINGLE source_sha FROM zabgagt_so10meta WHERE data_client = @sy-mandt
      AND text_object = 'TEXT' AND text_name = 'ZSO10_TEST' AND text_id = 'ST' AND text_language = 'E'
      INTO @DATA(lv_sha).
    cl_abap_unit_assert=>assert_equals( act = lv_sha exp = ls_result-text-source_sha ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD adopt_changed_source.
    prepare_adopt( ).
    DATA(ls_result) = adopt( iv_sha = repeat( val = 'a' occ = 64 ) ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-conflict_kind exp = 'SOURCE_CHANGED' ).
    SELECT COUNT( * ) FROM zabgagt_so10meta INTO @DATA(lv_count).
    cl_abap_unit_assert=>assert_equals( act = lv_count exp = 0 ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD adopt_foreign_owner.
    DATA lt_base TYPE STANDARD TABLE OF zabgagt_so10meta WITH EMPTY KEY.
    lt_base = VALUE #( ( data_client = sy-mandt text_object = 'TEXT' text_name = 'ZSO10_TEST'
      text_id = 'ST' text_language = 'E' repo_id = 'other-project' source_sha = repeat( val = 'a' occ = 64 ) ) ).
    mo_sql->insert_test_data( lt_base ).
    DATA(ls_result) = adopt( ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-conflict_kind exp = 'OWNERSHIP' ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_called_times( 0 ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD adopt_missing_source.
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    lo_read->configure_call( )->when( lo_read->create_input_configuration( ) )->then_raise_classic_exception( 'NOT_FOUND' ).
    DATA(ls_result) = adopt( ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD adopt_foreign_lock.
    DATA(lo_lock) = mo_functions->get_double( 'ENQUEUE_ESSSTXT' ).
    lo_lock->configure_call( )->when( lo_lock->create_input_configuration( ) )->then_raise_classic_exception( 'FOREIGN_LOCK' ).
    DATA(ls_result) = adopt( ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'LOCK' ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_called_times( 0 ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.
  METHOD adopt_invalid_hash.
    DATA(ls_result) = adopt( iv_sha = 'weak-prefix' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'VALIDATION' ).
    mo_functions->get_double( 'INIT_TEXT' )->verify( )->is_called_times( 0 ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_called_times( 0 ).
  ENDMETHOD.
  METHOD adopt_fingerprint_failure.
    prepare_adopt( ).
    DATA(lo_store) = NEW lcl_so10_adopt_failure( ).
    lo_store->fail_fingerprint = abap_true.
    DATA(ls_result) = adopt( io_store = lo_store ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'SOURCE_CHECK' ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD adopt_metadata_failure.
    prepare_adopt( ).
    DATA(ls_result) = adopt( io_store = NEW lcl_so10_adopt_failure( ) ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'METADATA_RECORD' ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.
  METHOD adopt_unverified_commit.
    prepare_adopt( ).
    DATA(lo_store) = NEW lcl_so10_adopt_failure( ).
    lo_store->fail_record = abap_false.
    DATA(ls_result) = adopt( io_store = lo_store ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_not_bound( ls_result-baseline_recorded ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'METADATA_VERIFY' ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_times( 1 ).
  ENDMETHOD.

  METHOD read_observed_baseline.
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    DATA(lt_lines) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'hello' ) ).
    DATA(lo_output) = lo_read->create_output_configuration( )->set_table_parameter( name = 'LINES' value = lt_lines ).
    lo_read->configure_call( )->when( lo_read->create_input_configuration( ) )->then_set_output( lo_output ).
    DATA lt_base TYPE STANDARD TABLE OF zabgagt_so10meta WITH EMPTY KEY.
    lt_base = VALUE #( ( data_client = sy-mandt text_object = 'TEXT' text_name = 'ZSO10_TEST'
      text_id = 'ST' text_language = 'E' repo_id = 'owned' source_sha = repeat( val = 'a' occ = 64 ) ) ).
    mo_sql->insert_test_data( lt_base ).
    DATA(ls_text) = NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~read(
      VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' ) ).
    cl_abap_unit_assert=>assert_true( ls_text-base_found ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-base_repo_id exp = 'owned' ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-base_sha exp = repeat( val = 'a' occ = 64 ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-source_sha
      exp = 'b65181c01b75573cde621898e49a135c346e0f83a843c3768ee8dbebed9ee312' ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-data_client exp = CONV string( sy-mandt ) ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD missing_read_keeps_key.
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    lo_read->configure_call( )->when( lo_read->create_input_configuration( ) )->then_raise_classic_exception( 'NOT_FOUND' ).
    DATA(ls_text) = NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~read(
      VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'EN' ) ).
    cl_abap_unit_assert=>assert_false( ls_text-found ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-header-tdname exp = 'ZSO10_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-header-tdspras exp = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-source_sha
      exp = '4f53cda18c2baa0c0354bb5f9a3ecbe5ed12ab4d8e11ba873c2f11161202b945' ).
  ENDMETHOD.
  METHOD read_other_client_denied.
    DATA(lv_other) = CONV sy-mandt( COND string( WHEN sy-mandt = '123' THEN '124' ELSE '123' ) ).
    DATA lt_clients TYPE STANDARD TABLE OF t000 WITH EMPTY KEY.
    lt_clients = VALUE #( ( mandt = lv_other ) ).
    mo_sql->insert_test_data( lt_clients ).
    DATA(lo_auth) = mo_functions->get_double( 'AUTHORITY_CHECK' ).
    lo_auth->configure_call( )->when( lo_auth->create_input_configuration( ) )->then_raise_classic_exception( 'USER_NOT_AUTHORIZED' ).
    TRY.
        NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~read(
          is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
          iv_data_client = CONV #( lv_other ) ).
        cl_abap_unit_assert=>fail( 'Other-client READ needs administrative authorization' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'authorization denied' ) ).
    ENDTRY.
    mo_functions->get_double( 'INIT_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD read_other_client_scope.
    DATA(lv_other) = CONV sy-mandt( COND string( WHEN sy-mandt = '123' THEN '124' ELSE '123' ) ).
    DATA lt_clients TYPE STANDARD TABLE OF t000 WITH EMPTY KEY.
    lt_clients = VALUE #( ( mandt = lv_other ) ).
    mo_sql->insert_test_data( lt_clients ).
    DATA(lo_auth) = mo_functions->get_double( 'AUTHORITY_CHECK' ).
    lo_auth->configure_call( )->when( lo_auth->create_input_configuration( ) )->then_raise_classic_exception( 'USER_IS_AUTHORIZED' ).
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    DATA(lo_input) = lo_read->create_input_configuration( )->set_importing_parameter( name = 'CLIENT' value = lv_other ).
    DATA(lt_lines) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'Other client' ) ).
    DATA(lo_output) = lo_read->create_output_configuration( )->set_table_parameter( name = 'LINES' value = lt_lines ).
    lo_read->configure_call( )->when( lo_input )->then_set_output( lo_output ).
    DATA(ls_text) = NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~read(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
      iv_data_client = CONV #( lv_other ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-data_client exp = CONV string( lv_other ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-lines exp = lt_lines ).
    lo_read->verify( )->is_called_times( 1 ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD malformed_read_client.
    TRY.
        NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~read(
          is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' ) iv_data_client = '1234' ).
        cl_abap_unit_assert=>fail( 'Malformed client must fail before native calls' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'exactly three digits' ) ).
    ENDTRY.
    mo_functions->get_double( 'INIT_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'AUTHORITY_CHECK' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD expect_error.
    TRY.
        NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~save(
          is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
          it_lines = VALUE #( ( tdformat = '*' tdline = 'Content' ) )
          iv_action = iv_action iv_overwrite = iv_overwrite ).
        cl_abap_unit_assert=>fail( 'Expected native safeguard failure' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS iv_message ) ).
    ENDTRY.
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_called_times( iv_save_calls ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_called_times( iv_flush_calls ).
  ENDMETHOD.
  METHOD closed_client_rejected.
    DATA(lo_policy) = mo_functions->get_double( 'TR_SYS_PARAMS' ).
    DATA(lv_closed) = CONV t000-cccoractiv( '2' ).
    DATA(lo_output) = lo_policy->create_output_configuration( )->set_exporting_parameter(
      name = 'SYSTEM_CLIENT_EDIT' value = lv_closed ).
    lo_policy->configure_call( )->when( lo_policy->create_input_configuration( ) )->then_set_output( lo_output ).
    expect_error( 'does not permit changes' ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD unknown_policy_rejected.
    DATA(lo_policy) = mo_functions->get_double( 'TR_SYS_PARAMS' ).
    DATA(lv_unknown) = CONV t000-cccoractiv( '?' ).
    DATA(lo_output) = lo_policy->create_output_configuration( )->set_exporting_parameter(
      name = 'SYSTEM_CLIENT_EDIT' value = lv_unknown ).
    lo_policy->configure_call( )->when( lo_policy->create_input_configuration( ) )->then_set_output( lo_output ).
    expect_error( 'Unknown client-change policy' ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD existing_create_rejected.
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    DATA(ls_header) = VALUE thead( tdobject = 'TEXT' tdname = 'ZSO10_TEST' tdid = 'ST' tdspras = 'E' ).
    DATA(lo_output) = lo_read->create_output_configuration( )->set_exporting_parameter(
      name = 'HEADER' value = ls_header ).
    lo_read->configure_call( )->when( lo_read->create_input_configuration( ) )->then_set_output( lo_output ).
    expect_error( 'already exists' ).
    DATA(lo_lock) = mo_functions->get_double( 'ENQUEUE_ESSSTXT' ).
    DATA(lo_lock_key) = lo_lock->create_input_configuration(
      )->set_importing_parameter( name = 'MANDT' value = sy-mandt
      )->set_importing_parameter( name = 'TDOBJECT' value = ls_header-tdobject
      )->set_importing_parameter( name = 'TDNAME' value = ls_header-tdname
      )->set_importing_parameter( name = 'TDID' value = ls_header-tdid
      )->set_importing_parameter( name = 'TDSPRAS' value = ls_header-tdspras
      )->set_importing_parameter( name = '_SCOPE' value = '1' ).
    lo_lock->verify( lo_lock_key )->is_called_once( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD authorization_denied.
    mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'NO_AUTHORITY' ).
    expect_error( 'authorization denied' ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD foreign_lock_rejected.
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'FOREIGN_LOCK' ).
    expect_error( 'could not be locked' ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'READ_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD missing_update_rejected.
    mo_functions->get_double( 'READ_TEXT' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'NOT_FOUND' ).
    expect_error( iv_message = 'does not exist' iv_action = 'UPDATE' ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD save_failure_unlocks.
    mo_functions->get_double( 'READ_TEXT' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'NOT_FOUND' ).
    mo_functions->get_double( 'SAVE_TEXT' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'OBJECT' ).
    expect_error( iv_message = 'Native SO10 save failed' iv_save_calls = 1 ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD existing_copy_rejected.
    expect_error( iv_message = 'use --overwrite' iv_action = 'COPY' ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD copy_overwrite_saved.
    DATA(ls_header) = VALUE thead( tdobject = 'TEXT' tdname = 'ZSO10_TEST' tdid = 'ST' tdspras = 'E' ).
    DATA(lt_lines) = VALUE zif_abgagt_so10_api=>ty_native_lines( ( tdformat = '*' tdline = 'Content' ) ).
    DATA(lo_read) = mo_functions->get_double( 'READ_TEXT' ).
    DATA(lo_output) = lo_read->create_output_configuration(
      )->set_exporting_parameter( name = 'HEADER' value = ls_header
      )->set_table_parameter( name = 'LINES' value = lt_lines ).
    lo_read->configure_call( )->when( lo_read->create_input_configuration( ) )->then_set_output( lo_output ).
    DATA(ls_text) = NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~save(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
      it_lines = lt_lines iv_action = 'COPY' iv_overwrite = abap_true ).
    cl_abap_unit_assert=>assert_true( ls_text-found ).
    cl_abap_unit_assert=>assert_equals( act = ls_text-lines exp = lt_lines ).
    DATA(lo_save) = mo_functions->get_double( 'SAVE_TEXT' ).
    DATA(lo_save_input) = lo_save->create_input_configuration(
      )->set_importing_parameter( name = 'CLIENT' value = sy-mandt
      )->set_importing_parameter( name = 'INSERT' value = abap_false
      )->set_importing_parameter( name = 'SAVEMODE_DIRECT' value = abap_true ).
    lo_save->verify( lo_save_input )->is_called_once( ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_called_once( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD copy_create_auth_denied.
    mo_functions->get_double( 'READ_TEXT' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'NOT_FOUND' ).
    mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'NO_AUTHORITY' ).
    expect_error( iv_message = 'authorization denied' iv_action = 'COPY' iv_overwrite = abap_true ).
    DATA(lo_authority) = mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' ).
    lo_authority->verify( lo_authority->create_input_configuration(
      )->set_importing_parameter( name = 'ACTIVITY' value = CONV string( 'CREA' ) ) )->is_called_once( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD copy_edit_auth_denied.
    mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'NO_AUTHORITY' ).
    expect_error( iv_message = 'authorization denied' iv_action = 'COPY' iv_overwrite = abap_true ).
    DATA(lo_authority) = mo_functions->get_double( 'CHECK_TEXT_AUTHORITY' ).
    lo_authority->verify( lo_authority->create_input_configuration(
      )->set_importing_parameter( name = 'ACTIVITY' value = CONV string( 'EDIT' ) ) )->is_called_once( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD expect_record_error.
    TRY.
        NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~record_transport(
          is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' ) iv_request = iv_task ).
        cl_abap_unit_assert=>fail( 'Expected CTS safeguard failure' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS iv_message ) ).
    ENDTRY.
    mo_functions->get_double( 'TR_APPEND_TO_COMM_OBJS_KEYS' )->verify( )->is_called_times( iv_append_calls ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'COMMIT_TEXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD records_task_key.
    DATA(ls_transport) = NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~record_transport(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' ) iv_request = 'DEVK900002' ).
    cl_abap_unit_assert=>assert_equals( act = ls_transport-request exp = 'DEVK900001' ).
    cl_abap_unit_assert=>assert_equals( act = ls_transport-task exp = 'DEVK900002' ).
    cl_abap_unit_assert=>assert_equals( act = ls_transport-object_key exp = 'TEXT,ZSO10_TEST,ST,E' ).
    DATA(lo_append) = mo_functions->get_double( 'TR_APPEND_TO_COMM_OBJS_KEYS' ).
    DATA lt_objects TYPE STANDARD TABLE OF e071 WITH EMPTY KEY.
    DATA lt_keys TYPE STANDARD TABLE OF e071k WITH EMPTY KEY.
    lt_objects = VALUE #( ( trkorr = 'DEVK900002' pgmid = 'R3TR' object = 'TEXT' obj_name = 'TEXT,ZSO10_TEST,ST,E' ) ).
    DATA(lo_input) = lo_append->create_input_configuration(
      )->set_importing_parameter( name = 'WI_TRKORR' value = CONV e070-trkorr( 'DEVK900002' )
      )->set_importing_parameter( name = 'IV_DIALOG' value = abap_false
      )->set_table_parameter( name = 'WT_E071' value = lt_objects
      )->set_table_parameter( name = 'WT_E071K' value = lt_keys ).
    lo_append->verify( lo_input )->is_called_once( ).
    mo_functions->get_double( 'SAVE_TEXT' )->verify( )->is_never_called( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD resolves_parent_request.
    DATA(ls_transport) = NEW zcl_abgagt_so10_api( )->zif_abgagt_so10_api~record_transport(
      is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' ) iv_request = 'DEVK900001' ).
    cl_abap_unit_assert=>assert_equals( act = ls_transport-task exp = 'DEVK900002' ).
    mo_functions->get_double( 'TR_APPEND_TO_COMM_OBJS_KEYS' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD wrong_category_rejected.
    UPDATE e070 SET trfunction = 'K' WHERE trkorr = 'DEVK900001'.
    expect_record_error( iv_message = 'workbench transports are not accepted' iv_task = 'DEVK900001' ).
  ENDMETHOD.
  METHOD released_task_rejected.
    UPDATE e070 SET trstatus = 'R' WHERE trkorr = 'DEVK900002'.
    expect_record_error( 'not modifiable' ).
  ENDMETHOD.
  METHOD wrong_client_rejected.
    DATA(lv_other_client) = COND mandt( WHEN sy-mandt = '100' THEN '200' ELSE '100' ).
    UPDATE e070c SET client = @lv_other_client WHERE trkorr = 'DEVK900001'.
    expect_record_error( 'source client must match' ).
  ENDMETHOD.
  METHOD foreign_owner_rejected.
    DATA(lv_other_user) = sy-uname.
    lv_other_user(1) = COND #( WHEN sy-uname(1) = 'Z' THEN 'Y' ELSE 'Z' ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_other_user <> sy-uname ) ).
    UPDATE e070 SET as4user = @lv_other_user WHERE trkorr = 'DEVK900002'.
    expect_record_error( 'owned by the current user' ).
  ENDMETHOD.
  METHOD ambiguous_tasks_rejected.
    DATA lt_extra TYPE STANDARD TABLE OF e070 WITH EMPTY KEY.
    lt_extra = VALUE #( ( trkorr = 'DEVK900003' trfunction = 'Q' trstatus = 'D' strkorr = 'DEVK900001' as4user = sy-uname ) ).
    mo_sql->insert_test_data( lt_extra ).
    expect_record_error( iv_message = 'Exactly one owned' iv_task = 'DEVK900001' ).
  ENDMETHOD.
  METHOD missing_text_no_record.
    mo_functions->get_double( 'READ_TEXT' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'NOT_FOUND' ).
    expect_record_error( 'text does not exist' ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD cts_failure_unlocks.
    mo_functions->get_double( 'TR_APPEND_TO_COMM_OBJS_KEYS' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'TR_NO_AUTHORIZATION' ).
    expect_record_error( iv_message = 'Native CTS text recording failed' iv_append_calls = 1 ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD no_transport_policy.
    DATA(lo_policy) = mo_functions->get_double( 'TR_SYS_PARAMS' ).
    DATA(lo_output) = lo_policy->create_output_configuration(
      )->set_exporting_parameter( name = 'SYSTEM_CLIENT_EDIT' value = CONV t000-cccoractiv( '3' ) ).
    lo_policy->configure_call( )->when( lo_policy->create_input_configuration( ) )->then_set_output( lo_output ).
    expect_record_error( 'does not permit transport recording' ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD foreign_record_lock.
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'FOREIGN_LOCK' ).
    expect_record_error( 'could not be locked' ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_never_called( ).
  ENDMETHOD.
  METHOD missing_entry_detected.
    DELETE FROM e071 WHERE trkorr = 'DEVK900002'.
    expect_record_error( iv_message = 'could not be verified' iv_append_calls = 1 ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
  METHOD flush_failure_unlocks.
    mo_functions->get_double( 'READ_TEXT' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'NOT_FOUND' ).
    mo_functions->get_double( 'COMMIT_TEXT' )->configure_call( )->ignore_all_parameters(
      )->then_raise_classic_exception( 'OTHERS' ).
    expect_error( iv_message = 'text flush failed' iv_save_calls = 1 iv_flush_calls = 1 ).
    mo_functions->get_double( 'ENQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
    mo_functions->get_double( 'DEQUEUE_ESSSTXT' )->verify( )->is_called_once( ).
  ENDMETHOD.
ENDCLASS.
CLASS ltcl_so10_native_validation IMPLEMENTATION.
  METHOD empty_save_rejected.
    DATA(lo_api) = NEW zcl_abgagt_so10_api( ).
    TRY.
        lo_api->zif_abgagt_so10_api~save(
          is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
          it_lines = VALUE #( ) iv_action = 'CREATE' ).
        cl_abap_unit_assert=>fail( 'Empty save must fail before native functions' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'Empty text' ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD invalid_itf_rejected.
    DATA(lo_api) = NEW zcl_abgagt_so10_api( ).
    TRY.
        lo_api->zif_abgagt_so10_api~save(
          is_key = VALUE #( object = 'TEXT' name = 'ZSO10_TEST' id = 'ST' language = 'E' )
          it_lines = VALUE #( ( tdformat = '*' tdline = |First{ cl_abap_char_utilities=>newline }second| ) )
          iv_action = 'CREATE' ).
        cl_abap_unit_assert=>fail( 'Embedded delimiter must fail before native functions' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'embedded delimiters' ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD overlong_key_rejected.
    DATA(lo_api) = NEW zcl_abgagt_so10_api( ).
    TRY.
        lo_api->zif_abgagt_so10_api~read(
          VALUE #( object = 'TEXT' name = repeat( val = 'x' occ = 71 ) id = 'ST' language = 'E' ) ).
        cl_abap_unit_assert=>fail( 'Overlong Text name must fail before conversion or native reads' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_true( xsdbool( lx_error->get_text( ) CS 'Invalid SO10 text key' ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
