CLASS lcl_so10_api DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_so10_api.
    DATA read_calls TYPE i.
    DATA read_client TYPE string.
    DATA save_calls TYPE i.
    DATA copy_calls TYPE i.
    DATA copy_source TYPE string.
    DATA copy_target TYPE string.
    DATA fail_copy TYPE abap_bool.
    DATA save_action TYPE string.
    DATA overwrite TYPE abap_bool.
    DATA fail_save TYPE abap_bool.
    DATA record_calls TYPE i.
    DATA fail_record TYPE abap_bool.
    DATA text TYPE zif_abgagt_so10_api=>ty_text.
ENDCLASS.
CLASS lcl_so10_api IMPLEMENTATION.
  METHOD zif_abgagt_so10_api~read.
    read_calls += 1.
    read_client = iv_data_client.
    rs_text = text.
  ENDMETHOD.
  METHOD zif_abgagt_so10_api~save.
    save_calls += 1.
    save_action = iv_action.
    overwrite = iv_overwrite.
    IF fail_save = abap_true.
      zcx_abapgit_exception=>raise( 'Save could not be verified' ).
    ENDIF.
    rs_text = text.
    rs_text-lines = it_lines.
  ENDMETHOD.
  METHOD zif_abgagt_so10_api~copy.
    copy_calls += 1.
    copy_source = COND #( WHEN iv_source_client IS INITIAL THEN CONV string( sy-mandt ) ELSE iv_source_client ).
    copy_target = iv_target_client.
    overwrite = iv_overwrite.
    IF fail_copy = abap_true.
      RAISE EXCEPTION TYPE cx_sy_zerodivide.
    ENDIF.
    rs_result = VALUE #( success = abap_true saved = NEW #( abap_true ) baseline_recorded = NEW #( abap_false )
      source_client = copy_source text = text ).
    rs_result-text-data_client = copy_target.
  ENDMETHOD.
  METHOD zif_abgagt_so10_api~record_transport.
    record_calls += 1.
    IF fail_record = abap_true.
      zcx_abapgit_exception=>raise( 'CTS entry could not be verified' ).
    ENDIF.
    rs_transport = VALUE #( request = iv_request task = 'DEVK900002'
      object_key = |TEXT,{ is_key-name },{ is_key-id },E| language = 'E' ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_so10_catalog DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_so10_catalog.
    DATA calls TYPE i.
    DATA prefixes TYPE string_table.
    DATA max_hits TYPE i.
    DATA result TYPE zif_abgagt_so10_api=>ty_catalog.
ENDCLASS.
CLASS lcl_so10_catalog IMPLEMENTATION.
  METHOD zif_abgagt_so10_catalog~search.
    calls += 1.
    prefixes = it_prefixes.
    max_hits = iv_max_hits.
    rs_result = result.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_so10_sync_api DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_so10_sync_api.
    DATA calls TYPE i.
    DATA client TYPE string.
    DATA repo_id TYPE string.
    DATA source_sha TYPE string.
    DATA fail_exception TYPE abap_bool.
    DATA result TYPE zif_abgagt_so10_api=>ty_write.
    DATA write_result TYPE zif_abgagt_so10_api=>ty_write.
    DATA write_calls TYPE i.
    DATA drop_calls TYPE i.
    DATA drop_result TYPE zif_abgagt_so10_api=>ty_write.
    DATA action TYPE string.
    DATA managed TYPE abap_bool.
    DATA ignore TYPE abap_bool.
ENDCLASS.
CLASS lcl_so10_sync_api IMPLEMENTATION.
  METHOD zif_abgagt_so10_sync_api~drop.
    drop_calls += 1.
    repo_id = iv_repo_id.
    client = iv_data_client.
    IF fail_exception = abap_true.
      RAISE EXCEPTION TYPE cx_sy_zerodivide.
    ENDIF.
    rs_result = drop_result.
  ENDMETHOD.
  METHOD zif_abgagt_so10_sync_api~write.
    write_calls += 1.
    action = iv_action.
    repo_id = iv_repo_id.
    managed = iv_managed.
    ignore = iv_ignore.
    IF fail_exception = abap_true.
      RAISE EXCEPTION TYPE cx_sy_zerodivide.
    ENDIF.
    rs_result = write_result.
    rs_result-text-lines = it_lines.
  ENDMETHOD.
  METHOD zif_abgagt_so10_sync_api~adopt.
    calls += 1.
    client = iv_data_client.
    repo_id = iv_repo_id.
    source_sha = iv_source_sha.
    IF fail_exception = abap_true.
      RAISE EXCEPTION TYPE cx_sy_zerodivide.
    ENDIF.
    rs_result = result.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_so10_command DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_api TYPE REF TO lcl_so10_api.
    DATA mo_catalog TYPE REF TO lcl_so10_catalog.
    DATA mo_sync_api TYPE REF TO lcl_so10_sync_api.
    DATA mo_command TYPE REF TO zif_abgagt_command.
    METHODS setup.
    METHODS reads_native_text FOR TESTING.
    METHODS read_client_observation FOR TESTING.
    METHODS missing_read_normalized FOR TESTING.
    METHODS list_headers_only FOR TESTING.
    METHODS list_requires_prefix FOR TESTING.
    METHODS list_overflow_no_bodies FOR TESTING.
    METHODS list_cap_rejected FOR TESTING.
    METHODS saved_text_returned FOR TESTING.
    METHODS empty_text_rejected FOR TESTING.
    METHODS overlong_line_rejected FOR TESTING.
    METHODS client_override_rejected FOR TESTING.
    METHODS save_transport_separate FOR TESTING.
    METHODS save_outcome_unknown FOR TESTING.
    METHODS copies_target_text FOR TESTING.
    METHODS invalid_copy_clients FOR TESTING.
    METHODS copy_default_source FOR TESTING.
    METHODS copy_rejects_payload FOR TESTING.
    METHODS copy_unknown_saved FOR TESTING.
    METHODS target_only_copy FOR TESTING.
    METHODS records_text_key FOR TESTING.
    METHODS transport_needs_request FOR TESTING.
    METHODS transport_no_content FOR TESTING.
    METHODS recording_outcome_unknown FOR TESTING.
    METHODS adopts_observed_source FOR TESTING.
    METHODS adoption_no_content FOR TESTING.
    METHODS adoption_needs_context FOR TESTING.
    METHODS sync_context_not_legacy_save FOR TESTING.
    METHODS standalone_needs_ignore FOR TESTING.
    METHODS standalone_ignore_routed FOR TESTING.
    METHODS write_conflict_context FOR TESTING.
    METHODS write_metadata_failure FOR TESTING.
    METHODS write_metadata_unknown FOR TESTING.
    METHODS write_already_matches FOR TESTING.
    METHODS write_context_rejected FOR TESTING.
    METHODS write_api_required FOR TESTING.
    METHODS adoption_metadata_unknown FOR TESTING.
    METHODS adoption_exception_unknown FOR TESTING.
    METHODS drop_managed_routed FOR TESTING.
    METHODS drop_requires_context FOR TESTING.
    METHODS drop_rejects_bypasses FOR TESTING.
    METHODS drop_partial_unknown FOR TESTING.
ENDCLASS.
CLASS ltcl_so10_command IMPLEMENTATION.
  METHOD drop_managed_routed.
    mo_sync_api->drop_result = VALUE #( success = abap_true saved = NEW #( abap_false )
      deleted = NEW #( abap_true ) baseline_removed = NEW #( abap_true )
      text = VALUE #( data_client = '123' header = VALUE #( tdspras = 'E' ) ) ).
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'DROP'
      name = 'ZTEST' id = 'ST' language = 'E' repo_id = 'owned' target_client = '123'
      lines = VALUE #( ( tdformat = '*' tdline = 'committed' ) ) ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_true( ls_result-deleted->* ).
    cl_abap_unit_assert=>assert_true( ls_result-baseline_removed->* ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->repo_id exp = 'owned' ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->client exp = '123' ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD drop_requires_context.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'DROP'
      name = 'ZTEST' id = 'ST' language = 'E' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'managed repository' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->drop_calls exp = 0 ).
  ENDMETHOD.
  METHOD drop_rejects_bypasses.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'DROP'
      name = 'ZTEST' id = 'ST' language = 'E' repo_id = 'owned' conflict_mode = 'ignore' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'Conflict mode is only' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->drop_calls exp = 0 ).
    lv_json = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'DROP'
      name = 'ZTEST' id = 'ST' language = 'E' repo_id = 'owned' target_client = 'bad'
      lines = VALUE #( ( tdformat = '*' tdline = 'committed' ) ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'Invalid target data client' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->drop_calls exp = 0 ).
  ENDMETHOD.
  METHOD drop_partial_unknown.
    mo_sync_api->drop_result = VALUE #( saved = NEW #( abap_false ) deleted = NEW #( abap_true )
      baseline_removed = NEW #( abap_false ) error = 'removal failed' ).
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'DROP'
      name = 'ZTEST' id = 'ST' language = 'E' repo_id = 'owned'
      lines = VALUE #( ( tdformat = '*' tdline = 'committed' ) ) ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-deleted->* ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_removed->* ).
    mo_sync_api->fail_exception = abap_true.
    lv_json = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'DROP'
      name = 'ZTEST' id = 'ST' language = 'E' repo_id = 'owned'
      lines = VALUE #( ( tdformat = '*' tdline = 'committed' ) ) ) ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_not_bound( ls_result-deleted ).
    cl_abap_unit_assert=>assert_not_bound( ls_result-baseline_removed ).
  ENDMETHOD.
  METHOD setup.
    mo_api = NEW #( ).
    mo_api->text = VALUE #( found = abap_true header = VALUE #( tdspras = 'E' )
      lines = VALUE #( ( tdformat = '*' tdline = 'Example text' ) ) ).
    mo_catalog = NEW #( ).
    mo_sync_api = NEW #( ).
    mo_sync_api->result = VALUE #( success = abap_true saved = NEW #( abap_false )
      baseline_recorded = NEW #( abap_true ) stage = 'COMPLETE'
      text = VALUE #( data_client = '123' header = VALUE #( tdspras = 'E' )
        source_sha = repeat( val = 'a' occ = 64 ) base_found = abap_true
        base_repo_id = 'owned' base_sha = repeat( val = 'a' occ = 64 ) ) ).
    mo_sync_api->write_result = mo_sync_api->result.
    mo_sync_api->write_result-saved = NEW #( abap_true ).
    mo_command = NEW zcl_abgagt_command_so10( io_api = mo_api io_catalog = mo_catalog io_sync_api = mo_sync_api ).
  ENDMETHOD.
  METHOD adopts_observed_source.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'ADOPT' name = 'NAME' language = 'EN' client = '123'
      repo_id = 'owned' source_sha = repeat( val = 'a' occ = 64 ) ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_true( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->client exp = '123' ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->repo_id exp = 'owned' ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->source_sha exp = ls_result-base_sha ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-language exp = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-data_client exp = '123' ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD adoption_no_content.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'ADOPT' name = 'NAME' language = 'E' repo_id = 'owned'
      source_sha = repeat( val = 'a' occ = 64 ) lines = VALUE #( ( tdformat = '*' tdline = 'content' ) ) ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD adoption_needs_context.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'ADOPT' name = 'NAME' language = 'E' ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->calls exp = 0 ).
  ENDMETHOD.
  METHOD sync_context_not_legacy_save.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'UPDATE' name = 'NAME' language = 'E' repo_id = 'owned'
      lines = VALUE #( ( tdformat = '*' tdline = 'content' ) ) ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->write_calls exp = 1 ).
    cl_abap_unit_assert=>assert_true( mo_sync_api->managed ).
    cl_abap_unit_assert=>assert_false( mo_sync_api->ignore ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->repo_id exp = 'owned' ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD adoption_metadata_unknown.
    mo_sync_api->result-success = abap_false.
    CLEAR mo_sync_api->result-baseline_recorded.
    mo_sync_api->result-stage = 'METADATA_VERIFY'.
    mo_sync_api->result-error = 'Metadata could not be verified'.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'ADOPT' name = 'NAME' language = 'E' repo_id = 'owned' source_sha = repeat( val = 'a' occ = 64 ) ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_not_bound( ls_result-baseline_recorded ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'METADATA_VERIFY' ).
  ENDMETHOD.
  METHOD adoption_exception_unknown.
    mo_sync_api->fail_exception = abap_true.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'ADOPT' name = 'NAME' language = 'E' repo_id = 'owned' source_sha = repeat( val = 'a' occ = 64 ) ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_not_bound( ls_result-baseline_recorded ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-stage exp = 'ADOPT' ).
  ENDMETHOD.

  METHOD reads_native_text.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'READ' name = 'Mixed case text' language = 'EN' ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-name exp = 'Mixed case text' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-id exp = 'ST' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-language exp = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->read_calls exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD read_client_observation.
    mo_api->text-data_client = '123'.
    mo_api->text-source_sha = repeat( val = 'a' occ = 64 ).
    mo_api->text-base_found = abap_true.
    mo_api->text-base_repo_id = 'owned'.
    mo_api->text-base_sha = repeat( val = 'b' occ = 64 ).
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'READ' name = 'NAME' language = 'EN' client = '123' ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->read_client exp = '123' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-data_client exp = '123' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-source_sha exp = mo_api->text-source_sha ).
    cl_abap_unit_assert=>assert_true( ls_result-base_found ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-base_repo_id exp = 'owned' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-base_sha exp = mo_api->text-base_sha ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD missing_read_normalized.
    mo_api->text-found = abap_false.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'READ' name = 'NAME' language = 'EN' ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-error_kind exp = 'NOT_FOUND' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-language exp = 'E' ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
  ENDMETHOD.
  METHOD list_headers_only.
    mo_catalog->result = VALUE #( max_hits = 100 at_least = 1
      keys = VALUE #( ( object = 'TEXT' name = 'NAME' id = 'ST' language = 'E' ) ) ).
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'LIST' prefixes = VALUE #( ( `ZAPP_` ) ( `YAPP_` ) ) ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-keys exp = mo_catalog->result-keys ).
    cl_abap_unit_assert=>assert_equals( act = mo_catalog->prefixes exp = VALUE string_table( ( `ZAPP_` ) ( `YAPP_` ) ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_catalog->max_hits exp = 100 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->read_calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD list_requires_prefix.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'LIST' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'explicit prefixes' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_catalog->calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->read_calls exp = 0 ).
  ENDMETHOD.
  METHOD list_overflow_no_bodies.
    mo_catalog->result = VALUE #( max_hits = 100 at_least = 101 has_more = abap_true
      keys = VALUE #( ( object = 'TEXT' name = 'NOT_TO_EXPOSE' id = 'ST' language = 'E' ) ) ).
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'LIST' prefixes = VALUE #( ( `ZAPP_` ) ) ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-error_kind exp = 'OVERFLOW' ).
    cl_abap_unit_assert=>assert_initial( ls_result-keys ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-at_least exp = 101 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->read_calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD list_cap_rejected.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'LIST' prefixes = VALUE #( ( `ZAPP_` ) ) max_hits = 1001 ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'between 1 and 1000' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_catalog->calls exp = 0 ).
  ENDMETHOD.
  METHOD saved_text_returned.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'CREATE' name = 'NAME' language = 'E' repo_id = 'owned'
      lines = VALUE #( ( tdformat = '*' tdline = 'Saved text' ) ) ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_bound( ls_result-saved ).
    cl_abap_unit_assert=>assert_true( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-lines exp = ls_params-lines ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-line_count exp = 1 ).
    cl_abap_unit_assert=>assert_false( ls_result-transport_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->write_calls exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD empty_text_rejected.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'CREATE' name = 'NAME' language = 'E' repo_id = 'owned' lines = VALUE #( ( tdline = ' ' ) ) ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'Empty text is not supported' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD overlong_line_rejected.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'UPDATE' name = 'NAME' language = 'E' repo_id = 'owned'
      lines = VALUE #( ( tdline = repeat( val = 'x' occ = 133 ) ) ) ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'native ITF field width' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD client_override_rejected.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'CREATE' name = 'NAME' language = 'E' client = '200'
      lines = VALUE #( ( tdline = 'Text' ) ) ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'Client overrides are not permitted' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD save_transport_separate.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'CREATE' name = 'NAME' language = 'E' transport = 'DEVK900001'
      lines = VALUE #( ( tdline = 'Text' ) ) ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'separate SO10 operations' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD copies_target_text.
    DATA(lv_source_client) = COND string( WHEN sy-mandt = '123' THEN '124' ELSE '123' ).
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'COPY' name = 'NAME' language = 'E' source_client = lv_source_client
      target_client = CONV #( sy-mandt ) overwrite = abap_true ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-source_client exp = lv_source_client ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-client exp = CONV string( sy-mandt ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-lines exp = CORRESPONDING zcl_abgagt_command_so10=>ty_lines( mo_api->text-lines ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->copy_calls exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->copy_target exp = CONV string( sy-mandt ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
    cl_abap_unit_assert=>assert_true( mo_api->overwrite ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->read_calls exp = 0 ).
    cl_abap_unit_assert=>assert_false( ls_result-transport_recorded->* ).
  ENDMETHOD.
  METHOD invalid_copy_clients.
    DATA(lt_clients) = VALUE string_table( ( `` ) ( `bad` ) ( `1234` ) ).
    LOOP AT lt_clients INTO DATA(lv_client).
      DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
        action = 'COPY' name = 'NAME' language = 'E' target_client = lv_client ).
      DATA(lv_json) = mo_command->execute( ls_params ).
      cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'three-digit data clients' ) ).
      cl_abap_unit_assert=>assert_equals( act = mo_api->copy_calls exp = 0 ).
      cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
    ENDLOOP.
  ENDMETHOD.
  METHOD copy_default_source.
    DATA(lv_target) = COND string( WHEN sy-mandt = '123' THEN '124' ELSE '123' ).
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'COPY' name = 'NAME' language = 'E' target_client = lv_target ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-source_client exp = CONV string( sy-mandt ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-target_client exp = lv_target ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->copy_calls exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->read_calls exp = 0 ).
  ENDMETHOD.
  METHOD copy_rejects_payload.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'COPY' name = 'NAME' language = 'E' target_client = '123'
      lines = VALUE #( ( tdline = 'untrusted snapshot' ) ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'no content or discovery payload' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->copy_calls exp = 0 ).
    lv_json = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'COPY' name = 'NAME' language = 'E' target_client = '123' repo_id = 'project-a' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'Repository context is only supported' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->copy_calls exp = 0 ).
  ENDMETHOD.
  METHOD copy_unknown_saved.
    mo_api->fail_copy = abap_true.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'COPY' name = 'NAME' language = 'E' target_client = '123' ) ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_false( ls_result-success ).
    cl_abap_unit_assert=>assert_initial( ls_result-saved ).
    cl_abap_unit_assert=>assert_false( ls_result-baseline_recorded->* ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD target_only_copy.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params( action = 'READ' name = 'NAME' language = 'E' target_client = '123' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'only valid for COPY' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->read_calls exp = 0 ).
  ENDMETHOD.
  METHOD records_text_key.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'TRANSPORT' name = 'Mixed name' language = 'EN' transport = 'DEVK900001' ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    DATA ls_result TYPE zcl_abgagt_command_so10=>ty_result.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json CHANGING data = ls_result ).
    cl_abap_unit_assert=>assert_true( ls_result-success ).
    cl_abap_unit_assert=>assert_true( ls_result-transport_recorded->* ).
    cl_abap_unit_assert=>assert_false( ls_result-saved->* ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-transport_request exp = 'DEVK900001' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-transport_task exp = 'DEVK900002' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-object_key exp = 'TEXT,Mixed name,ST,E' ).
    cl_abap_unit_assert=>assert_equals( act = ls_result-language exp = 'E' ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->record_calls exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
    cl_abap_unit_assert=>assert_initial( ls_result-lines ).
  ENDMETHOD.
  METHOD transport_needs_request.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params( action = 'TRANSPORT' name = 'NAME' language = 'E' ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'request/task number is required' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->record_calls exp = 0 ).
  ENDMETHOD.
  METHOD transport_no_content.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'TRANSPORT' name = 'NAME' language = 'E' transport = 'DEVK900001'
      lines = VALUE #( ( tdline = 'Unwanted content' ) ) ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'TRANSPORT does not accept text content' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->record_calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD recording_outcome_unknown.
    mo_api->fail_record = abap_true.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'TRANSPORT' name = 'NAME' language = 'E' transport = 'DEVK900001' ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'Recording outcome may be unknown' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"transport_recorded":null' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"saved":false' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->record_calls exp = 1 ).
  ENDMETHOD.
  METHOD standalone_needs_ignore.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'UPDATE' name = 'NAME' language = 'E' lines = VALUE #( ( tdline = 'Text' ) ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'Standalone SO10 writes require explicit conflict-ignore' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"saved":false' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->write_calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD standalone_ignore_routed.
    mo_sync_api->write_result-baseline_recorded = NEW #( abap_false ).
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'UPDATE' name = 'NAME' language = 'E' conflict_mode = 'ignore'
      lines = VALUE #( ( tdline = 'Text' ) ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"success":true' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"baseline_recorded":false' ) ).
    cl_abap_unit_assert=>assert_false( mo_sync_api->managed ).
    cl_abap_unit_assert=>assert_true( mo_sync_api->ignore ).
    cl_abap_unit_assert=>assert_initial( mo_sync_api->repo_id ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->action exp = 'UPDATE' ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD write_conflict_context.
    mo_sync_api->write_result = VALUE #( error = 'Owned by another repository' conflict_kind = 'OWNERSHIP'
      stage = 'SOURCE_CHECK' saved = NEW #( abap_false ) baseline_recorded = NEW #( abap_false ) ).
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'UPDATE' name = 'NAME' language = 'E' repo_id = 'owned' conflict_mode = 'ignore'
      lines = VALUE #( ( tdline = 'Text' ) ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"conflict_kind":"OWNERSHIP"' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"success":false' ) ).
    cl_abap_unit_assert=>assert_true( mo_sync_api->managed ).
    cl_abap_unit_assert=>assert_true( mo_sync_api->ignore ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->repo_id exp = 'owned' ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD write_metadata_failure.
    mo_sync_api->write_result-success = abap_false.
    mo_sync_api->write_result-baseline_recorded = NEW #( abap_false ).
    mo_sync_api->write_result-stage = 'METADATA_RECORD'.
    mo_sync_api->write_result-error = 'Metadata record failed after verified save'.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'CREATE' name = 'NAME' language = 'E' repo_id = 'owned' lines = VALUE #( ( tdline = 'Text' ) ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"success":false' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"saved":true' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"baseline_recorded":false' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"stage":"METADATA_RECORD"' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD write_metadata_unknown.
    mo_sync_api->write_result-success = abap_false.
    CLEAR mo_sync_api->write_result-baseline_recorded.
    mo_sync_api->write_result-stage = 'METADATA_VERIFY'.
    mo_sync_api->write_result-error = 'Metadata commit unverified'.
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'UPDATE' name = 'NAME' language = 'E' repo_id = 'owned' lines = VALUE #( ( tdline = 'Text' ) ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"success":false' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"saved":true' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"baseline_recorded":null' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"stage":"METADATA_VERIFY"' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD write_already_matches.
    mo_sync_api->write_result-saved = NEW #( abap_false ).
    DATA(lv_json) = mo_command->execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'UPDATE' name = 'NAME' language = 'E' repo_id = 'owned' lines = VALUE #( ( tdline = 'Text' ) ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"success":true' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"saved":false' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"baseline_recorded":true' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'already matches' ) ).
  ENDMETHOD.
  METHOD write_context_rejected.
    TYPES ty_requests TYPE STANDARD TABLE OF zcl_abgagt_command_so10=>ty_params WITH EMPTY KEY.
    DATA(lt_requests) = VALUE ty_requests(
      ( action = 'UPDATE' conflict_mode = 'overwrite' )
      ( action = 'READ' conflict_mode = 'ignore' )
      ( action = 'UPDATE' source_sha = repeat( val = 'a' occ = 64 ) )
      ( action = 'UPDATE' prefixes = VALUE #( ( `NAME` ) ) ) ).
    LOOP AT lt_requests INTO DATA(ls_request).
      ls_request-name = 'NAME'.
      ls_request-language = 'E'.
      ls_request-repo_id = 'owned'.
      ls_request-lines = VALUE #( ( tdline = 'Text' ) ).
      DATA(lv_json) = mo_command->execute( ls_request ).
      cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"success":false' ) ).
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->write_calls exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD write_api_required.
    DATA(lo_without_sync) = NEW zcl_abgagt_command_so10( io_api = mo_api io_catalog = mo_catalog ).
    DATA(lv_json) = lo_without_sync->zif_abgagt_command~execute( VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'UPDATE' name = 'NAME' language = 'E' repo_id = 'owned' lines = VALUE #( ( tdline = 'Text' ) ) ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'synchronized write API' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"saved":false' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
  METHOD save_outcome_unknown.
    mo_sync_api->fail_exception = abap_true.
    DATA(ls_params) = VALUE zcl_abgagt_command_so10=>ty_params(
      action = 'UPDATE' name = 'NAME' language = 'E' repo_id = 'owned' lines = VALUE #( ( tdline = 'Text' ) ) ).
    DATA(lv_json) = mo_command->execute( ls_params ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS 'Save outcome may be unknown' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"saved":null' ) ).
    cl_abap_unit_assert=>assert_true( xsdbool( lv_json CS '"baseline_recorded":null' ) ).
    cl_abap_unit_assert=>assert_equals( act = mo_sync_api->write_calls exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = mo_api->save_calls exp = 0 ).
  ENDMETHOD.
ENDCLASS.
