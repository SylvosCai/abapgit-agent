CLASS zcl_abgagt_so10_sync_store DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_so10_sync_store.
  PRIVATE SECTION.
    METHODS validate_key IMPORTING iv_data_client TYPE string is_key TYPE zif_abgagt_so10_api=>ty_key
      RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abgagt_so10_sync_store IMPLEMENTATION.
  METHOD validate_key.
    IF strlen( iv_data_client ) <> 3 OR iv_data_client CN '0123456789'
       OR is_key-object <> 'TEXT' OR strlen( is_key-name ) < 1 OR strlen( is_key-name ) > 70
       OR strlen( is_key-id ) < 1 OR strlen( is_key-id ) > 4
       OR is_key-id CN 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'
       OR strlen( is_key-language ) <> 1 OR is_key-language CN 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.
      zcx_abapgit_exception=>raise( 'Invalid complete SO10 metadata key' ).
    ENDIF.
    FIND PCRE `[\x00-\x1f\x7f*]` IN is_key-name.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( 'Invalid SO10 text name in metadata key' ).
    ENDIF.
    DATA(lv_native_name) = CONV thead-tdname( is_key-name ).
    IF strlen( lv_native_name ) <> strlen( is_key-name ).
      zcx_abapgit_exception=>raise( 'SO10 metadata keys cannot contain trailing name blanks' ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_so10_sync_store~fingerprint.
    rv_sha = zcl_abgagt_so10_sync_rules=>fingerprint( it_lines ).
  ENDMETHOD.

  METHOD zif_abgagt_so10_sync_store~load.
    validate_key( iv_data_client = iv_data_client is_key = is_key ).
    SELECT SINGLE repo_id, source_sha, source_user, source_date, source_time
      FROM zabgagt_so10meta
      WHERE data_client = @iv_data_client AND text_object = @is_key-object
        AND text_name = @is_key-name AND text_id = @is_key-id AND text_language = @is_key-language
      INTO @DATA(ls_metadata).
    IF sy-subrc = 0.
      rs_baseline = VALUE #( found = abap_true repo_id = ls_metadata-repo_id source_sha = ls_metadata-source_sha
        source_user = ls_metadata-source_user source_date = ls_metadata-source_date source_time = ls_metadata-source_time ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_so10_sync_store~remove.
    validate_key( iv_data_client = iv_data_client is_key = is_key ).
    FIND PCRE `\A[a-z0-9][a-z0-9._-]{0,79}\z` IN iv_repo_id.
    IF sy-subrc <> 0 OR strlen( iv_source_sha ) <> 64 OR iv_source_sha CN '0123456789abcdef'.
      zcx_abapgit_exception=>raise( 'Invalid SO10 repository identity or source fingerprint' ).
    ENDIF.
    DELETE FROM zabgagt_so10meta
      WHERE data_client = @iv_data_client AND text_object = @is_key-object
        AND text_name = @is_key-name AND text_id = @is_key-id AND text_language = @is_key-language
        AND repo_id = @iv_repo_id AND source_sha = @iv_source_sha.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'SO10 baseline removal failed; ownership or source changed' ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_so10_sync_store~record.
    validate_key( iv_data_client = iv_data_client is_key = is_key ).
    FIND PCRE `\A[a-z0-9][a-z0-9._-]{0,79}\z` IN iv_repo_id.
    IF sy-subrc <> 0 OR strlen( iv_source_sha ) <> 64 OR iv_source_sha CN '0123456789abcdef'.
      zcx_abapgit_exception=>raise( 'Invalid SO10 repository identity or source fingerprint' ).
    ENDIF.
    DATA lv_timestamp TYPE timestampl.
    GET TIME STAMP FIELD lv_timestamp.
    " The caller owns the full text-key lock; the predicate also prevents ownership replacement.
    UPDATE zabgagt_so10meta SET source_sha = @iv_source_sha, synced_at = @lv_timestamp, synced_by = @sy-uname,
      source_user = @is_header-tdluser, source_date = @is_header-tdldate, source_time = @is_header-tdltime
      WHERE data_client = @iv_data_client AND text_object = @is_key-object
        AND text_name = @is_key-name AND text_id = @is_key-id AND text_language = @is_key-language
        AND repo_id = @iv_repo_id.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    DATA(ls_metadata) = VALUE zabgagt_so10meta(
      data_client = iv_data_client text_object = is_key-object text_name = is_key-name
      text_id = is_key-id text_language = is_key-language repo_id = iv_repo_id
      source_sha = iv_source_sha synced_at = lv_timestamp synced_by = sy-uname
      source_user = is_header-tdluser source_date = is_header-tdldate source_time = is_header-tdltime ).
    TRY.
        INSERT zabgagt_so10meta FROM @ls_metadata.
      CATCH cx_sy_open_sql_db.
        zcx_abapgit_exception=>raise( 'SO10 baseline insert failed; repository ownership was not transferred' ).
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'SO10 baseline already exists or changed concurrently; ownership was not transferred' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
