CLASS zcl_abgagt_so10_api DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_so10_api.
    INTERFACES zif_abgagt_so10_sync_api.
    METHODS constructor IMPORTING io_sync_store TYPE REF TO zif_abgagt_so10_sync_store OPTIONAL.
  PRIVATE SECTION.
    DATA mo_sync_store TYPE REF TO zif_abgagt_so10_sync_store.
    METHODS make_header IMPORTING is_key TYPE zif_abgagt_so10_api=>ty_key
      RETURNING VALUE(rs_header) TYPE thead RAISING zcx_abapgit_exception.
    METHODS authorize IMPORTING is_header TYPE thead iv_activity TYPE string
      RAISING zcx_abapgit_exception.
    METHODS read_native IMPORTING is_header TYPE thead iv_data_client TYPE sy-mandt DEFAULT sy-mandt
      RETURNING VALUE(rs_text) TYPE zif_abgagt_so10_api=>ty_text RAISING zcx_abapgit_exception.
    METHODS unlock IMPORTING is_header TYPE thead iv_data_client TYPE sy-mandt DEFAULT sy-mandt.
    METHODS check_client_changeable IMPORTING iv_transport TYPE abap_bool DEFAULT abap_false
      iv_data_client TYPE sy-mandt DEFAULT sy-mandt
      RAISING zcx_abapgit_exception.
    METHODS validate_write_lines IMPORTING it_lines TYPE zif_abgagt_so10_api=>ty_native_lines
      RAISING zcx_abapgit_exception.
    METHODS save_text
      IMPORTING is_key TYPE zif_abgagt_so10_api=>ty_key it_lines TYPE zif_abgagt_so10_api=>ty_native_lines
        iv_action TYPE string iv_overwrite TYPE abap_bool DEFAULT abap_false
        iv_sync TYPE abap_bool DEFAULT abap_false iv_repo_id TYPE string OPTIONAL
        iv_managed TYPE abap_bool DEFAULT abap_true iv_ignore TYPE abap_bool DEFAULT abap_false
        iv_data_client TYPE sy-mandt DEFAULT sy-mandt
      RETURNING VALUE(rs_result) TYPE zif_abgagt_so10_api=>ty_write.
    METHODS complete_baseline IMPORTING iv_repo_id TYPE string iv_source_sha TYPE string
      CHANGING cs_result TYPE zif_abgagt_so10_api=>ty_write RAISING zcx_abapgit_exception.
    METHODS resolve_request IMPORTING iv_request TYPE string
      RETURNING VALUE(rs_transport) TYPE zif_abgagt_so10_api=>ty_transport RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abgagt_so10_api IMPLEMENTATION.
  METHOD constructor.
    mo_sync_store = COND #( WHEN io_sync_store IS BOUND THEN io_sync_store ELSE NEW zcl_abgagt_so10_sync_store( ) ).
  ENDMETHOD.

  METHOD make_header.
    IF is_key-object <> 'TEXT' OR is_key-name IS INITIAL OR strlen( is_key-name ) > 70
      OR is_key-id IS INITIAL OR strlen( is_key-id ) > 4
      OR is_key-language IS INITIAL OR strlen( is_key-language ) > 2.
      zcx_abapgit_exception=>raise( 'Invalid SO10 text key' ).
    ENDIF.
    FIND PCRE `[\x00-\x1f\x7f*]|\x20\z` IN is_key-name.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( 'SO10 requires one exact text key' ).
    ENDIF.
    FIND PCRE `[^A-Za-z0-9_]` IN is_key-id.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( 'Invalid Text ID' ).
    ENDIF.
    DATA lv_language TYPE spras.
    IF strlen( is_key-language ) = 2.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING input = is_key-language
        IMPORTING output = lv_language
        EXCEPTIONS unknown_language = 1 OTHERS = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Unknown ISO language' ).
      ENDIF.
    ELSE.
      lv_language = is_key-language.
    ENDIF.
    DATA lt_initial_lines TYPE zif_abgagt_so10_api=>ty_native_lines.
    CALL FUNCTION 'INIT_TEXT'
      EXPORTING id = CONV thead-tdid( is_key-id ) language = lv_language
                name = CONV thead-tdname( is_key-name ) object = CONV thead-tdobject( is_key-object )
      IMPORTING header = rs_header
      TABLES lines = lt_initial_lines
      EXCEPTIONS id = 1 language = 2 name = 3 object = 4 OTHERS = 5.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'SO10 text object, Text ID, name or language is not configured' ).
    ENDIF.
    IF rs_header-tdtexttype IS NOT INITIAL.
      zcx_abapgit_exception=>raise( 'Only native ITF standard texts are supported' ).
    ENDIF.
  ENDMETHOD.

  METHOD authorize.
    CALL FUNCTION 'CHECK_TEXT_AUTHORITY'
      EXPORTING activity = iv_activity id = is_header-tdid language = is_header-tdspras
                name = is_header-tdname object = is_header-tdobject
      EXCEPTIONS no_authority = 1 OTHERS = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |SO10 authorization denied for activity { iv_activity }| ).
    ENDIF.
  ENDMETHOD.

  METHOD read_native.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING client = iv_data_client object = is_header-tdobject name = is_header-tdname
                id = is_header-tdid language = is_header-tdspras
      IMPORTING header = rs_text-header
      TABLES lines = rs_text-lines
      EXCEPTIONS id = 1 language = 2 name = 3 not_found = 4 object = 5
                 reference_check = 6 wrong_access_to_archive = 7 OTHERS = 8.
    CASE sy-subrc.
      WHEN 0.
        IF rs_text-header-tdtexttype IS NOT INITIAL OR rs_text-header-tdrefobj IS NOT INITIAL
          OR rs_text-header-tdrefname IS NOT INITIAL.
          zcx_abapgit_exception=>raise( 'External or reference texts are not supported' ).
        ENDIF.
        rs_text-found = abap_true.
      WHEN 4.
        rs_text-found = abap_false.
        rs_text-header = is_header.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'SO10 text could not be read' ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_abgagt_so10_api~read.
    DATA(lv_client) = zcl_abgagt_so10_catalog=>data_client( iv_data_client ).
    DATA(ls_header) = make_header( is_key ).
    authorize( is_header = ls_header iv_activity = 'SHOW' ).
    rs_text = read_native( is_header = ls_header iv_data_client = lv_client ).
    rs_text-data_client = lv_client.
    rs_text-source_sha = mo_sync_store->fingerprint( rs_text-lines ).
    DATA(ls_canonical_key) = VALUE zif_abgagt_so10_api=>ty_key( object = ls_header-tdobject name = ls_header-tdname
      id = ls_header-tdid language = ls_header-tdspras ).
    DATA(ls_baseline) = mo_sync_store->load( iv_data_client = CONV #( lv_client ) is_key = ls_canonical_key ).
    rs_text-base_found = ls_baseline-found.
    rs_text-base_repo_id = ls_baseline-repo_id.
    rs_text-base_sha = ls_baseline-source_sha.
  ENDMETHOD.

  METHOD zif_abgagt_so10_sync_api~drop.
    rs_result-saved = NEW #( abap_false ).
    rs_result-baseline_recorded = NEW #( abap_false ).
    rs_result-deleted = NEW #( abap_false ).
    rs_result-baseline_removed = NEW #( abap_false ).
    rs_result-stage = 'VALIDATION'.
    DATA lv_locked TYPE abap_bool.
    DATA lv_client TYPE sy-mandt.
    DATA ls_header TYPE thead.
    TRY.
        FIND PCRE `\A[a-z0-9][a-z0-9._-]{0,79}\z` IN iv_repo_id.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'DROP requires a valid repository identity' ).
        ENDIF.
        validate_write_lines( it_lines ).
        DATA(lv_committed_sha) = mo_sync_store->fingerprint( it_lines ).
        lv_client = zcl_abgagt_so10_catalog=>data_client( iv_data_client ).
        ls_header = make_header( is_key ).
        authorize( is_header = ls_header iv_activity = 'SHOW' ).
        authorize( is_header = ls_header iv_activity = 'EDIT' ).
        check_client_changeable( iv_data_client = lv_client ).
        DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = ls_header-tdobject
          name = ls_header-tdname id = ls_header-tdid language = ls_header-tdspras ).
        rs_result-text-header = ls_header.
        rs_result-text-data_client = lv_client.
        rs_result-stage = 'LOCK'.
        CALL FUNCTION 'ENQUEUE_ESSSTXT'
          EXPORTING mode_stxh = 'E' mandt = lv_client tdobject = ls_header-tdobject
            tdname = ls_header-tdname tdid = ls_header-tdid tdspras = ls_header-tdspras _scope = '1' _wait = abap_false
          EXCEPTIONS foreign_lock = 1 system_failure = 2 OTHERS = 3.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'DROP text is locked; no deletion attempted' ).
        ENDIF.
        lv_locked = abap_true.
        rs_result-stage = 'SOURCE_CHECK'.
        DATA(ls_base) = mo_sync_store->load( iv_data_client = CONV #( lv_client ) is_key = ls_key ).
        IF ls_base-found = abap_false.
          rs_result-conflict_kind = 'BASELINE_MISSING'.
          zcx_abapgit_exception=>raise( 'DROP requires an explicitly adopted repository baseline' ).
        ENDIF.
        IF ls_base-repo_id <> iv_repo_id.
          rs_result-conflict_kind = 'OWNERSHIP'.
          zcx_abapgit_exception=>raise( 'DROP text belongs to another repository' ).
        ENDIF.
        IF ls_base-source_sha <> lv_committed_sha.
          rs_result-conflict_kind = 'SOURCE_CHANGED'.
          zcx_abapgit_exception=>raise( 'DROP baseline does not match committed ITF; IMPORT/review/commit first' ).
        ENDIF.
        DATA(ls_source) = read_native( is_header = ls_header iv_data_client = lv_client ).
        IF ls_source-found = abap_true.
          IF mo_sync_store->fingerprint( ls_source-lines ) <> lv_committed_sha.
            rs_result-conflict_kind = 'SOURCE_CHANGED'.
            zcx_abapgit_exception=>raise( 'DROP source differs from committed ITF; preserve it and IMPORT/review/commit first' ).
          ENDIF.
          rs_result-stage = 'TEXT_DELETE'.
          CLEAR rs_result-deleted.
          DATA lv_invalid_handler TYPE abap_bool.
          CALL FUNCTION 'DELETE_TEXT'
            EXPORTING client = lv_client object = ls_header-tdobject name = ls_header-tdname
              id = ls_header-tdid language = ls_header-tdspras savemode_direct = abap_true
            IMPORTING invalid_persistence_handler = lv_invalid_handler
            EXCEPTIONS not_found = 1 OTHERS = 2.
          IF sy-subrc <> 0 OR lv_invalid_handler = abap_true.
            zcx_abapgit_exception=>raise( 'Native SO10 deletion failed' ).
          ENDIF.
          rs_result-stage = 'TEXT_COMMIT'.
          COMMIT WORK AND WAIT.
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( 'SO10 deletion commit could not be verified' ).
          ENDIF.
          rs_result-stage = 'TEXT_VERIFY'.
          DATA(ls_deleted) = read_native( is_header = ls_header iv_data_client = lv_client ).
          IF ls_deleted-found = abap_true.
            zcx_abapgit_exception=>raise( 'SO10 text remains after deletion; baseline preserved' ).
          ENDIF.
        ENDIF.
        rs_result-deleted = NEW #( abap_true ).
        rs_result-stage = 'METADATA_REMOVE'.
        mo_sync_store->remove( iv_data_client = CONV #( lv_client ) is_key = ls_key
          iv_repo_id = iv_repo_id iv_source_sha = lv_committed_sha ).
        rs_result-stage = 'METADATA_COMMIT'.
        CLEAR rs_result-baseline_removed.
        COMMIT WORK AND WAIT.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'DROP baseline commit could not be verified' ).
        ENDIF.
        rs_result-stage = 'METADATA_VERIFY'.
        ls_base = mo_sync_store->load( iv_data_client = CONV #( lv_client ) is_key = ls_key ).
        IF ls_base-found = abap_true.
          zcx_abapgit_exception=>raise( 'DROP baseline removal could not be verified' ).
        ENDIF.
        rs_result-baseline_removed = NEW #( abap_true ).
        rs_result-stage = 'COMPLETE'.
        rs_result-success = abap_true.
      CATCH cx_root INTO DATA(lx_drop).
        IF lv_locked = abap_true AND rs_result-baseline_removed IS BOUND.
          ROLLBACK WORK.
        ENDIF.
        rs_result-error = lx_drop->get_text( ).
    ENDTRY.
    IF lv_locked = abap_true.
      unlock( is_header = ls_header iv_data_client = lv_client ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_so10_sync_api~adopt.
    rs_result-saved = NEW #( abap_false ).
    rs_result-baseline_recorded = NEW #( abap_false ).
    rs_result-stage = 'VALIDATION'.
    DATA lv_locked TYPE abap_bool.
    DATA lv_commit_started TYPE abap_bool.
    DATA lv_client TYPE sy-mandt.
    DATA ls_header TYPE thead.
    TRY.
        FIND PCRE `\A[a-z0-9][a-z0-9._-]{0,79}\z` IN iv_repo_id.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'Invalid SO10 repository identity' ).
        ENDIF.
        FIND PCRE `\A[0-9a-f]{64}\z` IN iv_source_sha.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'Invalid SO10 source fingerprint' ).
        ENDIF.
        lv_client = zcl_abgagt_so10_catalog=>data_client( iv_data_client ).
        ls_header = make_header( is_key ).
        authorize( is_header = ls_header iv_activity = 'SHOW' ).
        DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = ls_header-tdobject
          name = ls_header-tdname id = ls_header-tdid language = ls_header-tdspras ).
        rs_result-stage = 'LOCK'.
        CALL FUNCTION 'ENQUEUE_ESSSTXT'
          EXPORTING mode_stxh = 'E' mandt = lv_client tdobject = ls_header-tdobject
                    tdname = ls_header-tdname tdid = ls_header-tdid tdspras = ls_header-tdspras _scope = '1'
          EXCEPTIONS foreign_lock = 1 system_failure = 2 OTHERS = 3.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'SO10 text is locked by another session' ).
        ENDIF.
        lv_locked = abap_true.
        rs_result-stage = 'SOURCE_CHECK'.
        DATA(ls_base) = mo_sync_store->load( iv_data_client = CONV #( lv_client ) is_key = ls_key ).
        IF ls_base-found = abap_true AND ls_base-repo_id <> iv_repo_id.
          rs_result-conflict_kind = 'OWNERSHIP'.
          zcx_abapgit_exception=>raise( 'SO10 text is owned by another repository' ).
        ENDIF.
        rs_result-text = read_native( is_header = ls_header iv_data_client = lv_client ).
        rs_result-text-data_client = lv_client.
        IF rs_result-text-found = abap_false.
          zcx_abapgit_exception=>raise( 'SO10 text no longer exists; repeat explicit IMPORT' ).
        ENDIF.
        rs_result-text-source_sha = mo_sync_store->fingerprint( rs_result-text-lines ).
        IF rs_result-text-source_sha <> iv_source_sha.
          rs_result-conflict_kind = 'SOURCE_CHANGED'.
          zcx_abapgit_exception=>raise( 'SO10 source changed after file preparation; repeat explicit IMPORT' ).
        ENDIF.
        rs_result-stage = 'METADATA_RECORD'.
        mo_sync_store->record( iv_data_client = CONV #( lv_client ) is_key = ls_key
          iv_repo_id = iv_repo_id iv_source_sha = iv_source_sha is_header = rs_result-text-header ).
        rs_result-stage = 'METADATA_COMMIT'.
        CLEAR rs_result-baseline_recorded.
        lv_commit_started = abap_true.
        COMMIT WORK AND WAIT.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'SO10 baseline commit outcome is unknown' ).
        ENDIF.
        rs_result-stage = 'METADATA_VERIFY'.
        ls_base = mo_sync_store->load( iv_data_client = CONV #( lv_client ) is_key = ls_key ).
        IF ls_base-found = abap_false OR ls_base-repo_id <> iv_repo_id OR ls_base-source_sha <> iv_source_sha.
          zcx_abapgit_exception=>raise( 'SO10 baseline could not be verified; repeat explicit IMPORT' ).
        ENDIF.
        rs_result-baseline_recorded = NEW #( abap_true ).
        rs_result-text-base_found = abap_true.
        rs_result-text-base_repo_id = ls_base-repo_id.
        rs_result-text-base_sha = ls_base-source_sha.
        rs_result-stage = 'COMPLETE'.
        rs_result-success = abap_true.
      CATCH cx_root INTO DATA(lx_error).
        IF lv_commit_started = abap_false AND lv_locked = abap_true.
          ROLLBACK WORK.
        ENDIF.
        rs_result-error = lx_error->get_text( ).
    ENDTRY.
    IF lv_locked = abap_true.
      unlock( is_header = ls_header iv_data_client = lv_client ).
    ENDIF.
  ENDMETHOD.

  METHOD check_client_changeable.
    SELECT SINGLE mandt, cccoractiv FROM t000 WHERE mandt = @iv_data_client INTO @DATA(ls_client).
    IF sy-subrc <> 0 OR ls_client-mandt IS INITIAL.
      zcx_abapgit_exception=>raise( 'Target data client is not configured' ).
    ENDIF.
    DATA(lv_client_edit) = ls_client-cccoractiv.
    IF iv_data_client = sy-mandt.
      CALL FUNCTION 'TR_SYS_PARAMS'
        IMPORTING system_client_edit = lv_client_edit
        EXCEPTIONS OTHERS = 1.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Client-change policy could not be determined' ).
      ENDIF.
    ENDIF.
    IF iv_transport = abap_true AND lv_client_edit = '3'.
      zcx_abapgit_exception=>raise( 'The authenticated client does not permit transport recording' ).
    ENDIF.
    CASE lv_client_edit.
      WHEN '2'.
        zcx_abapgit_exception=>raise( 'The authenticated client does not permit changes' ).
      WHEN space OR '1' OR '3'.
        " SO10 transport recording remains a separate operation, as in RSTXTRAN.
        RETURN.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'Unknown client-change policy; refusing to save' ).
    ENDCASE.
  ENDMETHOD.

  METHOD unlock.
    CALL FUNCTION 'DEQUEUE_ESSSTXT'
      EXPORTING mode_stxh = 'E' mandt = iv_data_client tdobject = is_header-tdobject
                tdname = is_header-tdname tdid = is_header-tdid tdspras = is_header-tdspras
                _scope = '1'.
  ENDMETHOD.

  METHOD resolve_request.
    FIND PCRE `\A[A-Z0-9]{3}K[0-9]{6}\z` IN iv_request.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Invalid customizing request/task number' ).
    ENDIF.
    SELECT SINGLE trkorr, trfunction, trstatus, strkorr, as4user FROM e070
      WHERE trkorr = @iv_request INTO @DATA(ls_selected).
    IF sy-subrc <> 0 OR ls_selected-trstatus <> 'D'.
      zcx_abapgit_exception=>raise( 'Transport request/task does not exist or is not modifiable' ).
    ENDIF.
    DATA(lv_request) = CONV e070-trkorr( '' ).
    DATA(lv_task) = CONV e070-trkorr( '' ).
    CASE ls_selected-trfunction.
      WHEN 'Q'.
        IF ls_selected-as4user <> sy-uname OR ls_selected-strkorr IS INITIAL.
          zcx_abapgit_exception=>raise( 'Customizing task must be owned by the current user and have a parent request' ).
        ENDIF.
        lv_task = ls_selected-trkorr.
        lv_request = ls_selected-strkorr.
      WHEN 'W'.
        lv_request = ls_selected-trkorr.
        SELECT trkorr FROM e070 WHERE strkorr = @lv_request AND trfunction = 'Q'
          AND trstatus = 'D' AND as4user = @sy-uname ORDER BY trkorr
          INTO TABLE @DATA(lt_tasks) UP TO 2 ROWS.
        IF lines( lt_tasks ) <> 1.
          zcx_abapgit_exception=>raise( 'Exactly one owned modifiable customizing task is required; specify the task when ambiguous' ).
        ENDIF.
        lv_task = lt_tasks[ 1 ]-trkorr.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( 'SO10 requires a customizing request or task; workbench transports are not accepted' ).
    ENDCASE.
    SELECT SINGLE trfunction, trstatus FROM e070 WHERE trkorr = @lv_request INTO @DATA(ls_parent).
    IF sy-subrc <> 0 OR ls_parent-trfunction <> 'W' OR ls_parent-trstatus <> 'D'.
      zcx_abapgit_exception=>raise( 'Parent customizing request must exist and be modifiable' ).
    ENDIF.
    SELECT SINGLE client FROM e070c WHERE trkorr = @lv_request INTO @DATA(lv_request_client).
    IF sy-subrc <> 0 OR lv_request_client <> sy-mandt.
      zcx_abapgit_exception=>raise( 'Customizing request source client must match the authenticated text client' ).
    ENDIF.
    rs_transport-request = lv_request.
    rs_transport-task = lv_task.
  ENDMETHOD.

  METHOD zif_abgagt_so10_api~record_transport.
    DATA(ls_header) = make_header( is_key ).
    check_client_changeable( iv_transport = abap_true ).
    authorize( is_header = ls_header iv_activity = 'SHOW' ).
    rs_transport = resolve_request( iv_request ).
    DATA(lv_key) = |{ ls_header-tdobject },{ ls_header-tdname },{ ls_header-tdid },{ ls_header-tdspras }|.
    DATA lv_native_key TYPE e071-obj_name.
    DATA lv_key_limit TYPE i.
    DESCRIBE FIELD lv_native_key LENGTH lv_key_limit IN CHARACTER MODE.
    IF strlen( lv_key ) > lv_key_limit.
      zcx_abapgit_exception=>raise( 'SO10 transport key exceeds the native CTS field width' ).
    ENDIF.
    lv_native_key = lv_key.
    CALL FUNCTION 'ENQUEUE_ESSSTXT'
      EXPORTING mode_stxh = 'E' mandt = sy-mandt tdobject = ls_header-tdobject tdname = ls_header-tdname
        tdid = ls_header-tdid tdspras = ls_header-tdspras _scope = '1' _wait = abap_false
      EXCEPTIONS foreign_lock = 1 system_failure = 2 OTHERS = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'SO10 text could not be locked for transport recording' ).
    ENDIF.
    DATA(lv_committed) = abap_false.
    TRY.
        DATA(ls_text) = read_native( ls_header ).
        IF ls_text-found = abap_false.
          zcx_abapgit_exception=>raise( 'SO10 text does not exist; no transport entry was recorded' ).
        ENDIF.
        DATA lt_objects TYPE STANDARD TABLE OF e071 WITH EMPTY KEY.
        DATA lt_keys TYPE STANDARD TABLE OF e071k WITH EMPTY KEY.
        lt_objects = VALUE #( ( trkorr = CONV #( rs_transport-task ) pgmid = 'R3TR'
          object = 'TEXT' obj_name = lv_native_key ) ).
        CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
          EXPORTING wi_trkorr = CONV e070-trkorr( rs_transport-task ) iv_dialog = abap_false
          TABLES wt_e071 = lt_objects wt_e071k = lt_keys
          EXCEPTIONS OTHERS = 1.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'Native CTS text recording failed; ownership, request locks and client checks were not bypassed' ).
        ENDIF.
        lv_committed = abap_true.
        COMMIT WORK AND WAIT.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'Transport commit could not be confirmed' ).
        ENDIF.
        SELECT SINGLE trkorr FROM e071 WHERE trkorr = @rs_transport-task AND pgmid = 'R3TR'
          AND object = 'TEXT' AND obj_name = @lv_native_key INTO @DATA(lv_recorded_task).
        IF sy-subrc <> 0 OR lv_recorded_task IS INITIAL.
          zcx_abapgit_exception=>raise( 'Transport entry could not be verified after recording' ).
        ENDIF.
        rs_transport-object_key = lv_key.
        rs_transport-language = ls_header-tdspras.
        unlock( ls_header ).
      CATCH cx_root INTO DATA(lx_error).
        IF lv_committed = abap_false.
          ROLLBACK WORK.
        ENDIF.
        unlock( ls_header ).
        zcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_abgagt_so10_api~save.
    DATA(ls_result) = save_text( is_key = is_key it_lines = it_lines iv_action = iv_action iv_overwrite = iv_overwrite ).
    IF ls_result-success = abap_false.
      zcx_abapgit_exception=>raise( ls_result-error ).
    ENDIF.
    rs_text = ls_result-text.
  ENDMETHOD.

  METHOD zif_abgagt_so10_api~copy.
    rs_result-saved = NEW #( abap_false ).
    rs_result-baseline_recorded = NEW #( abap_false ).
    rs_result-stage = 'SOURCE_CHECK'.
    TRY.
        DATA(lv_source) = zcl_abgagt_so10_catalog=>data_client( iv_source_client ).
        DATA(lv_target) = zcl_abgagt_so10_catalog=>data_client( iv_target_client ).
        IF lv_source = lv_target.
          zcx_abapgit_exception=>raise( 'COPY requires distinct source and target data clients' ).
        ENDIF.
        DATA(ls_header) = make_header( is_key ).
        authorize( is_header = ls_header iv_activity = 'SHOW' ).
        DATA(ls_source) = read_native( is_header = ls_header iv_data_client = lv_source ).
        IF ls_source-found = abap_false.
          zcx_abapgit_exception=>raise( 'Source SO10 text not found' ).
        ENDIF.
        rs_result = save_text( is_key = is_key it_lines = ls_source-lines iv_action = 'COPY'
          iv_overwrite = iv_overwrite iv_data_client = lv_target ).
        rs_result-source_client = lv_source.
      CATCH cx_root INTO DATA(lx_error).
        rs_result-error = lx_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_abgagt_so10_sync_api~write.
    rs_result = save_text( is_key = is_key it_lines = it_lines iv_action = iv_action iv_sync = abap_true
      iv_repo_id = iv_repo_id iv_managed = iv_managed iv_ignore = iv_ignore ).
  ENDMETHOD.

  METHOD complete_baseline.
    DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = cs_result-text-header-tdobject
      name = cs_result-text-header-tdname id = cs_result-text-header-tdid language = cs_result-text-header-tdspras ).
    cs_result-stage = 'METADATA_RECORD'.
    mo_sync_store->record( iv_data_client = CONV #( sy-mandt ) is_key = ls_key iv_repo_id = iv_repo_id
      iv_source_sha = iv_source_sha is_header = cs_result-text-header ).
    cs_result-stage = 'METADATA_COMMIT'.
    CLEAR cs_result-baseline_recorded.
    COMMIT WORK AND WAIT.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'SO10 baseline commit outcome is unknown' ).
    ENDIF.
    cs_result-stage = 'METADATA_VERIFY'.
    DATA(ls_base) = mo_sync_store->load( iv_data_client = CONV #( sy-mandt ) is_key = ls_key ).
    IF ls_base-found = abap_false OR ls_base-repo_id <> iv_repo_id OR ls_base-source_sha <> iv_source_sha.
      zcx_abapgit_exception=>raise( 'SO10 baseline could not be verified; recover through explicit IMPORT' ).
    ENDIF.
    cs_result-baseline_recorded = NEW #( abap_true ).
    cs_result-text-base_found = abap_true.
    cs_result-text-base_repo_id = ls_base-repo_id.
    cs_result-text-base_sha = ls_base-source_sha.
  ENDMETHOD.

  METHOD validate_write_lines.
    IF it_lines IS INITIAL.
      zcx_abapgit_exception=>raise( 'Empty text could delete a standard text' ).
    ENDIF.
    DATA(lv_has_content) = abap_false.
    LOOP AT it_lines INTO DATA(ls_line).
      FIND PCRE `[\r\n\x00]` IN ls_line-tdline.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( 'Native ITF lines cannot contain embedded delimiters' ).
      ENDIF.
      FIND PCRE `[\r\n\x00]` IN ls_line-tdformat.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( 'ITF format cannot contain embedded delimiters' ).
      ENDIF.
      FIND PCRE `\S` IN ls_line-tdline.
      IF sy-subrc = 0.
        lv_has_content = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_has_content = abap_false.
      zcx_abapgit_exception=>raise( 'Empty text could delete a standard text' ).
    ENDIF.
  ENDMETHOD.

  METHOD save_text.
    rs_result-saved = NEW #( abap_false ).
    rs_result-baseline_recorded = NEW #( abap_false ).
    rs_result-stage = 'VALIDATION'.
    DATA lv_locked TYPE abap_bool.
    DATA ls_header TYPE thead.
    TRY.
        IF iv_action <> 'CREATE' AND iv_action <> 'UPDATE' AND iv_action <> 'COPY'.
          zcx_abapgit_exception=>raise( 'Unsupported SO10 save action' ).
        ENDIF.
        IF ( iv_action <> 'COPY' AND iv_overwrite <> abap_false ) OR ( iv_sync = abap_true AND iv_action = 'COPY' ).
          zcx_abapgit_exception=>raise( 'Overwrite is only valid for COPY; COPY is independent of Git synchronization' ).
        ENDIF.
        validate_write_lines( it_lines ).
        DATA lv_desired_sha TYPE string.
        IF iv_sync = abap_true.
          lv_desired_sha = mo_sync_store->fingerprint( it_lines ).
        ENDIF.
        ls_header = make_header( is_key ).
        ls_header-mandt = iv_data_client.
        check_client_changeable( iv_data_client = iv_data_client ).
        IF iv_action <> 'COPY' OR iv_overwrite = abap_false.
          authorize( is_header = ls_header iv_activity = COND #( WHEN iv_action = 'UPDATE' THEN 'EDIT' ELSE 'CREA' ) ).
        ENDIF.
        rs_result-stage = 'LOCK'.
        CALL FUNCTION 'ENQUEUE_ESSSTXT'
          EXPORTING mode_stxh = 'E' mandt = iv_data_client tdobject = ls_header-tdobject
                    tdname = ls_header-tdname tdid = ls_header-tdid tdspras = ls_header-tdspras
                    _scope = '1' _wait = abap_false
          EXCEPTIONS foreign_lock = 1 system_failure = 2 OTHERS = 3.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'SO10 text could not be locked; no save was attempted' ).
        ENDIF.
        lv_locked = abap_true.
        rs_result-stage = 'SOURCE_CHECK'.
        DATA(ls_existing) = read_native( is_header = ls_header iv_data_client = iv_data_client ).
        DATA(lv_write) = abap_true.
        IF iv_sync = abap_true.
          DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = ls_header-tdobject
            name = ls_header-tdname id = ls_header-tdid language = ls_header-tdspras ).
          DATA ls_base TYPE zcl_abgagt_so10_sync_rules=>ty_baseline.
          IF iv_managed = abap_true.
            ls_base = mo_sync_store->load( iv_data_client = CONV #( sy-mandt ) is_key = ls_key ).
          ENDIF.
          DATA(lv_current_sha) = COND string( WHEN ls_existing-found = abap_true
            THEN mo_sync_store->fingerprint( ls_existing-lines ) ELSE `` ).
          lv_write = zcl_abgagt_so10_sync_rules=>check_write( iv_action = iv_action iv_exists = ls_existing-found
            iv_repo_id = iv_repo_id iv_current_sha = lv_current_sha iv_desired_sha = lv_desired_sha
            is_baseline = ls_base iv_managed = iv_managed iv_ignore = iv_ignore ).
        ENDIF.
        IF iv_action = 'COPY' AND iv_overwrite = abap_true.
          authorize( is_header = ls_header iv_activity = COND #( WHEN ls_existing-found = abap_true THEN 'EDIT' ELSE 'CREA' ) ).
        ENDIF.
        IF ls_existing-found = abap_true.
          IF iv_action = 'CREATE'.
            zcx_abapgit_exception=>raise( 'SO10 text already exists; CREATE does not overwrite it' ).
          ELSEIF iv_action = 'COPY' AND iv_overwrite = abap_false.
            zcx_abapgit_exception=>raise( 'Target SO10 text already exists; use --overwrite to replace it' ).
          ENDIF.
        ENDIF.
        IF iv_action = 'UPDATE' AND ls_existing-found = abap_false.
          zcx_abapgit_exception=>raise( 'SO10 text does not exist; UPDATE does not create it' ).
        ENDIF.
        IF ls_existing-found = abap_true.
          ls_header = ls_existing-header.
        ENDIF.
        rs_result-text = ls_existing.
        IF lv_write = abap_true.
        rs_result-stage = 'TEXT_SAVE'.
        DATA(lt_write_lines) = it_lines.
        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING client = iv_data_client header = ls_header savemode_direct = abap_true
                    insert = xsdbool( ls_existing-found = abap_false )
          TABLES lines = lt_write_lines
          EXCEPTIONS id = 1 language = 2 name = 3 object = 4 OTHERS = 5.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'Native SO10 save failed' ).
        ENDIF.
        IF iv_data_client = sy-mandt.
          CALL FUNCTION 'COMMIT_TEXT'
            EXPORTING object = ls_header-tdobject name = ls_header-tdname
                      id = ls_header-tdid language = ls_header-tdspras
            EXCEPTIONS OTHERS = 1.
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( 'Native SO10 text flush failed' ).
          ENDIF.
        ENDIF.
        rs_result-stage = 'TEXT_COMMIT'.
        CLEAR rs_result-saved.
        COMMIT WORK AND WAIT.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( 'SO10 commit failed; save outcome is unknown' ).
        ENDIF.
        rs_result-stage = 'TEXT_VERIFY'.
        rs_result-text = read_native( is_header = ls_header iv_data_client = iv_data_client ).
        IF rs_result-text-found = abap_false OR rs_result-text-lines <> it_lines.
          zcx_abapgit_exception=>raise( 'Committed SO10 text could not be verified; read back before retrying' ).
        ENDIF.
        rs_result-saved = NEW #( abap_true ).
        ENDIF.
        rs_result-text-data_client = iv_data_client.
        IF iv_sync = abap_true.
          rs_result-text-source_sha = lv_desired_sha.
          IF iv_managed = abap_true.
            complete_baseline( EXPORTING iv_repo_id = iv_repo_id iv_source_sha = lv_desired_sha CHANGING cs_result = rs_result ).
          ENDIF.
        ENDIF.
        rs_result-stage = 'COMPLETE'.
        rs_result-success = abap_true.
      CATCH cx_root INTO DATA(lx_error).
        IF lv_locked = abap_true AND rs_result-baseline_recorded IS BOUND.
          ROLLBACK WORK.
        ENDIF.
        rs_result-error = lx_error->get_text( ).
        rs_result-conflict_kind = COND #( WHEN rs_result-error CS 'BASELINE_MISSING:' THEN 'BASELINE_MISSING'
          WHEN rs_result-error CS 'LOCAL_EDIT:' THEN 'LOCAL_EDIT' WHEN rs_result-error CS 'SYSTEM_EDIT:' THEN 'SYSTEM_EDIT'
          WHEN rs_result-error CS 'another repository' THEN 'OWNERSHIP' ELSE `` ).
    ENDTRY.
    IF lv_locked = abap_true.
      unlock( is_header = ls_header iv_data_client = iv_data_client ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
