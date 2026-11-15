CLASS zcl_abgagt_command_so10 DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    TYPES:
      BEGIN OF ty_line,
        tdformat TYPE string,
        tdline TYPE string,
      END OF ty_line,
      ty_lines TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY,
      BEGIN OF ty_params,
        action TYPE string,
        object TYPE string,
        name TYPE string,
        id TYPE string,
        language TYPE string,
        client TYPE string,
        source_client TYPE string,
        target_client TYPE string,
        prefixes TYPE string_table,
        max_hits TYPE i,
        repo_id TYPE string,
        source_sha TYPE string,
        conflict_mode TYPE string,
        overwrite TYPE abap_bool,
        transport TYPE string,
        lines TYPE ty_lines,
      END OF ty_params,
      BEGIN OF ty_result,
        success TYPE abap_bool,
        command TYPE string,
        action TYPE string,
        object TYPE string,
        name TYPE string,
        id TYPE string,
        language TYPE string,
        client TYPE string,
        source_client TYPE string,
        target_client TYPE string,
        data_client TYPE string,
        found TYPE abap_bool,
        source_sha TYPE string,
        base_found TYPE abap_bool,
        base_repo_id TYPE string,
        base_sha TYPE string,
        error_kind TYPE string,
        keys TYPE zif_abgagt_so10_api=>ty_keys,
        max_hits TYPE i,
        at_least TYPE i,
        has_more TYPE abap_bool,
        line_count TYPE i,
        saved TYPE REF TO abap_bool,
        baseline_recorded TYPE REF TO abap_bool,
        deleted TYPE REF TO abap_bool,
        baseline_removed TYPE REF TO abap_bool,
        stage TYPE string,
        conflict_kind TYPE string,
        transport_recorded TYPE REF TO abap_bool,
        transport_request TYPE string,
        transport_task TYPE string,
        object_key TYPE string,
        message TYPE string,
        error TYPE string,
        lines TYPE ty_lines,
      END OF ty_result.
    METHODS constructor IMPORTING io_api TYPE REF TO zif_abgagt_so10_api OPTIONAL
      io_catalog TYPE REF TO zif_abgagt_so10_catalog OPTIONAL
      io_sync_api TYPE REF TO zif_abgagt_so10_sync_api OPTIONAL.
  PRIVATE SECTION.
    DATA mo_api TYPE REF TO zif_abgagt_so10_api.
    DATA mo_catalog TYPE REF TO zif_abgagt_so10_catalog.
    DATA mo_sync_api TYPE REF TO zif_abgagt_so10_sync_api.
    METHODS drop_text IMPORTING is_params TYPE ty_params is_key TYPE zif_abgagt_so10_api=>ty_key
      RETURNING VALUE(rs_result) TYPE ty_result RAISING zcx_abapgit_exception.
    METHODS validate_copy_options IMPORTING is_params TYPE ty_params iv_action TYPE string
      RAISING zcx_abapgit_exception.
    METHODS copy_text IMPORTING is_params TYPE ty_params is_key TYPE zif_abgagt_so10_api=>ty_key
      RETURNING VALUE(rs_result) TYPE ty_result RAISING zcx_abapgit_exception.
    METHODS write_text IMPORTING is_params TYPE ty_params is_key TYPE zif_abgagt_so10_api=>ty_key
      RETURNING VALUE(rs_result) TYPE ty_result RAISING zcx_abapgit_exception.
    METHODS validate_sync_context IMPORTING is_params TYPE ty_params iv_action TYPE string
      RAISING zcx_abapgit_exception.
    METHODS adopt_text IMPORTING is_params TYPE ty_params is_key TYPE zif_abgagt_so10_api=>ty_key
      RETURNING VALUE(rs_result) TYPE ty_result RAISING zcx_abapgit_exception.
    METHODS list_texts IMPORTING is_params TYPE ty_params
      RETURNING VALUE(rs_catalog) TYPE zif_abgagt_so10_api=>ty_catalog RAISING zcx_abapgit_exception.
    METHODS validate_lines IMPORTING it_lines TYPE ty_lines RAISING zcx_abapgit_exception.
    METHODS validate_client_override IMPORTING iv_client TYPE string iv_action TYPE string
      RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abgagt_command_so10 IMPLEMENTATION.
  METHOD constructor.
    mo_api = COND #( WHEN io_api IS BOUND THEN io_api ELSE NEW zcl_abgagt_so10_api( ) ).
    mo_catalog = COND #( WHEN io_catalog IS BOUND THEN io_catalog ELSE NEW zcl_abgagt_so10_catalog( ) ).
    IF io_sync_api IS BOUND.
      mo_sync_api = io_sync_api.
    ELSEIF io_api IS NOT BOUND.
      mo_sync_api = CAST #( mo_api ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_so10.
  ENDMETHOD.

  METHOD validate_copy_options.
    IF iv_action <> 'COPY' AND ( is_params-source_client IS NOT INITIAL OR is_params-overwrite <> abap_false ).
      zcx_abapgit_exception=>raise( 'Source client and overwrite are only valid for COPY' ).
    ENDIF.
    IF is_params-target_client IS NOT INITIAL AND iv_action <> 'COPY' AND iv_action <> 'DROP'.
      zcx_abapgit_exception=>raise( 'Target data client is only valid for COPY and DROP' ).
    ENDIF.
    IF iv_action = 'DROP' AND is_params-target_client IS NOT INITIAL AND
      ( strlen( is_params-target_client ) <> 3 OR is_params-target_client CN '0123456789' ).
      zcx_abapgit_exception=>raise( 'Invalid target data client' ).
    ENDIF.
  ENDMETHOD.

  METHOD drop_text.
    IF mo_sync_api IS NOT BOUND OR is_params-repo_id IS INITIAL
      OR is_params-prefixes IS NOT INITIAL OR is_params-max_hits <> 0.
      zcx_abapgit_exception=>raise( 'DROP requires a managed repository, committed ITF and exact key' ).
    ENDIF.
    validate_lines( is_params-lines ).
    rs_result = VALUE #( command = 'SO10' action = 'DROP' object = is_key-object name = is_key-name
      id = is_key-id language = is_key-language client = sy-mandt saved = NEW #( abap_false )
      baseline_recorded = NEW #( abap_false ) transport_recorded = NEW #( abap_false ) stage = 'DROP' ).
    TRY.
        DATA(ls_drop) = mo_sync_api->drop( is_key = is_key it_lines = CORRESPONDING #( is_params-lines )
          iv_repo_id = is_params-repo_id iv_data_client = is_params-target_client ).
      CATCH cx_root INTO DATA(lx_drop).
        rs_result-error = |{ lx_drop->get_text( ) }. DROP outcomes may be unknown; inspect before retrying|.
        RETURN.
    ENDTRY.
    rs_result = CORRESPONDING #( ls_drop ).
    rs_result-command = 'SO10'.
    rs_result-action = 'DROP'.
    rs_result-object = is_key-object.
    rs_result-name = is_key-name.
    rs_result-id = is_key-id.
    rs_result-language = COND #( WHEN ls_drop-text-header-tdspras IS INITIAL THEN is_key-language ELSE ls_drop-text-header-tdspras ).
    rs_result-client = sy-mandt.
    rs_result-data_client = ls_drop-text-data_client.
    rs_result-target_client = ls_drop-text-data_client.
    rs_result-transport_recorded = NEW #( abap_false ).
    rs_result-message = 'SO10 text and owned baseline dropped; committed Git payload retained'.
  ENDMETHOD.

  METHOD copy_text.
    IF is_params-lines IS NOT INITIAL OR is_params-prefixes IS NOT INITIAL OR is_params-max_hits <> 0.
      zcx_abapgit_exception=>raise( 'COPY accepts only a SAP source/target key; no content or discovery payload' ).
    ENDIF.
    IF strlen( is_params-target_client ) <> 3 OR is_params-target_client CN '0123456789'
      OR ( is_params-source_client IS NOT INITIAL AND
        ( strlen( is_params-source_client ) <> 3 OR is_params-source_client CN '0123456789' ) ).
      zcx_abapgit_exception=>raise( 'COPY requires three-digit data clients' ).
    ENDIF.
    rs_result = CORRESPONDING #( is_params ).
    rs_result-action = 'COPY'.
    rs_result-command = 'SO10'.
    rs_result-client = sy-mandt.
    rs_result-saved = NEW #( abap_false ).
    rs_result-baseline_recorded = NEW #( abap_false ).
    rs_result-transport_recorded = NEW #( abap_false ).
    TRY.
        DATA(ls_copy) = mo_api->copy( is_key = is_key iv_source_client = is_params-source_client
          iv_target_client = is_params-target_client iv_overwrite = is_params-overwrite ).
        rs_result-success = ls_copy-success.
        rs_result-error = ls_copy-error.
        rs_result-stage = ls_copy-stage.
        rs_result-saved = ls_copy-saved.
        rs_result-source_client = ls_copy-source_client.
        rs_result-target_client = ls_copy-text-data_client.
        rs_result-data_client = ls_copy-text-data_client.
        rs_result-language = ls_copy-text-header-tdspras.
        rs_result-found = ls_copy-text-found.
        rs_result-lines = CORRESPONDING #( ls_copy-text-lines ).
        rs_result-line_count = lines( rs_result-lines ).
        rs_result-message = 'SO10 text copied between SAP data clients; no Git baseline or transport changed'.
      CATCH cx_root INTO DATA(lx_error).
        CLEAR rs_result-saved.
        rs_result-error = |{ lx_error->get_text( ) }. Copy outcome may be unknown; read back before retrying|.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_client_override.
    IF iv_client IS NOT INITIAL AND iv_action <> 'READ' AND iv_action <> 'LIST' AND iv_action <> 'ADOPT'.
      zcx_abapgit_exception=>raise( 'Client overrides are not permitted; use the authenticated connection' ).
    ENDIF.
    IF iv_client IS NOT INITIAL AND ( strlen( iv_client ) <> 3 OR iv_client CN '0123456789' ).
      zcx_abapgit_exception=>raise( 'SO10 data client must contain exactly three digits' ).
    ENDIF.
  ENDMETHOD.

  METHOD validate_sync_context.
    IF is_params-source_sha IS NOT INITIAL AND iv_action <> 'ADOPT'.
      zcx_abapgit_exception=>raise( 'Observed source fingerprints are only supported by explicit ADOPT' ).
    ENDIF.
    IF is_params-repo_id IS NOT INITIAL AND iv_action <> 'ADOPT' AND iv_action <> 'CREATE' AND iv_action <> 'UPDATE' AND iv_action <> 'DROP'.
      zcx_abapgit_exception=>raise( 'Repository context is only supported by ADOPT, CREATE and UPDATE' ).
    ENDIF.
    IF is_params-conflict_mode IS NOT INITIAL AND iv_action <> 'CREATE' AND iv_action <> 'UPDATE'.
      zcx_abapgit_exception=>raise( 'Conflict mode is only supported by CREATE and UPDATE' ).
    ENDIF.
    IF is_params-conflict_mode IS NOT INITIAL AND is_params-conflict_mode <> 'abort' AND is_params-conflict_mode <> 'ignore'.
      zcx_abapgit_exception=>raise( 'SO10 conflict mode must be abort or ignore' ).
    ENDIF.
  ENDMETHOD.

  METHOD write_text.
    IF is_params-repo_id IS INITIAL AND is_params-conflict_mode <> 'ignore'.
      zcx_abapgit_exception=>raise( 'Standalone SO10 writes require explicit conflict-ignore' ).
    ENDIF.
    validate_lines( is_params-lines ).
    IF mo_sync_api IS NOT BOUND OR is_params-prefixes IS NOT INITIAL OR is_params-max_hits <> 0.
      zcx_abapgit_exception=>raise( 'CREATE/UPDATE require the synchronized write API and one exact text; no discovery' ).
    ENDIF.
    rs_result = VALUE #( command = 'SO10' action = is_params-action object = is_key-object name = is_key-name
      id = is_key-id language = is_key-language client = sy-mandt transport_recorded = NEW #( abap_false ) stage = 'WRITE' ).
    TRY.
        DATA(ls_write) = mo_sync_api->write( is_key = is_key it_lines = CORRESPONDING #( is_params-lines )
          iv_action = is_params-action iv_repo_id = is_params-repo_id
          iv_managed = xsdbool( is_params-repo_id IS NOT INITIAL ) iv_ignore = xsdbool( is_params-conflict_mode = 'ignore' ) ).
      CATCH cx_root INTO DATA(lx_write).
        rs_result-error = |{ lx_write->get_text( ) }. Save outcome may be unknown; baseline outcome may be unknown; read back before retrying|.
        RETURN.
    ENDTRY.
    rs_result = CORRESPONDING #( ls_write ).
    rs_result-command = 'SO10'.
    rs_result-action = is_params-action.
    rs_result-object = is_key-object.
    rs_result-name = is_key-name.
    rs_result-id = is_key-id.
    rs_result-language = COND #( WHEN ls_write-text-header-tdspras IS INITIAL THEN is_key-language ELSE ls_write-text-header-tdspras ).
    rs_result-client = sy-mandt.
    rs_result-data_client = ls_write-text-data_client.
    rs_result-transport_recorded = NEW #( abap_false ).
    rs_result-source_sha = ls_write-text-source_sha.
    rs_result-base_found = ls_write-text-base_found.
    rs_result-base_repo_id = ls_write-text-base_repo_id.
    rs_result-base_sha = ls_write-text-base_sha.
    rs_result-lines = CORRESPONDING #( ls_write-text-lines ).
    rs_result-line_count = lines( ls_write-text-lines ).
    IF rs_result-success = abap_true.
      rs_result-message = COND #( WHEN ls_write-saved IS BOUND AND ls_write-saved->* = abap_true
        THEN 'SO10 text saved and verified; not recorded on a transport' ELSE 'SO10 desired text already matches; no text saved' ).
    ENDIF.
  ENDMETHOD.

  METHOD adopt_text.
    IF mo_sync_api IS NOT BOUND OR is_params-lines IS NOT INITIAL
      OR is_params-prefixes IS NOT INITIAL OR is_params-max_hits <> 0
      OR is_params-repo_id IS INITIAL OR is_params-source_sha IS INITIAL.
      zcx_abapgit_exception=>raise( 'ADOPT requires an exact key, repository and observed source fingerprint; no content or discovery' ).
    ENDIF.
    rs_result = VALUE #( command = 'SO10' action = 'ADOPT' object = is_key-object name = is_key-name
      id = is_key-id language = is_key-language client = sy-mandt saved = NEW #( abap_false )
      transport_recorded = NEW #( abap_false ) stage = 'ADOPT' ).
    TRY.
        DATA(ls_adoption) = mo_sync_api->adopt( is_key = is_key iv_repo_id = is_params-repo_id
          iv_source_sha = is_params-source_sha iv_data_client = is_params-client ).
      CATCH cx_root INTO DATA(lx_adoption).
        rs_result-error = lx_adoption->get_text( ).
        RETURN.
    ENDTRY.
    rs_result = CORRESPONDING #( ls_adoption ).
    rs_result-command = 'SO10'.
    rs_result-action = 'ADOPT'.
    rs_result-object = is_key-object.
    rs_result-name = is_key-name.
    rs_result-id = is_key-id.
    rs_result-language = COND #( WHEN ls_adoption-text-header-tdspras IS INITIAL THEN is_key-language
      ELSE ls_adoption-text-header-tdspras ).
    rs_result-client = sy-mandt.
    rs_result-data_client = ls_adoption-text-data_client.
    rs_result-transport_recorded = NEW #( abap_false ).
    rs_result-source_sha = ls_adoption-text-source_sha.
    rs_result-base_found = ls_adoption-text-base_found.
    rs_result-base_repo_id = ls_adoption-text-base_repo_id.
    rs_result-base_sha = ls_adoption-text-base_sha.
    IF rs_result-success = abap_true.
      rs_result-message = 'SO10 observed source baseline registered; no text saved'.
    ENDIF.
  ENDMETHOD.

  METHOD list_texts.
    IF is_params-prefixes IS INITIAL OR is_params-lines IS NOT INITIAL
      OR ( is_params-object IS NOT INITIAL AND to_upper( is_params-object ) <> 'TEXT' ).
      zcx_abapgit_exception=>raise( 'LIST requires explicit prefixes and TEXT headers only, without text content' ).
    ENDIF.
    DATA(lv_max) = COND i( WHEN is_params-max_hits = 0 THEN 100 ELSE is_params-max_hits ).
    IF lv_max < 1 OR lv_max > 1000.
      zcx_abapgit_exception=>raise( 'LIST max-hits must be between 1 and 1000' ).
    ENDIF.
    DATA(lv_language) = to_upper( is_params-language ).
    IF strlen( lv_language ) = 2.
      DATA lv_sap_language TYPE spras.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING input = lv_language IMPORTING output = lv_sap_language
        EXCEPTIONS unknown_language = 1 OTHERS = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Unknown ISO language' ).
      ENDIF.
      lv_language = lv_sap_language.
    ENDIF.
    rs_catalog = mo_catalog->search(
      iv_data_client = COND #( WHEN is_params-client IS INITIAL THEN CONV string( sy-mandt ) ELSE is_params-client )
      it_prefixes = is_params-prefixes iv_name = is_params-name iv_id = to_upper( is_params-id )
      iv_language = lv_language iv_max_hits = lv_max ).
  ENDMETHOD.

  METHOD validate_lines.
    IF it_lines IS INITIAL.
      zcx_abapgit_exception=>raise( 'Empty text is not supported; it could delete a text' ).
    ENDIF.
    DATA(lv_has_content) = abap_false.
    LOOP AT it_lines INTO DATA(ls_line).
      IF strlen( ls_line-tdformat ) > 2 OR strlen( ls_line-tdline ) > 132.
        zcx_abapgit_exception=>raise( 'Text line exceeds the native ITF field width' ).
      ENDIF.
      FIND PCRE `[\r\n\x00]` IN ls_line-tdline.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( 'Text lines cannot contain embedded delimiters' ).
      ENDIF.
      FIND PCRE `[\r\n\x00]` IN ls_line-tdformat.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( 'ITF format cannot contain embedded delimiters' ).
      ENDIF.
      DATA(lv_content) = condense( ls_line-tdline ).
      IF lv_content IS NOT INITIAL.
        lv_has_content = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_has_content = abap_false.
      zcx_abapgit_exception=>raise( 'Empty text is not supported; it could delete a text' ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA ls_params TYPE ty_params.
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.
    DATA(ls_result) = VALUE ty_result( command = 'SO10' client = sy-mandt source_client = ls_params-source_client
      action = to_upper( ls_params-action ) object = to_upper( ls_params-object )
      name = ls_params-name id = to_upper( ls_params-id ) language = to_upper( ls_params-language )
      saved = NEW abap_bool( abap_false ) baseline_recorded = NEW abap_bool( abap_false )
      transport_recorded = NEW abap_bool( abap_false ) ).
    IF ls_result-action = 'DROP'.
      ls_result-deleted = NEW #( abap_false ).
      ls_result-baseline_removed = NEW #( abap_false ).
    ENDIF.
    IF ls_result-object IS INITIAL.
      ls_result-object = 'TEXT'.
    ENDIF.
    IF ls_result-id IS INITIAL.
      ls_result-id = 'ST'.
    ENDIF.
    TRY.
        validate_client_override( iv_client = ls_params-client iv_action = ls_result-action ).
        IF ls_params-transport IS NOT INITIAL AND ls_result-action <> 'TRANSPORT'.
          zcx_abapgit_exception=>raise( 'Save and transport recording are separate SO10 operations' ).
        ENDIF.
        IF ls_result-action <> 'READ' AND ls_result-action <> 'CREATE' AND ls_result-action <> 'UPDATE'
          AND ls_result-action <> 'COPY' AND ls_result-action <> 'TRANSPORT'
          AND ls_result-action <> 'LIST' AND ls_result-action <> 'ADOPT' AND ls_result-action <> 'DROP'.
          zcx_abapgit_exception=>raise( 'Supported SO10 actions: READ, LIST, CREATE, UPDATE, COPY, DROP, TRANSPORT, ADOPT' ).
        ENDIF.
        validate_sync_context( is_params = ls_params iv_action = ls_result-action ).
        validate_copy_options( is_params = ls_params iv_action = ls_result-action ).
        IF ls_result-action = 'LIST'.
          ls_result-id = to_upper( ls_params-id ).
          DATA(ls_catalog) = list_texts( ls_params ).
          ls_result-max_hits = ls_catalog-max_hits.
          ls_result-at_least = ls_catalog-at_least.
          ls_result-has_more = ls_catalog-has_more.
          ls_result-data_client = COND #( WHEN ls_params-client IS INITIAL THEN CONV string( sy-mandt ) ELSE ls_params-client ).
          IF ls_catalog-has_more = abap_true.
            ls_result-error_kind = 'OVERFLOW'.
            zcx_abapgit_exception=>raise( 'SO10 discovery exceeds max-hits; narrow the prefixes or choose a finite higher limit' ).
          ENDIF.
          ls_result-keys = ls_catalog-keys.
          ls_result-message = 'Authorized SO10 headers retrieved; no text bodies read'.
          ls_result-success = abap_true.
          rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
          RETURN.
        ENDIF.
        IF ls_result-object <> 'TEXT' OR ls_result-name IS INITIAL OR strlen( ls_result-name ) > 70
          OR ls_result-id IS INITIAL OR strlen( ls_result-id ) > 4
          OR ls_result-language IS INITIAL OR strlen( ls_result-language ) > 2.
          zcx_abapgit_exception=>raise( 'Invalid SO10 text object, Text name, Text ID or language' ).
        ENDIF.
        FIND PCRE `[\r\n\x00*]` IN ls_result-name.
        IF sy-subrc = 0.
          zcx_abapgit_exception=>raise( 'Text name must address one exact text' ).
        ENDIF.
        IF ls_result-id CS '*' OR ls_result-language CS '*'.
          zcx_abapgit_exception=>raise( 'Text ID and language must address one exact key' ).
        ENDIF.
        DATA(ls_key) = VALUE zif_abgagt_so10_api=>ty_key( object = ls_result-object
          name = ls_result-name id = ls_result-id language = ls_result-language ).
        IF ls_result-action = 'DROP'.
          ls_result = drop_text( is_params = ls_params is_key = ls_key ).
          rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
          RETURN.
        ELSEIF ls_result-action = 'ADOPT'.
          ls_result = adopt_text( is_params = ls_params is_key = ls_key ).
          rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
          RETURN.
        ELSEIF ls_result-action = 'CREATE' OR ls_result-action = 'UPDATE'.
          ls_params-action = ls_result-action.
          ls_result = write_text( is_params = ls_params is_key = ls_key ).
          rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
          RETURN.
        ELSEIF ls_result-action = 'COPY'.
          ls_params-object = ls_result-object.
          ls_params-id = ls_result-id.
          ls_result = copy_text( is_params = ls_params is_key = ls_key ).
          rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
          RETURN.
        ENDIF.
        DATA ls_text TYPE zif_abgagt_so10_api=>ty_text.
        IF ls_result-action = 'READ'.
          IF ls_params-lines IS NOT INITIAL.
            zcx_abapgit_exception=>raise( 'READ does not accept text content' ).
          ENDIF.
          ls_text = mo_api->read( is_key = ls_key iv_data_client = ls_params-client ).
          ls_result-data_client = ls_text-data_client.
          ls_result-found = ls_text-found.
          ls_result-language = ls_text-header-tdspras.
          ls_result-source_sha = ls_text-source_sha.
          ls_result-base_found = ls_text-base_found.
          ls_result-base_repo_id = ls_text-base_repo_id.
          ls_result-base_sha = ls_text-base_sha.
          IF ls_text-found = abap_false.
            ls_result-error_kind = 'NOT_FOUND'.
            zcx_abapgit_exception=>raise( 'SO10 text not found' ).
          ENDIF.
          ls_result-message = 'SO10 text retrieved'.
        ELSEIF ls_result-action = 'TRANSPORT'.
          IF ls_params-lines IS NOT INITIAL.
            zcx_abapgit_exception=>raise( 'TRANSPORT does not accept text content' ).
          ENDIF.
          FIND PCRE `\A[A-Z0-9]{3}K[0-9]{6}\z` IN ls_params-transport.
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( 'An existing customizing request/task number is required' ).
          ENDIF.
          CLEAR ls_result-transport_recorded.
          DATA(ls_transport) = mo_api->record_transport( is_key = ls_key iv_request = ls_params-transport ).
          ls_result-transport_request = ls_transport-request.
          ls_result-transport_task = ls_transport-task.
          ls_result-object_key = ls_transport-object_key.
          ls_result-language = ls_transport-language.
          ls_result-transport_recorded = NEW abap_bool( abap_true ).
          ls_result-message = 'SO10 text key recorded on a customizing task; no text was saved'.
        ENDIF.
        IF ls_result-action <> 'TRANSPORT'.
          ls_result-language = ls_text-header-tdspras.
        ENDIF.
        ls_result-lines = CORRESPONDING #( ls_text-lines ).
        ls_result-line_count = lines( ls_text-lines ).
        ls_result-success = abap_true.
      CATCH cx_root INTO DATA(lx_error).
        ls_result-error = lx_error->get_text( ).
        IF ls_result-saved IS NOT BOUND.
          ls_result-error = |{ ls_result-error }. Save outcome may be unknown; read back before retrying|.
        ENDIF.
        IF ls_result-transport_recorded IS NOT BOUND.
          ls_result-error = |{ ls_result-error }. Recording outcome may be unknown; check the task before retrying|.
        ENDIF.
    ENDTRY.
    rv_result = /ui2/cl_json=>serialize( data = ls_result pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ENDMETHOD.
ENDCLASS.
