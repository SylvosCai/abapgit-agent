CLASS zcl_abgagt_so10_sync_rules DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_baseline,
             found TYPE abap_bool,
             repo_id TYPE string,
             source_sha TYPE string,
             source_user TYPE string,
             source_date TYPE string,
             source_time TYPE string,
           END OF ty_baseline.
    CLASS-METHODS fingerprint
      IMPORTING it_lines TYPE zif_abgagt_so10_api=>ty_native_lines
      RETURNING VALUE(rv_sha) TYPE string RAISING zcx_abapgit_exception.
    CLASS-METHODS check_write
      IMPORTING iv_action TYPE string iv_exists TYPE abap_bool
                iv_repo_id TYPE string iv_current_sha TYPE string iv_desired_sha TYPE string
                is_baseline TYPE ty_baseline OPTIONAL
                iv_managed TYPE abap_bool DEFAULT abap_true
                iv_ignore TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_write) TYPE abap_bool RAISING zcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-METHODS validate_sha IMPORTING iv_sha TYPE string RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abgagt_so10_sync_rules IMPLEMENTATION.
  METHOD validate_sha.
    IF strlen( iv_sha ) <> 64 OR iv_sha CN '0123456789abcdef'.
      zcx_abapgit_exception=>raise( 'Invalid SO10 SHA-256 fingerprint; refusing to compare' ).
    ENDIF.
  ENDMETHOD.

  METHOD fingerprint.
    DATA(lv_source) = /ui2/cl_json=>serialize(
      data = it_lines compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
    TRY.
        cl_abap_message_digest=>calculate_hash_for_char(
          EXPORTING if_algorithm = 'SHA256' if_data = lv_source
          IMPORTING ef_hashstring = rv_sha ).
      CATCH cx_root.
        zcx_abapgit_exception=>raise( 'SO10 SHA-256 calculation failed; no fallback fingerprint is permitted' ).
    ENDTRY.
    rv_sha = to_lower( rv_sha ).
    validate_sha( rv_sha ).
  ENDMETHOD.

  METHOD check_write.
    IF iv_action <> 'CREATE' AND iv_action <> 'UPDATE'.
      zcx_abapgit_exception=>raise( 'Git SO10 conflict checks apply only to CREATE and UPDATE' ).
    ENDIF.
    validate_sha( iv_desired_sha ).
    IF iv_exists = abap_true.
      validate_sha( iv_current_sha ).
    ENDIF.
    IF iv_managed = abap_true.
      FIND PCRE `\A[a-z0-9][a-z0-9._-]{0,79}\z` IN iv_repo_id.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'A stable SO10 repository identity is required' ).
      ENDIF.
      IF is_baseline-found = abap_true.
        IF is_baseline-repo_id <> iv_repo_id.
          zcx_abapgit_exception=>raise( 'SO10 baseline belongs to another repository; conflict-ignore cannot transfer ownership' ).
        ENDIF.
        validate_sha( is_baseline-source_sha ).
      ENDIF.
    ENDIF.

    IF iv_action = 'CREATE' AND iv_exists = abap_true.
      zcx_abapgit_exception=>raise( 'SO10 text already exists; CREATE cannot become UPDATE' ).
    ENDIF.
    IF iv_action = 'UPDATE' AND iv_exists = abap_false.
      zcx_abapgit_exception=>raise( 'SO10 text is missing; UPDATE cannot become CREATE' ).
    ENDIF.
    IF iv_managed = abap_false AND iv_ignore = abap_false.
      zcx_abapgit_exception=>raise( 'Standalone SO10 writes require explicit conflict-ignore' ).
    ENDIF.
    IF iv_action = 'CREATE'.
      rv_write = abap_true.
      RETURN.
    ENDIF.
    IF iv_managed = abap_true AND is_baseline-found = abap_false AND iv_ignore = abap_false.
      zcx_abapgit_exception=>raise( 'BASELINE_MISSING: use explicit IMPORT/adoption before UPDATE' ).
    ENDIF.
    IF iv_current_sha = iv_desired_sha.
      rv_write = abap_false.
      RETURN.
    ENDIF.
    IF iv_ignore = abap_true OR iv_current_sha = is_baseline-source_sha.
      rv_write = abap_true.
      RETURN.
    ENDIF.
    IF iv_desired_sha = is_baseline-source_sha.
      zcx_abapgit_exception=>raise( 'LOCAL_EDIT: SAP text changed since its confirmed Git baseline' ).
    ENDIF.
    zcx_abapgit_exception=>raise( 'SYSTEM_EDIT: SAP and desired text both differ from the confirmed baseline' ).
  ENDMETHOD.
ENDCLASS.
