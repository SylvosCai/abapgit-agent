INTERFACE zif_abgagt_so10_sync_store PUBLIC.
  METHODS fingerprint
    IMPORTING it_lines TYPE zif_abgagt_so10_api=>ty_native_lines
    RETURNING VALUE(rv_sha) TYPE string RAISING zcx_abapgit_exception.
  METHODS load
    IMPORTING iv_data_client TYPE string is_key TYPE zif_abgagt_so10_api=>ty_key
    RETURNING VALUE(rs_baseline) TYPE zcl_abgagt_so10_sync_rules=>ty_baseline
    RAISING zcx_abapgit_exception.
  METHODS record
    IMPORTING iv_data_client TYPE string is_key TYPE zif_abgagt_so10_api=>ty_key
              iv_repo_id TYPE string iv_source_sha TYPE string is_header TYPE thead OPTIONAL
    RAISING zcx_abapgit_exception.
  METHODS remove DEFAULT FAIL
    IMPORTING iv_data_client TYPE string is_key TYPE zif_abgagt_so10_api=>ty_key
              iv_repo_id TYPE string iv_source_sha TYPE string
    RAISING zcx_abapgit_exception.
ENDINTERFACE.
