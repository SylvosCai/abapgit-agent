INTERFACE zif_abgagt_so10_sync_api PUBLIC.
  METHODS adopt
    IMPORTING is_key TYPE zif_abgagt_so10_api=>ty_key
              iv_repo_id TYPE string
              iv_source_sha TYPE string
              iv_data_client TYPE string OPTIONAL
    RETURNING VALUE(rs_result) TYPE zif_abgagt_so10_api=>ty_write.
  " Existing adoption-only implementations fail closed until write is implemented.
  METHODS write DEFAULT FAIL
    IMPORTING is_key TYPE zif_abgagt_so10_api=>ty_key
              it_lines TYPE zif_abgagt_so10_api=>ty_native_lines
              iv_action TYPE string
              iv_repo_id TYPE string OPTIONAL
              iv_managed TYPE abap_bool DEFAULT abap_true
              iv_ignore TYPE abap_bool DEFAULT abap_false
    RETURNING VALUE(rs_result) TYPE zif_abgagt_so10_api=>ty_write.
  METHODS drop DEFAULT FAIL
    IMPORTING is_key TYPE zif_abgagt_so10_api=>ty_key
              it_lines TYPE zif_abgagt_so10_api=>ty_native_lines
              iv_repo_id TYPE string iv_data_client TYPE string OPTIONAL
    RETURNING VALUE(rs_result) TYPE zif_abgagt_so10_api=>ty_write.
ENDINTERFACE.
