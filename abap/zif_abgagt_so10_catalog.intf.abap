INTERFACE zif_abgagt_so10_catalog PUBLIC.
  METHODS search
    IMPORTING iv_data_client TYPE string
              it_prefixes TYPE string_table
              iv_name TYPE string OPTIONAL
              iv_id TYPE string OPTIONAL
              iv_language TYPE string OPTIONAL
              iv_max_hits TYPE i DEFAULT 100
    RETURNING VALUE(rs_result) TYPE zif_abgagt_so10_api=>ty_catalog
    RAISING zcx_abapgit_exception.
ENDINTERFACE.
