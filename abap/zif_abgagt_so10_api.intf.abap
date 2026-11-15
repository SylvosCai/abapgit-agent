INTERFACE zif_abgagt_so10_api PUBLIC.
  TYPES:
    BEGIN OF ty_key,
      object   TYPE string,
      name     TYPE string,
      id       TYPE string,
      language TYPE string,
    END OF ty_key,
    ty_native_lines TYPE STANDARD TABLE OF tline WITH EMPTY KEY,
    BEGIN OF ty_text,
      found  TYPE abap_bool,
      header TYPE thead,
      lines  TYPE ty_native_lines,
      data_client TYPE string,
      source_sha TYPE string,
      base_found TYPE abap_bool,
      base_repo_id TYPE string,
      base_sha TYPE string,
    END OF ty_text,
    ty_keys TYPE STANDARD TABLE OF ty_key WITH EMPTY KEY,
    BEGIN OF ty_catalog,
      keys TYPE ty_keys,
      max_hits TYPE i,
      at_least TYPE i,
      has_more TYPE abap_bool,
    END OF ty_catalog,
    BEGIN OF ty_write,
      success TYPE abap_bool,
      error TYPE string,
      stage TYPE string,
      conflict_kind TYPE string,
      source_client TYPE string,
      saved TYPE REF TO abap_bool,
      baseline_recorded TYPE REF TO abap_bool,
      deleted TYPE REF TO abap_bool,
      baseline_removed TYPE REF TO abap_bool,
      text TYPE ty_text,
    END OF ty_write,
    BEGIN OF ty_transport,
      request TYPE string,
      task TYPE string,
      object_key TYPE string,
      language TYPE string,
    END OF ty_transport.

  METHODS read
    IMPORTING is_key TYPE ty_key iv_data_client TYPE string OPTIONAL
    RETURNING VALUE(rs_text) TYPE ty_text
    RAISING zcx_abapgit_exception.

  METHODS save
    IMPORTING is_key TYPE ty_key
              it_lines TYPE ty_native_lines
              iv_action TYPE string
              iv_overwrite TYPE abap_bool DEFAULT abap_false
    RETURNING VALUE(rs_text) TYPE ty_text
    RAISING zcx_abapgit_exception.
  METHODS copy DEFAULT FAIL
    IMPORTING is_key TYPE ty_key iv_target_client TYPE string
              iv_source_client TYPE string OPTIONAL iv_overwrite TYPE abap_bool DEFAULT abap_false
    RETURNING VALUE(rs_result) TYPE ty_write.

  METHODS record_transport
    IMPORTING is_key TYPE ty_key
              iv_request TYPE string
    RETURNING VALUE(rs_transport) TYPE ty_transport
    RAISING zcx_abapgit_exception.
ENDINTERFACE.
