" Interface for abapGit Agent - handles pull operations only.
" Note: inspect and run_tests are implemented in separate command classes
" (zcl_abgagt_command_inspect, zcl_abgagt_command_unit) using ABAP standard classes.

INTERFACE zif_abgagt_agent PUBLIC.

  TYPES: BEGIN OF ty_object,
    type TYPE string,
    id TYPE string,
    number TYPE string,
    text TYPE string,
    obj_type TYPE string,
    obj_name TYPE string,
    exception TYPE string,
  END OF ty_object.

  TYPES: ty_object_list TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_result,
    success TYPE abap_bool,
    job_id TYPE string,
    message TYPE string,
    error_detail TYPE string,
    transport_request TYPE string,
    activated_count TYPE i,
    failed_count TYPE i,
    log_messages TYPE ty_object_list,
    activated_objects TYPE ty_object_list,
    failed_objects TYPE ty_object_list,
    started_at TYPE timestampl,
    finished_at TYPE timestampl,
  END OF ty_result.

  METHODS pull
    IMPORTING
      iv_url TYPE string
      iv_branch TYPE string DEFAULT 'main'
      iv_username TYPE string OPTIONAL
      iv_password TYPE string OPTIONAL
      iv_transport_request TYPE string OPTIONAL
      it_files TYPE string_table OPTIONAL
    RETURNING
      VALUE(rs_result) TYPE ty_result
    RAISING
      zcx_abapgit_exception.

  METHODS get_repo_status
    IMPORTING
      iv_url TYPE string
    RETURNING
      VALUE(rv_status) TYPE string.

ENDINTERFACE.
