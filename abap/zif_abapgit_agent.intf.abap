INTERFACE zif_abapgit_agent PUBLIC.

  TYPES: BEGIN OF ty_object,
    obj_type TYPE string,
    obj_name TYPE string,
    text TYPE string,
  END OF ty_object.

  TYPES: ty_object_list TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_result,
    success TYPE abap_bool,
    job_id TYPE string,
    message TYPE string,
    error_detail TYPE string,
    activated_count TYPE i,
    failed_count TYPE i,
    activated_objects TYPE ty_object_list,
    failed_objects TYPE ty_object_list,
    started_at TYPE timestampl,
    finished_at TYPE timestampl,
  END OF ty_result.

  TYPES: BEGIN OF ty_syntax_error,
    line TYPE string,
    column TYPE string,
    text TYPE string,
    word TYPE string,
  END OF ty_syntax_error.

  TYPES: ty_syntax_errors TYPE STANDARD TABLE OF ty_syntax_error WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_syntax_result,
    success TYPE abap_bool,
    object_type TYPE string,
    object_name TYPE string,
    error_count TYPE i,
    errors TYPE ty_syntax_errors,
  END OF ty_syntax_result.

  TYPES: BEGIN OF ty_pull_params,
    url TYPE string,
    branch TYPE string,
    username TYPE string,
    password TYPE string,
    package TYPE devclass,
    folder_logic TYPE string,
    create_new TYPE abap_bool,
  END OF ty_pull_params.

  TYPES: ty_log_table TYPE TABLE OF string.
  TYPES: ty_job_status TYPE string.

  CONSTANTS:
    gc_status_pending TYPE ty_job_status VALUE 'PENDING',
    gc_status_running TYPE ty_job_status VALUE 'RUNNING',
    gc_status_completed TYPE ty_job_status VALUE 'COMPLETED',
    gc_status_failed TYPE ty_job_status VALUE 'FAILED'.

  METHODS pull
    IMPORTING
      iv_url TYPE string
      iv_branch TYPE string DEFAULT 'main'
      iv_username TYPE string OPTIONAL
      iv_password TYPE string OPTIONAL
    RETURNING
      VALUE(rs_result) TYPE ty_result
    RAISING
      zcx_abapgit_exception.

  METHODS get_repo_status
    IMPORTING
      iv_url TYPE string
    RETURNING
      VALUE(rv_status) TYPE string.

  METHODS syntax_check
    IMPORTING
      iv_object_type TYPE string
      iv_object_name TYPE string
    RETURNING
      VALUE(rs_result) TYPE ty_syntax_result.

ENDINTERFACE.
