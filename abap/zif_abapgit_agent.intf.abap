INTERFACE zif_abapgit_agent PUBLIC.

  TYPES: BEGIN OF ty_result.
    TYPES: success TYPE abap_bool.
    TYPES: job_id TYPE string.
    TYPES: message TYPE string.
    TYPES: error_detail TYPE string.
    TYPES: activated_count TYPE i.
    TYPES: failed_count TYPE i.
    TYPES: started_at TYPE timestampl.
    TYPES: finished_at TYPE timestampl.
  TYPES: END OF ty_result.

  TYPES: BEGIN OF ty_pull_params.
    TYPES: url TYPE string.
    TYPES: branch TYPE string DEFAULT 'main'.
    TYPES: username TYPE string.
    TYPES: password TYPE string.
    TYPES: package TYPE devclass.
    TYPES: folder_logic TYPE string DEFAULT 'PREFIX'.
    TYPES: create_new TYPE abap_bool DEFAULT abap_false.
  TYPES: END OF ty_pull_params.

  TYPES: ty_log_table TYPE TABLE OF string.
  TYPES: ty_job_status TYPE string.

  CONSTANTS:
    gc_status_pending TYPE ty_job_status VALUE 'PENDING',
    gc_status_running TYPE ty_job_status VALUE 'RUNNING',
    gc_status_completed TYPE ty_job_status VALUE 'COMPLETED',
    gc_status_failed TYPE ty_job_status VALUE 'PENDING'.

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

ENDINTERFACE.
