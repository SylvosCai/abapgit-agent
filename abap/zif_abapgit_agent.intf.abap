"!-------------------------------------------------------------------
"! Purpose: Interface for ABAP Git Agent - programmatic git pull and activation
"! Created by: ABAP AI Bridge
"! Date: 2026-02-05
"!-------------------------------------------------------------------

INTERFACE zif_abapgit_agent
  PUBLIC .

  " Result structure for pull/activate operations
  TYPES:
    BEGIN OF ty_result,
      success         TYPE abap_bool,
      job_id          TYPE string,
      message         TYPE string,
      error_detail    TYPE string,
      activated_count TYPE i,
      failed_count    TYPE i,
      started_at      TYPE timestampl,
      finished_at     TYPE timestampl,
    END OF ty_result,

    BEGIN OF ty_pull_params,
      url            TYPE string,
      branch         TYPE string DEFAULT 'main',
      username       TYPE string,
      password       TYPE string,
      package        TYPE devclass,
      folder_logic   TYPE string DEFAULT 'PREFIX',
      create_new     TYPE abap_bool DEFAULT abap_false,
    END OF ty_pull_params,

    ty_log_table TYPE TABLE OF string,

    " Job status
    ty_job_status TYPE string.

  " Constants
  CONSTANTS:
    gc_status_pending   TYPE ty_job_status VALUE 'PENDING',
    gc_status_running   TYPE ty_job_status VALUE 'RUNNING',
    gc_status_completed TYPE ty_job_status VALUE 'COMPLETED',
    gc_status_failed    TYPE ty_job_status VALUE 'FAILED'.

  " Pull repository and activate objects
  METHODS pull
    IMPORTING
      iv_url        TYPE string
      iv_branch     TYPE string DEFAULT 'main'
      iv_username   TYPE string OPTIONAL
      iv_password   TYPE string OPTIONAL
    RETURNING
      VALUE(rs_result) TYPE ty_result
    RAISING
      zcx_abapgit_exception.

  " Get repository status by URL
  METHODS get_repo_status
    IMPORTING
      iv_url        TYPE string
    RETURNING
      VALUE(rv_status) TYPE string.

ENDINTERFACE.
