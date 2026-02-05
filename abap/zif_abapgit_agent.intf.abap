"&lt;interface ZIF_ABAPGIT_AGENT
"&gt; Purpose: Interface for ABAP Git Agent - programmatic git pull and activation
"&gt; Created by: ABAP AI Bridge
"&gt; Date: 2026-02-05

INTERFACE zif_abapgit_agent
  PUBLIC .

  " Result structure for pull/activate operations
  TYPES:
    BEGIN OF ty_result,
      success         TYPE abap_bool,
      job_id          TYPE string,
      message         TYPE string,
      error_log       TYPE string_table,
      activated_count TYPE i,
      failed_count    TYPE i,
      started_at     TYPE timestampl,
      finished_at    TYPE timestampl,
    END OF ty_result,

    " Repository status
    BEGIN OF ty_repo_status,
      url           TYPE string,
      branch        TYPE string,
      commit_sha    TYPE string,
      last_pull     TYPE timestampl,
      is_active     TYPE abap_bool,
      object_count  TYPE i,
    END OF ty_repo_status,

    " Object for activation
    BEGIN OF ty_object,
      object_type TYPE trobjtype,
      object_name TYPE sobj_name,
      devclass    TYPE devclass,
    END OF ty_object,

    ty_object_table TYPE TABLE OF ty_object,

    " Log entry structure
    BEGIN OF ty_log_entry,
      timestamp  TYPE timestampl,
      type        TYPE string,     " ERROR, WARNING, INFO, SUCCESS
      message     TYPE string,
      object_type TYPE trobjtype,
      object_name TYPE sobj_name,
      line        TYPE i,
      column      TYPE i,
    END OF ty_log_entry,

    ty_log_table TYPE TABLE OF ty_log_entry,

    " Job status
    ty_job_status TYPE string.

  " Constants
  CONSTANTS:
    gc_status_pending   TYPE ty_job_status VALUE 'PENDING',
    gc_status_running   TYPE ty_job_status VALUE 'RUNNING',
    gc_status_completed TYPE ty_job_status VALUE 'COMPLETED',
    gc_status_failed    TYPE ty_job_status VALUE 'FAILED'.

ENDINTERFACE.
