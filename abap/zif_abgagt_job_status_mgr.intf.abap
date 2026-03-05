"! <p class="shorttext synchronized">Background Job Status Manager Interface</p>
INTERFACE zif_abgagt_job_status_mgr PUBLIC.

  TYPES:
    BEGIN OF ty_job_status,
      job_name      TYPE btcjob,
      job_number    TYPE btcjobcnt,
      status        TYPE string,
      stage         TYPE string,
      message       TYPE string,
      progress      TYPE i,
      current       TYPE i,
      total         TYPE i,
      result        TYPE string,
      error_message TYPE string,
      started_at    TYPE timestamp,
      updated_at    TYPE timestamp,
      completed_at  TYPE timestamp,
    END OF ty_job_status.

  "! Update job status in shared buffer
  "! @parameter is_status | Job status to save
  METHODS update_status
    IMPORTING
      is_status TYPE ty_job_status.

  "! Get job status from shared buffer
  "! @parameter iv_job_number | Job number
  "! @parameter rs_status | Job status (initial if not found)
  METHODS get_status
    IMPORTING
      iv_job_number    TYPE btcjobcnt
    RETURNING
      VALUE(rs_status) TYPE ty_job_status.

  "! Delete job status from shared buffer
  "! @parameter iv_job_number | Job number
  METHODS delete_status
    IMPORTING
      iv_job_number TYPE btcjobcnt.

ENDINTERFACE.
