"&lt;function Z_ABAPGIT_AGENT_PULL
"&gt; Purpose: Trigger git pull and activation for a repository
"&gt; Created by: ABAP AI Bridge
"&gt; Date: 2026-02-05

FUNCTION z_abapgit_agent_pull.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_URL) TYPE STRING
*"    VALUE(IV_BRANCH) TYPE STRING DEFAULT 'main'
*"    VALUE(IV_ACTIVATE) TYPE ABAP_BOOL DEFAULT ABAP_TRUE
*"  EXPORTING
*"    EV_SUCCESS TYPE ABAP_BOOL
*"    EV_JOB_ID TYPE STRING
*"    EV_MESSAGE TYPE STRING
*"    EV_ACTIVATED_COUNT TYPE I
*"    EV_FAILED_COUNT TYPE I
*"  TABLES
*"    ET_ERROR_LOG TYPE STRING_TABLE
*"----------------------------------------------------------------------
  DATA: lv_job_id TYPE string.
  DATA: ls_result TYPE zif_abapgit_agent=>ty_result.

  CLEAR: et_error_log, ev_message, ev_activated_count, ev_failed_count.

  " Generate unique job ID
  lv_job_id = |{ sy-uname }_{ sy-datetime }_{ sy-uzeit }|.
  CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP DATA(lv_started_at) TIME ZONE sy-tzone.

  " Start background job for pull and activation
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname = |ABAPGIT_PULL_{ lv_job_id }|
    IMPORTING
      jobcount = DATA(lv_jobcount).

  " Submit background job
  SUBMIT z_abapgit_agent_pull_job VIA JOB lv_jobcount
    WITH pv_url = iv_url
    WITH pv_branch = iv_branch
    WITH pv_job_id = lv_job_id
    AND RETURN.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount  = lv_jobcount
      jobname   = |ABAPGIT_PULL_{ lv_job_id }|
      sdlstrtdt = sy-datum
      sdlstrttm = sy-uzeit.

  " Return immediately - caller should poll for results
  ev_success = abap_true.
  ev_job_id = lv_job_id.
  ev_message = |Pull job started. Poll /api/status/{ lv_job_id } for results.|.

ENDFUNCTION.
