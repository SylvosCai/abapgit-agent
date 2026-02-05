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

  CLEAR: et_error_log, ev_message, ev_activated_count, ev_failed_count.

  lv_job_id = |{ sy-uname }_{ sy-datetime }_{ sy-uzeit }|.

  DATA(lv_jobcount) = start_pull_job(
    iv_url    = iv_url
    iv_branch = iv_branch
    iv_job_id = lv_job_id
  ).

  ev_success = abap_true.
  ev_job_id = lv_job_id.
  ev_message = |Pull job started. Poll for results.|.

ENDFUNCTION.

FORM start_pull_job USING iv_url TYPE string iv_branch TYPE string iv_job_id TYPE string
  RETURNING VALUE(rv_jobcount) TYPE btcjobcount.

  DATA: lv_jobname TYPE btcjob.

  lv_jobname = |ABAPGIT_PULL_{ iv_job_id }|.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname = lv_jobname
    IMPORTING
      jobcount = rv_jobcount.

  SUBMIT z_abapgit_agent_pull_job VIA JOB rv_jobcount
    WITH pv_url = iv_url
    WITH pv_branch = iv_branch
    WITH pv_job_id = iv_job_id
    AND RETURN.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount  = rv_jobcount
      jobname   = lv_jobname
      sdlstrtdt = sy-datum
      sdlstrttm = sy-uzeit.
ENDFORM.
