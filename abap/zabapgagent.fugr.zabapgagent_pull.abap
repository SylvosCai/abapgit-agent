FUNCTION zabapgagent_pull.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_URL) TYPE STRING
*"    VALUE(IV_BRANCH) TYPE STRING DEFAULT 'main'
*"  EXPORTING
*"    VALUE(EV_SUCCESS) TYPE CHAR1
*"    VALUE(EV_JOB_ID) TYPE STRING
*"    VALUE(EV_MESSAGE) TYPE STRING
*"----------------------------------------------------------------------
  DATA: lv_job_id TYPE string,
        lv_jobcount TYPE char12,
        lv_jobname TYPE char32,
        lv_datum TYPE d,
        lv_uzeit TYPE t.

  CLEAR: ev_message.

  lv_datum = sy-datum.
  lv_uzeit = sy-uzeit.
  lv_job_id = |{ sy-uname }{ lv_datum }{ lv_uzeit }|.
  lv_jobname = |ZABAPG_{ lv_job_id }|.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname = lv_jobname
    IMPORTING
      jobcount = lv_jobcount.

  SUBMIT zabapgagent_pull_job VIA JOB lv_jobcount
    WITH pv_url = iv_url
    WITH pv_branch = iv_branch
    WITH pv_job_id = lv_job_id
    AND RETURN.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount  = lv_jobcount
      jobname   = lv_jobname
      sdlstrtdt = sy-datum
      sdlstrttm = sy-uzeit.

  ev_success = 'X'.
  ev_job_id = lv_job_id.
  ev_message = |Job started: { lv_jobname }|.

ENDFUNCTION.
