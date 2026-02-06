FUNCTION zabapgagent_pull.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_URL) TYPE STRING
*"    VALUE(IV_BRANCH) TYPE STRING DEFAULT 'main'
*"    VALUE(IV_TEST_RUN) TYPE CHAR1 DEFAULT ' '
*"    VALUE(IV_USERNAME) TYPE STRING OPTIONAL
*"    VALUE(IV_PASSWORD) TYPE STRING OPTIONAL
*"  EXPORTING
*"    VALUE(EV_SUCCESS) TYPE CHAR1
*"    VALUE(EV_JOB_ID) TYPE STRING
*"    VALUE(EV_MESSAGE) TYPE STRING
*"  RAISING
*"    ERROR
*"    ZCX_ABAPGIT_EXCEPTION
*"----------------------------------------------------------------------
  DATA: lv_job_id TYPE string.

  CLEAR: ev_message.

  lv_job_id = |{ sy-uname }{ sy-datum }{ sy-uzeit }|.

  " Call ZABAPGAGENT_DO_PULL directly (synchronous - waits for completion)
  CALL FUNCTION 'ZABAPGAGENT_DO_PULL'
    EXPORTING
      iv_url         = iv_url
      iv_branch      = iv_branch
      iv_test_run    = iv_test_run
      iv_job_id      = lv_job_id
      iv_username    = iv_username
      iv_password    = iv_password
    IMPORTING
      ev_success     = ev_success
      ev_message     = ev_message.

  ev_job_id = lv_job_id.

ENDFUNCTION.
