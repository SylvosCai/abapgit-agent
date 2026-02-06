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

  SUBMIT zabapgagent_pull_job WITH pv_url = iv_url
    WITH pv_branch = iv_branch
    WITH pv_test = iv_test_run
    WITH pv_username = iv_username
    WITH pv_password = iv_password
    WITH pv_job_id = lv_job_id
    AND RETURN.

  ev_success = 'X'.
  ev_job_id = lv_job_id.
  ev_message = |Job submitted: { lv_job_id }|.

ENDFUNCTION.
