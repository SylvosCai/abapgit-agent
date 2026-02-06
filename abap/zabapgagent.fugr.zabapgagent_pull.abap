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
  DATA: lv_job_id TYPE string.

  CLEAR: ev_message.

  lv_job_id = |{ sy-uname }{ sy-datum }{ sy-uzeit }|.

  SUBMIT zabapgagent_pull_job WITH pv_url = iv_url
    WITH pv_branch = iv_branch
    WITH pv_job_id = lv_job_id
    AND RETURN.

  ev_success = 'X'.
  ev_job_id = lv_job_id.
  ev_message = |Job submitted: { lv_job_id }|.

ENDFUNCTION.
