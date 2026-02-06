FUNCTION zabapgagent_get_status.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_JOB_ID) TYPE STRING
*"  EXPORTING
*"    VALUE(EV_STATUS) TYPE STRING
*"    VALUE(EV_SUCCESS) TYPE CHAR1
*"    VALUE(EV_MESSAGE) TYPE STRING
*"----------------------------------------------------------------------
  CLEAR: ev_status, ev_message.

  " TEST SYNTAX ERROR
  syntax_error_for_test.

  ev_status = 'COMPLETED'.
  ev_success = 'X'.
  ev_message = |Job { iv_job_id } found|.

ENDFUNCTION.
