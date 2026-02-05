"&lt;function Z_ABAPGIT_AGENT_GET_STATUS
"&gt; Purpose: Get status and result of a pull/activation job
"&gt; Created by: ABAP AI Bridge
"&gt; Date: 2026-02-05

FUNCTION z_abapgit_agent_get_status.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_JOB_ID) TYPE STRING
*"  EXPORTING
*"    EV_STATUS TYPE STRING
*"    EV_SUCCESS TYPE ABAP_BOOL
*"    EV_MESSAGE TYPE STRING
*"    EV_ACTIVATED_COUNT TYPE I
*"    EV_FAILED_COUNT TYPE I
*"  TABLES
*"    ET_ERROR_LOG TYPE STRING_TABLE
*"----------------------------------------------------------------------
  DATA: lv_status TYPE zif_abapgit_agent=>ty_job_status.
  DATA: ls_result TYPE zif_abapgit_agent=>ty_result.

  " Read job status from database table
  SELECT SINGLE * FROM z_abapgit_agent_log
    WHERE job_id = @iv_job_id
    INTO @DATA(ls_log_entry).

  IF sy-subrc <> 0.
    ev_status = 'NOT_FOUND'.
    ev_success = abap_false.
    ev_message = |Job { iv_job_id } not found|.
    RETURN.
  ENDIF.

  " Get job result
  SELECT SINGLE * FROM z_abapgit_agent_res
    WHERE job_id = @iv_job_id
    INTO @ls_result.

  IF sy-subrc = 0.
    ev_status = ls_result-status.
    ev_success = ls_result-success.
    ev_message = ls_result-message.
    ev_activated_count = ls_result-activated_count.
    ev_failed_count = ls_result-failed_count.

    " Get error log
    SELECT message FROM z_abapgit_agent_err
      WHERE job_id = @iv_job_id
      INTO TABLE @et_error_log.
  ELSE.
    " Job is still running
    ev_status = 'RUNNING'.
    ev_success = abap_true.
    ev_message = 'Job still running'.
  ENDIF.

ENDFUNCTION.
