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
  DATA: ls_result TYPE zif_abapgit_agent=>ty_result.

  SELECT SINGLE * FROM z_abapgit_agent_res
    WHERE job_id = @iv_job_id
    INTO @ls_result.

  IF sy-subrc <> 0.
    ev_status = 'NOT_FOUND'.
    ev_success = abap_false.
    ev_message = |Job { iv_job_id } not found|.
    RETURN.
  ENDIF.

  ev_status = ls_result-status.
  ev_success = ls_result-success.
  ev_message = ls_result-message.
  ev_activated_count = ls_result-activated_count.
  ev_failed_count = ls_result-failed_count.

  SELECT message FROM z_abapgit_agent_err
    WHERE job_id = @iv_job_id
    INTO TABLE @et_error_log.

ENDFUNCTION.
