FUNCTION zabapgagent_get_status.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_JOB_ID) TYPE STRING
*"  EXPORTING
*"    VALUE(EV_STATUS) TYPE STRING
*"    VALUE(EV_SUCCESS) TYPE ABAP_BOOL
*"    VALUE(EV_MESSAGE) TYPE STRING
*"----------------------------------------------------------------------
  DATA: lv_job_id TYPE string.

  CLEAR: ev_status, ev_message.

  SELECT SINGLE job_id FROM z_abapgres
    WHERE job_id = @iv_job_id
    INTO @lv_job_id.

  IF sy-subrc <> 0.
    ev_status = 'NOT_FOUND'.
    ev_success = abap_false.
    ev_message = |Job { iv_job_id } not found|.
    RETURN.
  ENDIF.

  ev_status = 'COMPLETED'.
  ev_success = abap_true.
  ev_message = |Job { iv_job_id } found|.

ENDFUNCTION.
