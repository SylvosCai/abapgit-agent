*&---------------------------------------------------------------------*
*& Report  ZABAPGAGENT_PULL_JOB
*&---------------------------------------------------------------------*
*& Standalone program to pull and activate ABAP code from git
*&---------------------------------------------------------------------*
REPORT zabapgagent_pull_job.

PARAMETERS:
  p_url    TYPE string LOWER CASE OBLIGATORY,
  p_branch TYPE string LOWER CASE DEFAULT 'main',
  p_test   TYPE abap_bool AS CHECKBOX DEFAULT abap_false,
  p_job_id TYPE string.

START-OF-SELECTION.

  WRITE: / 'ABAP Git Agent - Pull Job'.
  WRITE: / 'URL:', p_url.
  WRITE: / 'Branch:', p_branch.
  WRITE: / 'Job ID:', p_job_id.
  IF p_test = abap_true.
    WRITE: / 'TEST MODE - No changes will be made.'.
  ENDIF.
  ULINE.

  IF lv_success = abap_true AND lv_job_id IS NOT INITIAL AND p_test = abap_false.
    ULINE.
    WRITE: / 'Checking job status...'.

    DATA lv_status TYPE string.
    DATA lv_retry TYPE i VALUE 0.
    DATA lv_max_retry TYPE i VALUE 30.

    WHILE lv_retry < lv_max_retry.
      CALL FUNCTION 'ZABAPGAGENT_GET_STATUS'
        EXPORTING
          iv_job_id = lv_job_id
        IMPORTING
          ev_status  = lv_status
          ev_success = lv_success
          ev_message = lv_message.

      WRITE: / 'Status:', lv_status.

      IF lv_status = 'COMPLETED' OR lv_status = 'FAILED'.
        EXIT.
      ENDIF.

      lv_retry = lv_retry + 1.
      WAIT UP TO 2 SECONDS.
    ENDWHILE.

    IF lv_retry >= lv_max_retry.
      WRITE: / 'Warning: Polling timed out. Check job status manually.'.
    ELSE.
      ULINE.
      WRITE: / 'Final Result:'.
      WRITE: / 'Success:', lv_success.
      WRITE: / 'Status:', lv_status.
      WRITE: / 'Message:', lv_message.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
