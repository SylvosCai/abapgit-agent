FUNCTION-POOL z_abapgagent.            "MESSAGE-ID ..

" Global variables for password popup authentication
DATA: gv_auth_user TYPE string.
DATA: gv_auth_pass TYPE string.

*&---------------------------------------------------------------------*
*&      Form  PASSWORD_POPUP
*&---------------------------------------------------------------------*
*&      Used by abapGit to get credentials
*&      Note: For API usage, credentials are passed via function module
*&            parameters, not via this popup
*&---------------------------------------------------------------------*
FORM password_popup USING iv_repo_url TYPE string
                 CHANGING cv_user TYPE string
                          cv_pass TYPE string.

  " For API usage, return empty - credentials should be passed directly
  " If global variables are set, use those (for background job usage)
  IF gv_auth_user IS NOT INITIAL.
    cv_user = gv_auth_user.
    cv_pass = gv_auth_pass.
  ELSE.
    " Clear to force abapGit to prompt or fail
    CLEAR: cv_user, cv_pass.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_LOG_DETAILS
*&---------------------------------------------------------------------*
*&      Extract detailed log information from abapGit repo log
*&---------------------------------------------------------------------*
FORM get_log_details USING li_repo TYPE REF TO zif_abapgit_repo
                 RETURNING rv_details TYPE string.

  DATA: lo_log TYPE REF TO zif_abapgit_log.
  DATA: lt_log TYPE zif_abapgit_log=>ty_log_ts.
  DATA: ls_log TYPE zif_abapgit_log=>ty_log.
  DATA: lv_severity TYPE string.
  DATA: lv_line TYPE string.

  rv_details = ''.

  IF li_repo IS NOT BOUND.
    RETURN.
  ENDIF.

  " Try to get the log from repo
  lo_log = li_repo->get_log( ).
  IF lo_log IS NOT BOUND.
    RETURN.
  ENDIF.

  " Get all log entries
  lt_log = lo_log->get_messages( ).

  " Build detailed string from log entries
  LOOP AT lt_log INTO ls_log.
    " Determine severity text
    CASE ls_log-type.
      WHEN 'E' OR 'W'.
        lv_severity = 'ERROR'.
      WHEN 'I'.
        lv_severity = 'INFO'.
      WHEN 'S'.
        lv_severity = 'SUCCESS'.
      WHEN OTHERS.
        lv_severity = 'LOG'.
    ENDCASE.

    " Format: [TYPE] Message
    lv_line = |[{ lv_severity }] { ls_log-text }|.
    IF rv_details IS INITIAL.
      rv_details = lv_line.
    ELSE.
      rv_details = rv_details && |\n| && lv_line.
    ENDIF.
  ENDLOOP.

ENDFORM.
