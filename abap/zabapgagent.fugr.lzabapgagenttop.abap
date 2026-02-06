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
