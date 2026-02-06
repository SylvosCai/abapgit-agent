FUNCTION-POOL z_abapgagent.            "MESSAGE-ID ..

" Global variables for password popup authentication
DATA: gv_auth_user TYPE string.
DATA: gv_auth_pass TYPE string.

*&---------------------------------------------------------------------*
*&      Form  PASSWORD_POPUP
*&---------------------------------------------------------------------*
*&      Used by abapGit to get credentials
*&---------------------------------------------------------------------*
FORM password_popup USING iv_repo_url TYPE string
                 CHANGING cv_user TYPE string
                          cv_pass TYPE string.

  cv_user = gv_auth_user.
  cv_pass = gv_auth_pass.

ENDFORM.
