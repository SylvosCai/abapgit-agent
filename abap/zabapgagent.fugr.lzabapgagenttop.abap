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
*&      Form  BUILD_ERROR_DETAIL
*&---------------------------------------------------------------------*
*&      Build detailed error information from exception
*&---------------------------------------------------------------------*
FORM build_error_detail USING ix_exception TYPE REF TO zcx_abapgit_exception
                              ii_repo TYPE REF TO zif_abapgit_repo
                    CHANGING cv_detail TYPE string.

  DATA: lv_devclass TYPE devclass.
  DATA: lv_prev_text TYPE string.
  DATA: lv_msg TYPE string.

  cv_detail = ix_exception->get_text( ).

  " Check for previous exception in chain
  DATA(lx_previous) = ix_exception->previous.
  IF lx_previous IS BOUND.
    lv_prev_text = lx_previous->get_text( ).
    IF lv_prev_text IS NOT INITIAL.
      cv_detail = cv_detail && |\nCaused by: { lv_prev_text }|.
    ENDIF.
  ENDIF.

  " Add repository info
  IF ii_repo IS BOUND.
    lv_devclass = ii_repo->get_package( ).
    IF lv_devclass IS NOT INITIAL.
      cv_detail = cv_detail && |\nPackage: { lv_devclass }|.
    ENDIF.
  ENDIF.

ENDFORM.
