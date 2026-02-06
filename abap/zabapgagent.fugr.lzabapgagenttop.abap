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
*&      Form  CHECK_LOG_FOR_ERRORS
*&---------------------------------------------------------------------*
*&      Check if the repo log contains errors and return details
*&---------------------------------------------------------------------*
FORM check_log_for_errors USING li_repo TYPE REF TO zif_abapgit_repo
                     CHANGING cv_has_error TYPE abap_bool
                              cv_error_detail TYPE string.

  DATA: lo_log TYPE REF TO zif_abapgit_log.
  DATA: lv_devclass TYPE devclass.
  DATA: lt_inactive TYPE STANDARD TABLE OF tadir.
  DATA: ls_inactive TYPE tadir.
  DATA: lv_count TYPE i.
  DATA: lv_line TYPE string.

  cv_has_error = abap_false.
  cv_error_detail = ''.

  IF li_repo IS NOT BOUND.
    RETURN.
  ENDIF.

  " Check for inactive objects (main indicator of activation errors)
  lv_devclass = li_repo->get_package( ).
  IF lv_devclass IS NOT INITIAL.
    SELECT * FROM tadir INTO TABLE lt_inactive
      WHERE devclass = lv_devclass
      AND object NOT IN ('DEVC', 'PACK').

    lv_count = lines( lt_inactive ).
    IF lv_count > 0.
      cv_error_detail = |Inactive objects in { lv_devclass } ({ lv_count }):|.
      LOOP AT lt_inactive INTO ls_inactive.
        lv_line = |  - { ls_inactive-object } { ls_inactive-obj_name }|.
        cv_error_detail = cv_error_detail && |\n| && lv_line.
      ENDLOOP.
      cv_has_error = abap_true.
    ENDIF.
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
  DATA: lt_inactive TYPE STANDARD TABLE OF tadir.
  DATA: ls_inactive TYPE tadir.
  DATA: lv_count TYPE i.
  DATA: lv_line TYPE string.

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

      " Check for inactive objects in the package
      SELECT * FROM tadir INTO TABLE lt_inactive
        WHERE devclass = lv_devclass
        AND object NOT IN ('DEVC', 'PACK').

      lv_count = lines( lt_inactive ).
      IF lv_count > 0.
        cv_detail = cv_detail && |\nInactive objects ({ lv_count }):|.
        LOOP AT lt_inactive INTO ls_inactive.
          lv_line = |  - { ls_inactive-object } { ls_inactive-obj_name }|.
          cv_detail = cv_detail && |\n| && lv_line.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
