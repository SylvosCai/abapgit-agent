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

  IF gv_auth_user IS NOT INITIAL.
    cv_user = gv_auth_user.
    cv_pass = gv_auth_pass.
  ELSE.
    CLEAR: cv_user, cv_pass.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_LOG_FOR_ERRORS
*&---------------------------------------------------------------------*
FORM check_log_for_errors USING io_repo TYPE REF TO zif_abapgit_repo
                     CHANGING cv_has_error TYPE abap_bool
                              cv_error_detail TYPE string.

  DATA: lt_inactive TYPE STANDARD TABLE OF tadir.
  DATA: ls_inactive TYPE tadir.

  cv_has_error = abap_false.
  cv_error_detail = ''.

  IF io_repo IS NOT BOUND.
    RETURN.
  ENDIF.

  DATA(lv_devclass) = io_repo->get_package( ).
  IF lv_devclass IS INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_inactive
    WHERE devclass = lv_devclass
    AND object NOT IN ('DEVC', 'PACK').

  DATA(lv_count) = lines( lt_inactive ).
  IF lv_count > 0.
    cv_has_error = abap_true.
    cv_error_detail = |{ lv_count } inactive objects (activation errors):|.
    LOOP AT lt_inactive INTO ls_inactive.
      cv_error_detail = cv_error_detail && |\n  - { ls_inactive-object } { ls_inactive-obj_name }|.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_ERROR_DETAIL
*&---------------------------------------------------------------------*
FORM build_error_detail USING ix_exception TYPE REF TO zcx_abapgit_exception
                              ii_repo TYPE REF TO zif_abapgit_repo
                    CHANGING cv_detail TYPE string.

  DATA: lv_devclass TYPE devclass.
  DATA: lt_inactive TYPE STANDARD TABLE OF tadir.
  DATA: ls_inactive TYPE tadir.
  DATA: lv_count TYPE i.
  DATA: lv_line TYPE string.

  cv_detail = ix_exception->get_text( ).

  " Check for previous exception in chain
  DATA(lx_previous) = ix_exception->previous.
  IF lx_previous IS BOUND.
    DATA(lv_prev_text) = lx_previous->get_text( ).
    IF lv_prev_text IS NOT INITIAL.
      cv_detail = cv_detail && |\nCaused by: { lv_prev_text }|.
    ENDIF.
  ENDIF.

  " Add repository info
  IF ii_repo IS BOUND.
    lv_devclass = ii_repo->get_package( ).
    IF lv_devclass IS NOT INITIAL.
      cv_detail = cv_detail && |\nPackage: { lv_devclass }|.

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
