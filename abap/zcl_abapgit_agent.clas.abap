CLASS zcl_abapgit_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_agent.

  PRIVATE SECTION.
    DATA: mo_repo TYPE REF TO zif_abapgit_repo.

    METHODS:
      configure_credentials
        IMPORTING iv_url TYPE string
                  iv_username TYPE string
                  iv_password TYPE string
        RAISING zcx_abapgit_exception,

      prepare_deserialize_checks
        RETURNING VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
        RAISING zcx_abapgit_exception,

      check_inactive_objects
        IMPORTING iv_package TYPE devclass
        CHANGING cv_count TYPE i
                cv_detail TYPE string,

      handle_exception
        IMPORTING ix_exception TYPE REF TO cx_root
        RETURNING VALUE(rs_result) TYPE zif_abapgit_agent=>ty_result.

ENDCLASS.

CLASS zcl_abapgit_agent IMPLEMENTATION.

  METHOD zif_abapgit_agent~pull.
    DATA: lv_job_id TYPE string.
    lv_job_id = |{ sy-uname }{ sy-datum }{ sy-uzeit }|.
    rs_result-job_id = lv_job_id.
    rs_result-success = abap_false.

    IF iv_url IS INITIAL.
      rs_result-message = 'URL is required'.
      RETURN.
    ENDIF.

    TRY.
        IF iv_username IS NOT INITIAL AND iv_password IS NOT INITIAL.
          configure_credentials(
            iv_url      = iv_url
            iv_username = iv_username
            iv_password = iv_password ).
        ENDIF.

        DATA: li_repo TYPE REF TO zif_abapgit_repo.
        zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
          EXPORTING iv_url = iv_url
          IMPORTING ei_repo = li_repo ).
        mo_repo = li_repo.

        IF mo_repo IS BOUND.
          mo_repo->refresh( ).

          DATA(ls_checks) = prepare_deserialize_checks( ).

          mo_repo->create_new_log( ).

          mo_repo->deserialize(
            is_checks = ls_checks
            ii_log   = mo_repo->get_log( ) ).

          DATA: lv_inactive_count TYPE i.
          DATA: lv_inactive_detail TYPE string.
          lv_inactive_count = 0.
          lv_inactive_detail = ''.
          check_inactive_objects(
            EXPORTING iv_package = mo_repo->get_package( )
            CHANGING cv_count = lv_inactive_count
                    cv_detail = lv_inactive_detail ).

          IF lv_inactive_count > 0.
            rs_result-message = 'Pull completed with activation errors'.
            rs_result-error_detail = lv_inactive_detail.
          ELSE.
            rs_result-success = abap_true.
            rs_result-message = 'Pull completed successfully'.
          ENDIF.
        ELSE.
          rs_result-message = |Repository not found: { iv_url }|.
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(lx_git).
        rs_result = handle_exception( ix_exception = lx_git ).
      CATCH cx_root INTO DATA(lx_error).
        rs_result = handle_exception( ix_exception = lx_error ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_agent~get_repo_status.
    DATA: li_repo TYPE REF TO zif_abapgit_repo.
    zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
      EXPORTING iv_url = iv_url
      IMPORTING ei_repo = li_repo ).

    IF li_repo IS BOUND.
      rv_status = 'Found'.
    ELSE.
      rv_status = 'Not found'.
    ENDIF.
  ENDMETHOD.

  METHOD configure_credentials.
    zcl_abapgit_persist_factory=>get_user( )->set_repo_git_user_name(
      iv_url = iv_url iv_username = iv_username ).
    zcl_abapgit_persist_factory=>get_user( )->set_repo_login(
      iv_url = iv_url iv_login = iv_username ).
    zcl_abapgit_login_manager=>set_basic(
      iv_uri      = iv_url
      iv_username = iv_username
      iv_password = iv_password ).
  ENDMETHOD.

  METHOD prepare_deserialize_checks.
    rs_checks = mo_repo->deserialize_checks( ).

    DATA: ls_overwrite LIKE LINE OF rs_checks-overwrite.
    LOOP AT rs_checks-overwrite INTO ls_overwrite.
      ls_overwrite-decision = zif_abapgit_definitions=>c_yes.
      MODIFY rs_checks-overwrite FROM ls_overwrite.
    ENDLOOP.

    DATA: lo_settings TYPE REF TO zcl_abapgit_settings.
    lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    lo_settings->set_activate_wo_popup( abap_true ).
  ENDMETHOD.

  METHOD check_inactive_objects.
    DATA: lt_inactive TYPE STANDARD TABLE OF tadir.
    DATA: ls_inactive TYPE tadir.

    cv_count = 0.
    cv_detail = ''.

    IF iv_package IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM tadir INTO TABLE lt_inactive
      WHERE devclass = iv_package
      AND object NOT IN ('DEVC', 'PACK').

    cv_count = lines( lt_inactive ).
    IF cv_count > 0.
      cv_detail = |{ cv_count } inactive objects (activation errors):|.
      LOOP AT lt_inactive INTO ls_inactive.
        cv_detail = cv_detail && |\n  - { ls_inactive-object } { ls_inactive-obj_name }|.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD handle_exception.
    rs_result-success = abap_false.
    rs_result-message = ix_exception->get_text( ).

    DATA: lx_prev TYPE REF TO cx_root.
    lx_prev = ix_exception->previous.
    WHILE lx_prev IS BOUND.
      DATA: lv_msg TYPE string.
      lv_msg = lx_prev->get_text( ).
      IF lv_msg IS NOT INITIAL.
        rs_result-error_detail = rs_result-error_detail && |\n  -> { lv_msg }|.
      ENDIF.
      lx_prev = lx_prev->previous.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
