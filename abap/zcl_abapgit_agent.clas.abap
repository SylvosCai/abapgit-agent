"<!> <class ZCL_ABAPGIT_AGENT
"!> Purpose: ABAP Git Agent - OO implementation for git pull and activation
"!> Created by: ABAP AI Bridge
"!> Date: 2026-02-06

CLASS zcl_abapgit_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_agent.

  PRIVATE SECTION.
    DATA: mo_repo TYPE REF TO zif_abapgit_repo,
          mv_url TYPE string,
          mv_job_id TYPE string.

    METHODS:
      configure_credentials
        IMPORTING iv_url TYPE string
                  iv_username TYPE string
                  iv_password TYPE string,

      prepare_deserialize_checks
        RETURNING VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks,

      check_for_errors
        IMPORTING io_repo TYPE REF TO zif_abapgit_repo
        RETURNING VALUE(rv_has_error) TYPE abap_bool
        RETURNING VALUE(rv_error_detail) TYPE string,

      handle_exception
        IMPORTING ix_exception TYPE REF TO cx_root
        RETURNING VALUE(rs_result) TYPE zif_abapgit_agent=>ty_result.

ENDCLASS.

CLASS zcl_abapgit_agent IMPLEMENTATION.

  METHOD zif_abapgit_agent~pull.
    " Initialize result
    rs_result-job_id = |{ sy-uname }{ sy-datum }{ sy-uzeit }|.
    rs_result-started_at = |{ sy-datlo }{ sy-timlo }|.

    " Validate URL
    IF iv_url IS INITIAL.
      rs_result-success = abap_false.
      rs_result-message = 'URL is required'.
      RETURN.
    ENDIF.

    mv_url = iv_url.

    TRY.
        " Configure credentials if provided
        IF iv_username IS NOT INITIAL AND iv_password IS NOT INITIAL.
          configure_credentials(
            iv_url      = iv_url
            iv_username = iv_username
            iv_password = iv_password ).
        ENDIF.

        " Find or create repository
        find_or_create_repo(
          iv_url    = iv_url
          iv_branch = iv_branch ).

        " Check if repo was found
        IF mo_repo IS NOT BOUND.
          rs_result-success = abap_false.
          rs_result-message = |Repository not found: { iv_url }|.
          RETURN.
        ENDIF.

        " Get deserialize checks
        DATA(ls_checks) = prepare_deserialize_checks( ).

        " Create new log
        mo_repo->create_new_log( ).

        " Pull and deserialize
        mo_repo->deserialize(
          is_checks = ls_checks
          ii_log   = mo_repo->get_log( ) ).

        " Check for activation errors
        DATA(lv_has_error) = abap_false.
        DATA(lv_error_detail) = check_for_errors(
          io_repo = mo_repo
          rv_has_error = lv_has_error
          rv_error_detail = DATA(lv_detail) ).

        IF lv_has_error = abap_true.
          rs_result-success = abap_false.
          rs_result-message = 'Pull completed with activation errors'.
          rs_result-error_detail = lv_error_detail.
        ELSE.
          rs_result-success = abap_true.
          rs_result-message = 'Pull completed successfully'.
        ENDIF.

        rs_result-finished_at = |{ sy-datlo }{ sy-timlo }|.

      CATCH zcx_abapgit_exception INTO DATA(lx_git).
        rs_result = handle_exception(
          ix_exception = lx_git ).
      CATCH cx_root INTO DATA(lx_error).
        rs_result = handle_exception(
          ix_exception = lx_error ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_agent~get_repo_status.
    " Find repository by URL
    zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
      EXPORTING
        iv_url    = iv_url
      IMPORTING
        ei_repo   = DATA(li_repo)
        ev_reason = DATA(lv_reason) ).

    IF li_repo IS BOUND.
      rv_status = 'Found'.
    ELSE.
      rv_status = |Not found: { lv_reason }|.
    ENDIF.
  ENDMETHOD.

  METHOD configure_credentials.
    " Store username in abapGit user persistence
    zcl_abapgit_persist_factory=>get_user( )->set_repo_git_user_name(
      iv_url = iv_url iv_username = iv_username ).
    zcl_abapgit_persist_factory=>get_user( )->set_repo_login(
      iv_url = iv_url iv_login = iv_username ).

    " Use login_manager to set Basic auth directly
    zcl_abapgit_login_manager=>set_basic(
      iv_uri      = iv_url
      iv_username = iv_username
      iv_password = iv_password ).
  ENDMETHOD.

  METHOD prepare_deserialize_checks.
    " Get deserialize checks
    rs_checks = mo_repo->deserialize_checks( ).

    " Set all overwrite decisions to YES (non-GUI mode)
    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF rs_checks-overwrite.
    LOOP AT rs_checks-overwrite ASSIGNING <ls_overwrite>.
      <ls_overwrite>-decision = zif_abapgit_definitions=>c_yes.
    ENDLOOP.

    " Enable activate without popup
    DATA(lo_settings) = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    DATA(lv_activation_setting) = lo_settings->get_activate_wo_popup( ).
    lo_settings->set_activate_wo_popup( abap_true ).
  ENDMETHOD.

  METHOD find_or_create_repo.
    " Check if repo already exists
    zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
      EXPORTING
        iv_url    = iv_url
      IMPORTING
        ei_repo   = mo_repo
        ev_reason = DATA(lv_reason) ).

    " Repo found - refresh it
    IF mo_repo IS BOUND.
      mo_repo->refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD check_for_errors.
    " Get log entries
    DATA(lo_log) = io_repo->get_log( ).
    IF lo_log IS BOUND.
      DATA(lt_bapiret2) = lo_log->get_bapiret2( ).

      " Filter for error messages
      LOOP AT lt_bapiret2 ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).
        IF <ls_bapiret2>-type = 'E' OR <ls_bapiret2>-type = 'W'.
          rv_has_error = abap_true.
          DATA(lv_line) = |{ <ls_bapiret2>-type }: { <ls_bapiret2>-id }/{ <ls_bapiret2>-number } - { <ls_bapiret2>-message }|.
          IF rv_error_detail IS INITIAL.
            rv_error_detail = lv_line.
          ELSE.
            rv_error_detail = rv_error_detail && |\n| && lv_line.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD handle_exception.
    rs_result-success = abap_false.
    rs_result-message = ix_exception->get_text( ).

    " Get previous exception if available
    DATA(lx_previous) = ix_exception->previous.
    WHILE lx_previous IS BOUND.
      DATA(lv_prev_msg) = lx_previous->get_text( ).
      IF lv_prev_msg IS NOT INITIAL.
        rs_result-error_detail = rs_result-error_detail && |\n  -> { lv_prev_msg }|.
      ENDIF.
      lx_previous = lx_previous->previous.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
