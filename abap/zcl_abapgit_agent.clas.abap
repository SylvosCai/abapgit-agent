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

      check_log_for_errors
        EXPORTING ev_has_error TYPE abap_bool
                  ev_detail TYPE string,

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

          " Check the abapGit log for errors
          DATA: lv_has_error TYPE abap_bool.
          DATA: lv_error_detail TYPE string.
          check_log_for_errors(
            EXPORTING ev_has_error = lv_has_error
                      ev_detail    = lv_error_detail ).

          IF lv_has_error = abap_true.
            rs_result-message = 'Pull completed with errors'.
            rs_result-error_detail = lv_error_detail.
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

  METHOD check_log_for_errors.
    DATA: lo_log TYPE REF TO zif_abapgit_log.

    ev_has_error = abap_false.
    ev_detail = ''.

    lo_log = mo_repo->get_log( ).
    IF lo_log IS BOUND.

      " Check overall status
      DATA(lv_status) = lo_log->get_status( ).
      IF lv_status = zif_abapgit_log=>c_status-error.
        ev_has_error = abap_true.
      ENDIF.

      " Get all messages for detail
      DATA: lt_messages TYPE zif_abapgit_log=>ty_msgs.
      DATA: ls_msg TYPE zif_abapgit_log=>ty_msg.
      lt_messages = lo_log->get_messages( ).

      " Build detail string from error/warning messages
      LOOP AT lt_messages INTO ls_msg.
        IF ls_msg-type = 'E' OR ls_msg-type = 'A' OR ls_msg-type = 'W'.
          IF ls_msg-obj_type IS NOT INITIAL AND ls_msg-obj_name IS NOT INITIAL.
            ev_detail = ev_detail && |\n  - { ls_msg-obj_type } { ls_msg-obj_name }: { ls_msg-text }|.
          ELSE.
            ev_detail = ev_detail && |\n  - { ls_msg-text }|.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF ev_detail IS NOT INITIAL.
        ev_detail = |Errors/Warnings:{ ev_detail }|.
      ENDIF.
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
