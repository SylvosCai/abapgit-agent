" TODO: Implement detailed syntax error parsing
" When a syntax error occurs, the log shows the affected object name
" but not the specific line/column. For better error reporting:
" - Parse the error message to extract object info
" - For syntax errors, query SEPSA or TRINT_OBJECT_LOG for details
" - Return structured error with line numbers and fix suggestions

CLASS zcl_abapgit_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_agent.

    METHODS: get_version RETURNING VALUE(rv_version) TYPE string.

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
        RETURNING VALUE(rv_has_error) TYPE abap_bool,

      get_log_detail
        RETURNING VALUE(rv_detail) TYPE string,

      get_object_lists
        RETURNING VALUE(rs_result) TYPE zif_abapgit_agent=>ty_result,

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

          " Check the abapGit log for errors and extract object lists
          DATA(lv_has_error) = check_log_for_errors( ).
          DATA(lv_error_detail) = get_log_detail( ).

          " Extract activated and failed objects from the log
          DATA(ls_obj_result) = get_object_lists( ).

          rs_result-activated_objects = ls_obj_result-activated_objects.
          rs_result-failed_objects = ls_obj_result-failed_objects.

          " Count objects
          rs_result-activated_count = 0.
          LOOP AT rs_result-activated_objects ASSIGNING FIELD-SYMBOL(<ls_act>).
            rs_result-activated_count = rs_result-activated_count + 1.
          ENDLOOP.

          rs_result-failed_count = 0.
          LOOP AT rs_result-failed_objects ASSIGNING FIELD-SYMBOL(<ls_fail>).
            rs_result-failed_count = rs_result-failed_count + 1.
          ENDLOOP.

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
    TRY.
        zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
          EXPORTING iv_url = iv_url
          IMPORTING ei_repo = li_repo ).
      CATCH zcx_abapgit_exception.
        rv_status = 'Not found'.
        RETURN.
    ENDTRY.

    IF li_repo IS BOUND.
      rv_status = 'Found'.
    ELSE.
      rv_status = 'Not found'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_agent~syntax_check.
    rs_result-success = abap_false.
    rs_result-object_type = iv_object_type.
    rs_result-object_name = iv_object_name.

    IF iv_object_type IS INITIAL OR iv_object_name IS INITIAL.
      rs_result-error_count = 1.
      DATA(ls_err) = VALUE zif_abapgit_agent=>ty_syntax_error( line = '1' column = '1' text = 'Object type and name are required' ).
      APPEND ls_err TO rs_result-errors.
      RETURN.
    ENDIF.

    " Check if function module exists
    DATA lv_func_exists TYPE abap_bool.
    lv_func_exists = abap_false.
    FUNCTION EXISTS RSYNTAX_CHECK_OBJECT.
      lv_func_exists = abap_true.
    ENDFUNCTION.

    IF lv_func_exists = abap_false.
      rs_result-error_count = 1.
      ls_err-line = '1'.
      ls_err-column = '1'.
      ls_err-text = 'Syntax check not available in this system'.
      APPEND ls_err TO rs_result-errors.
      RETURN.
    ENDIF.

    " Local structure for syntax errors
    DATA: BEGIN OF ls_error,
            line TYPE string,
            column TYPE string,
            text TYPE string,
            word TYPE string,
          END OF ls_error.
    DATA lt_errors LIKE TABLE OF ls_error.

    " Call syntax check function module
    CALL FUNCTION 'RSYNTAX_CHECK_OBJECT'
      EXPORTING
        object_name = iv_object_name
        object_type = iv_object_type
      TABLES
        error_table = lt_errors
      EXCEPTIONS
        object_not_found = 1
        OTHERS = 2.

    IF sy-subrc <> 0.
      rs_result-error_count = 1.
      ls_err-line = '1'.
      ls_err-column = '1'.
      ls_err-text = |Syntax check failed (RC: { sy-subrc })|.
      APPEND ls_err TO rs_result-errors.
      RETURN.
    ENDIF.

    " Process errors
    DATA lv_error_count TYPE i.
    lv_error_count = lines( lt_errors ).
    rs_result-error_count = lv_error_count.

    LOOP AT lt_errors INTO ls_error.
      ls_err-line = ls_error-line.
      ls_err-column = ls_error-column.
      ls_err-text = ls_error-text.
      ls_err-word = ls_error-word.
      APPEND ls_err TO rs_result-errors.
    ENDLOOP.

    IF lv_error_count = 0.
      rs_result-success = abap_true.
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

    rv_has_error = abap_false.

    lo_log = mo_repo->get_log( ).
    IF lo_log IS BOUND.
      DATA(lv_status) = lo_log->get_status( ).
      IF lv_status = zif_abapgit_log=>c_status-error.
        rv_has_error = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_log_detail.
    " TODO: For syntax errors (type 'E'), the message contains "Error updating where-used list"
    " Enhance to extract:
    " - Line number from error message
    " - Column number if available
    " - Error code (e.g., "ZC123")
    " - Query SNHI for detailed syntax information
    DATA: lo_log TYPE REF TO zif_abapgit_log.

    rv_detail = ''.

    lo_log = mo_repo->get_log( ).
    IF lo_log IS BOUND.
      DATA: lt_messages TYPE zif_abapgit_log=>ty_log_outs.
      DATA: ls_msg TYPE zif_abapgit_log=>ty_log_out.
      lt_messages = lo_log->get_messages( ).

      LOOP AT lt_messages INTO ls_msg.
        IF ls_msg-type = 'E' OR ls_msg-type = 'A' OR ls_msg-type = 'W'.
          IF ls_msg-obj_type IS NOT INITIAL AND ls_msg-obj_name IS NOT INITIAL.
            rv_detail = rv_detail && '\n  - ' && ls_msg-obj_type && ' ' && ls_msg-obj_name && ': ' && ls_msg-text.
          ELSE.
            rv_detail = rv_detail && '\n  - ' && ls_msg-text.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF rv_detail IS NOT INITIAL.
        rv_detail = 'Errors/Warnings:' && rv_detail.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_object_lists.
    " Extract activated and failed objects from the log
    DATA: lo_log TYPE REF TO zif_abapgit_log.

    CLEAR: rs_result-activated_objects, rs_result-failed_objects.

    lo_log = mo_repo->get_log( ).
    IF lo_log IS BOUND.
      DATA: lt_messages TYPE zif_abapgit_log=>ty_log_outs.
      DATA: ls_msg TYPE zif_abapgit_log=>ty_log_out.
      lt_messages = lo_log->get_messages( ).

      LOOP AT lt_messages INTO ls_msg.
        DATA: ls_object TYPE zif_abapgit_agent=>ty_object.
        ls_object-obj_type = ls_msg-obj_type.
        ls_object-obj_name = ls_msg-obj_name.
        ls_object-text = ls_msg-text.

        " Success messages (type 'S') - activated objects
        IF ls_msg-type = 'S'.
          APPEND ls_object TO rs_result-activated_objects.
        " Error/Abort/Warning messages - failed objects
        ELSEIF ls_msg-type = 'E' OR ls_msg-type = 'A' OR ls_msg-type = 'W'.
          APPEND ls_object TO rs_result-failed_objects.
        ENDIF.
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
        rs_result-error_detail = rs_result-error_detail && '\n  -> ' && lv_msg.
      ENDIF.
      lx_prev = lx_prev->previous.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_version.
    rv_version = '1.0.0'.
  ENDMETHOD.

ENDCLASS.
