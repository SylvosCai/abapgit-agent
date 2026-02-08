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
    GET TIME STAMP FIELD rs_result-started_at.

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

          " If specific files are requested, deserialize only those files
          IF it_files IS NOT INITIAL AND lines( it_files ) > 0.
            " Deserialize specific files only
            DATA(lv_file_count) = lines( it_files ).

            " Create a log for file operations
            mo_repo->create_new_log( ).

            " Process each file
            LOOP AT it_files INTO DATA(lv_file).
              " Parse file name to get object type and name
              DATA: lv_obj_type TYPE string.
              DATA: lv_obj_name TYPE string.
              DATA: lv_file_name TYPE string.

              lv_file_name = lv_file.

              " Parse file name: e.g., "zcl_my_class.clas.abap" -> CLAS / ZCL_MY_CLASS
              IF lv_file_name CP '*.clas.abap'.
                lv_obj_type = 'CLAS'.
                lv_obj_name = to_upper( lv_file_name ).
                REPLACE ALL OCCURRENCES OF '.CLAS.ABAP' IN lv_obj_name WITH ''.
              ELSEIF lv_file_name CP '*.prog.abap'.
                lv_obj_type = 'PROG'.
                lv_obj_name = to_upper( lv_file_name ).
                REPLACE ALL OCCURRENCES OF '.PROG.ABAP' IN lv_obj_name WITH ''.
              ELSEIF lv_file_name CP '*.intf.abap'.
                lv_obj_type = 'INTF'.
                lv_obj_name = to_upper( lv_file_name ).
                REPLACE ALL OCCURRENCES OF '.INTF.ABAP' IN lv_obj_name WITH ''.
              ELSE.
                " Try to determine from folder structure or use default
                CONTINUE.
              ENDIF.

              " Get file object from repo
              DATA: li_file TYPE REF TO zif_abapgit_repo_file.
              DATA: lv_path TYPE string.

              lv_path = lv_file.

              " Get file from repo
              li_file = mo_repo->get_file( iv_path = lv_path ).

              IF li_file IS BOUND.
                " Deserialize single file using file logic
                DATA: li_file_logic TYPE REF TO zcl_abapgit_file_logic.
                CREATE OBJECT li_file_logic.

                DATA: ls_file TYPE zif_abapgit_types=>ty_file.
                ls_file-path = li_file->get_path( ).
                ls_file-filename = li_file->get_filename( ).
                ls_file-data = li_file->get_data( ).

                " Deserialize the file
                li_file_logic->deserialize(
                  EXPORTING
                    is_file = ls_file
                    iv_dot_abapgit = abap_true
                    ii_log = mo_repo->get_log( ) ).

                " Check for errors in the log
                DATA(lv_has_error) = check_log_for_errors( ).

                " Record the result
                DATA: ls_object TYPE zif_abapgit_agent=>ty_object.
                ls_object-obj_type = lv_obj_type.
                ls_object-obj_name = lv_obj_name.
                IF lv_has_error = abap_true.
                  ls_object-type = 'E'.
                  ls_object-text = |File { lv_file } activation failed|.
                  APPEND ls_object TO rs_result-failed_objects.
                ELSE.
                  ls_object-type = 'S'.
                  ls_object-text = |File { lv_file } activated|.
                  APPEND ls_object TO rs_result-activated_objects.
                ENDIF.
              ENDIF.
            ENDLOOP.

            rs_result-activated_count = lines( rs_result-activated_objects ).
            rs_result-failed_count = lines( rs_result-failed_objects ).
            rs_result-log_messages = rs_result-failed_objects.
            APPEND LINES OF rs_result-activated_objects TO rs_result-log_messages.

            IF rs_result-failed_count > 0.
              rs_result-message = |Pull completed with { rs_result-failed_count } file(s) failed|.
              rs_result-error_detail = get_log_detail( ).
            ELSE.
              rs_result-success = abap_true.
              rs_result-message = |Pull completed successfully ({ lv_file_count } file(s))|.
            ENDIF.

          ELSE.
            " Full repo pull (original behavior)
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

            rs_result-log_messages = ls_obj_result-log_messages.
            rs_result-activated_objects = ls_obj_result-activated_objects.
            rs_result-failed_objects = ls_obj_result-failed_objects.

            " Count objects
            rs_result-activated_count = lines( rs_result-activated_objects ).
            rs_result-failed_count = lines( rs_result-failed_objects ).

            GET TIME STAMP FIELD rs_result-finished_at.

            IF lv_has_error = abap_true.
              rs_result-message = 'Pull completed with errors'.
              rs_result-error_detail = lv_error_detail.
            ELSE.
              rs_result-success = abap_true.
              rs_result-message = 'Pull completed successfully'.
            ENDIF.
          ENDIF.

          GET TIME STAMP FIELD rs_result-finished_at.

        ELSE.
          rs_result-message = |Repository not found: { iv_url }|.
          GET TIME STAMP FIELD rs_result-finished_at.
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(lx_git).
        rs_result = handle_exception( ix_exception = lx_git ).
        GET TIME STAMP FIELD rs_result-finished_at.
      CATCH cx_root INTO DATA(lx_error).
        rs_result = handle_exception( ix_exception = lx_error ).
        GET TIME STAMP FIELD rs_result-finished_at.
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
    " Extract detailed log messages including type, id, number, text, obj_type, obj_name, exception
    DATA: lo_log TYPE REF TO zif_abapgit_log.

    rv_detail = ''.

    lo_log = mo_repo->get_log( ).
    IF lo_log IS BOUND.
      DATA: lt_messages TYPE zif_abapgit_log=>ty_log_outs.
      DATA: ls_msg TYPE zif_abapgit_log=>ty_log_out.
      lt_messages = lo_log->get_messages( ).

      DATA lv_first TYPE abap_bool VALUE abap_false.

      LOOP AT lt_messages INTO ls_msg.
        IF ls_msg-type = 'E' OR ls_msg-type = 'A' OR ls_msg-type = 'W'.
          DATA: lv_msg TYPE string.
          IF ls_msg-obj_type IS NOT INITIAL AND ls_msg-obj_name IS NOT INITIAL.
            lv_msg = |{ ls_msg-obj_type } { ls_msg-obj_name }: { ls_msg-text }|.
          ELSE.
            lv_msg = ls_msg-text.
          ENDIF.
          " Add exception text if available
          IF ls_msg-exception IS BOUND.
            lv_msg = |{ lv_msg }\nException: { ls_msg-exception->get_text( ) }|.
          ENDIF.
          IF lv_first = abap_false.
            rv_detail = lv_msg.
            lv_first = abap_true.
          ELSE.
            rv_detail = |{ rv_detail }&&&{ lv_msg }|.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " Replace marker with newline for display
      IF rv_detail IS NOT INITIAL.
        rv_detail = replace( val = rv_detail sub = '&&&' with = cl_abap_char_utilities=>newline ).
        rv_detail = |Error Details:{ cl_abap_char_utilities=>newline }{ rv_detail }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_object_lists.
    " Extract activated and failed objects from the log with full details
    DATA: lo_log TYPE REF TO zif_abapgit_log.
    DATA: lv_key TYPE string.

    CLEAR: rs_result-log_messages, rs_result-activated_objects, rs_result-failed_objects.

    lo_log = mo_repo->get_log( ).
    IF lo_log IS BOUND.
      DATA: lt_messages TYPE zif_abapgit_log=>ty_log_outs.
      DATA: ls_msg TYPE zif_abapgit_log=>ty_log_out.
      lt_messages = lo_log->get_messages( ).

      LOOP AT lt_messages INTO ls_msg.
        DATA: ls_object TYPE zif_abapgit_agent=>ty_object.
        ls_object-type = ls_msg-type.
        ls_object-id = ls_msg-id.
        ls_object-number = ls_msg-number.
        ls_object-text = ls_msg-text.
        ls_object-obj_type = ls_msg-obj_type.
        ls_object-obj_name = ls_msg-obj_name.

        " Exception is a REF, need to convert to string
        " Also append exception text to the message for better error reporting
        IF ls_msg-exception IS BOUND.
          DATA: lv_exc_text TYPE string.
          lv_exc_text = ls_msg-exception->get_text( ).
          ls_object-exception = lv_exc_text.
          " Append exception text to the main text if it's not already there
          IF lv_exc_text IS NOT INITIAL AND ls_msg-text NA lv_exc_text.
            ls_object-text = |{ ls_msg-text }\nException: { lv_exc_text }|.
          ENDIF.
        ENDIF.

        " Add all messages to log_messages table
        APPEND ls_object TO rs_result-log_messages.

        " Success messages (type 'S') - add to activated objects if unique
        IF ls_msg-type = 'S' AND ls_msg-obj_type IS NOT INITIAL AND ls_msg-obj_name IS NOT INITIAL.
          " Check for duplicates
          lv_key = |{ ls_msg-obj_type }{ ls_msg-obj_name }|.
          READ TABLE rs_result-activated_objects WITH KEY obj_type = ls_msg-obj_type
                                                        obj_name = ls_msg-obj_name
                                                  TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_object TO rs_result-activated_objects.
          ENDIF.
        ENDIF.

        " Error/Abort/Warning messages - add to failed objects
        IF ls_msg-type = 'E' OR ls_msg-type = 'A' OR ls_msg-type = 'W'.
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
