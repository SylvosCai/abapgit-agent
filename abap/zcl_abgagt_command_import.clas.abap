*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_import DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    INTERFACES zif_abgagt_progressable.

    METHODS constructor
      IMPORTING
        io_repo_srv TYPE REF TO zif_abapgit_repo_srv OPTIONAL
        io_user     TYPE REF TO zif_abapgit_persist_user OPTIONAL.

    TYPES: BEGIN OF ty_import_params,
             url TYPE string,
             branch TYPE string,
             message TYPE string,
             username TYPE string,
             password TYPE string,
           END OF ty_import_params.

  PRIVATE SECTION.
    DATA mo_repo_srv TYPE REF TO zif_abapgit_repo_srv.
    DATA mo_user TYPE REF TO zif_abapgit_persist_user.

    METHODS get_repo_srv
      RETURNING VALUE(ro_srv) TYPE REF TO zif_abapgit_repo_srv.
    METHODS get_user
      RETURNING VALUE(ro_user) TYPE REF TO zif_abapgit_persist_user.

ENDCLASS.

CLASS zcl_abgagt_command_import IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = 'IMPORT'.
  ENDMETHOD.

  METHOD constructor.
    mo_repo_srv = io_repo_srv.
    mo_user = io_user.
  ENDMETHOD.

  METHOD get_repo_srv.
    IF mo_repo_srv IS NOT BOUND.
      mo_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).
    ENDIF.
    ro_srv = mo_repo_srv.
  ENDMETHOD.

  METHOD get_user.
    IF mo_user IS NOT BOUND.
      mo_user = zcl_abapgit_persist_factory=>get_user( ).
    ENDIF.
    ro_user = mo_user.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_import_params,
          li_repo TYPE REF TO zif_abapgit_repo,
          li_repo_online TYPE REF TO zif_abapgit_repo_online,
          li_repo_srv TYPE REF TO zif_abapgit_repo_srv,
          li_user TYPE REF TO zif_abapgit_persist_user,
          lo_stage TYPE REF TO zcl_abapgit_stage,
          lt_files TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lv_package TYPE devclass,
          ls_comment TYPE zif_abapgit_git_definitions=>ty_comment.

    TRY.
        " Stage 1: Parse and validate parameters (10%)
        RAISE EVENT zif_abgagt_progressable~progress_update
          EXPORTING
            iv_stage    = 'PARSE_PARAMS'
            iv_message  = 'Parsing import parameters'
            iv_progress = 10.

        IF is_param IS SUPPLIED.
          " Handle both structure and JSON string input
          " When called from background job, is_param is a JSON string
          " When called directly (e.g., in unit tests), is_param is a structure
          DATA lv_param_type TYPE string.
          DESCRIBE FIELD is_param TYPE lv_param_type.

          IF lv_param_type = 'g' OR lv_param_type = 'C'.
            " String type - deserialize JSON

            DATA lv_json_string TYPE string.
            lv_json_string = is_param.
            /ui2/cl_json=>deserialize(
              EXPORTING
                json = lv_json_string
              CHANGING
                data = ls_params
            ).
          ELSE.
            " Structure type - use CORRESPONDING
            ls_params = CORRESPONDING #( is_param ).
          ENDIF.
        ENDIF.

        IF ls_params-url IS INITIAL.
          rv_result = '{"success":"","error":"URL is required"}'.
          RETURN.
        ENDIF.

        " Stage 2: Find repository (20%)
        RAISE EVENT zif_abgagt_progressable~progress_update
          EXPORTING
            iv_stage    = 'FIND_REPO'
            iv_message  = 'Finding repository'
            iv_progress = 20.

        li_repo_srv = get_repo_srv( ).
        li_repo_srv->get_repo_from_url(
          EXPORTING iv_url = ls_params-url
          IMPORTING ei_repo = li_repo ).

        IF li_repo IS NOT BOUND.
          rv_result = '{"success":"","error":"Repository not found"}'.
          RETURN.
        ENDIF.

        " Cast to online repo and configure credentials
        li_repo_online ?= li_repo.

        " Select branch before staging so the push targets the right ref
        IF ls_params-branch IS NOT INITIAL.
          DATA(lv_git_ref) = COND string(
            WHEN ls_params-branch CS 'refs/'                                     THEN ls_params-branch
            WHEN ls_params-branch(1) = 'v' AND ls_params-branch CN ' '          THEN |refs/tags/{ ls_params-branch }|
            ELSE |refs/heads/{ ls_params-branch }| ).
          li_repo_online->select_branch( lv_git_ref ).
        ENDIF.

        IF ls_params-username IS NOT INITIAL AND ls_params-password IS NOT INITIAL.
          li_user = get_user( ).
          li_user->set_repo_git_user_name(
            iv_url = ls_params-url
            iv_username = ls_params-username ).
          li_user->set_repo_login(
            iv_url = ls_params-url
            iv_login = ls_params-username ).
          zcl_abapgit_login_manager=>set_basic(
            iv_uri      = ls_params-url
            iv_username = ls_params-username
            iv_password = ls_params-password ).
        ENDIF.

        lv_package = li_repo->get_package( ).

        " Stage 3: Refresh repository (40%)
        RAISE EVENT zif_abgagt_progressable~progress_update
          EXPORTING
            iv_stage    = 'REFRESH_REPO'
            iv_message  = 'Refreshing repository'
            iv_progress = 40.

        li_repo->refresh( ).

        " Stage 4: Stage local files (60%)
        RAISE EVENT zif_abgagt_progressable~progress_update
          EXPORTING
            iv_stage    = 'STAGE_FILES'
            iv_message  = 'Staging local files'
            iv_progress = 60.

        lt_files = li_repo->get_files_local( ).
        DATA(lv_files_staged) = lines( lt_files ).

        IF lv_files_staged = 0.
          rv_result = '{"success":"","error":"No objects found in package"}'.
          RETURN.
        ENDIF.

        lo_stage = NEW #( ).
        DATA(lv_total_files) = lines( lt_files ).

        LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_file>).
          lo_stage->add(
            iv_path     = <ls_file>-file-path
            iv_filename = <ls_file>-file-filename
            iv_data     = <ls_file>-file-data ).

          " Update progress during staging (60-70%)
          IF lv_total_files > 10.
            DATA(lv_index) = sy-tabix.
            IF lv_index MOD 10 = 0 OR lv_index = lv_total_files.
              DATA(lv_file_progress) = 60 + ( lv_index * 10 / lv_total_files ).
              RAISE EVENT zif_abgagt_progressable~progress_update
                EXPORTING
                  iv_stage    = 'STAGE_FILES'
                  iv_message  = |Staging files ({ lv_index } of { lv_total_files })|
                  iv_progress = lv_file_progress
                  iv_current  = lv_index
                  iv_total    = lv_total_files.
            ENDIF.
          ENDIF.
        ENDLOOP.

        " Stage 5: Prepare commit (75%)
        RAISE EVENT zif_abgagt_progressable~progress_update
          EXPORTING
            iv_stage    = 'PREPARE_COMMIT'
            iv_message  = 'Preparing commit'
            iv_progress = 75.

        li_user = get_user( ).
        DATA(lv_committer_name) = li_user->get_default_git_user_name( ).
        DATA(lv_committer_email) = li_user->get_default_git_user_email( ).

        IF ls_params-message IS NOT INITIAL.
          DATA(lv_message) = ls_params-message.
        ELSE.
          lv_message = |feat: initial import from ABAP package { lv_package }|.
        ENDIF.

        ls_comment-committer-name  = lv_committer_name.
        ls_comment-committer-email = lv_committer_email.
        ls_comment-comment         = lv_message.

        " Stage 6: Push to repository (90%)
        RAISE EVENT zif_abgagt_progressable~progress_update
          EXPORTING
            iv_stage    = 'PUSH'
            iv_message  = 'Pushing to repository'
            iv_progress = 90.

        li_repo_online->push(
          is_comment = ls_comment
          io_stage   = lo_stage ).

        COMMIT WORK.

        " Complete (100%)
        RAISE EVENT zif_abgagt_progressable~progress_update
          EXPORTING
            iv_stage    = 'COMPLETED'
            iv_message  = 'Import completed successfully'
            iv_progress = 100.

        DATA: BEGIN OF ls_result,
                success        TYPE string,
                files_staged   TYPE i,
                commit_message TYPE string,
              END OF ls_result.
        ls_result-success        = 'X'.
        ls_result-files_staged   = lv_files_staged.
        ls_result-commit_message = lv_message.
        /ui2/cl_json=>serialize(
          EXPORTING
            data        = ls_result
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          RECEIVING
            r_json      = rv_result ).

      CATCH zcx_abapgit_exception INTO DATA(lx_abapgit).
        DATA: BEGIN OF ls_error,
                success TYPE string,
                error   TYPE string,
              END OF ls_error.
        ls_error-error = lx_abapgit->get_text( ).
        /ui2/cl_json=>serialize(
          EXPORTING
            data        = ls_error
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          RECEIVING
            r_json      = rv_result ).
      CATCH cx_root INTO DATA(lx_other).
        DATA: BEGIN OF ls_error2,
                success TYPE string,
                error   TYPE string,
              END OF ls_error2.
        ls_error2-error = lx_other->get_text( ).
        /ui2/cl_json=>serialize(
          EXPORTING
            data        = ls_error2
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          RECEIVING
            r_json      = rv_result ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
