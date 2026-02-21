*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_import DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    METHODS constructor
      IMPORTING
        io_repo_srv TYPE REF TO zif_abapgit_repo_srv OPTIONAL
        io_user     TYPE REF TO zif_abapgit_persist_user OPTIONAL.

    TYPES: BEGIN OF ty_import_params,
             url TYPE string,
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
          lo_stage TYPE REF TO zcl_abapgit_stage,
          lt_files TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lv_package TYPE devclass,
          lv_message TYPE string,
          lv_files_staged TYPE i.

    " Use injected dependencies
    DATA: li_user TYPE REF TO zif_abapgit_persist_user,
          lv_committer_name TYPE string,
          lv_committer_email TYPE string.

    TRY.
        IF is_param IS SUPPLIED.
          ls_params = CORRESPONDING #( is_param ).
        ENDIF.

        " Validate URL
        IF ls_params-url IS INITIAL.
          rv_result = '{"success":"","error":"URL is required"}'.
          RETURN.
        ENDIF.

        " Configure credentials if provided
        IF ls_params-username IS NOT INITIAL AND ls_params-password IS NOT INITIAL.
          get_user( )->set_repo_git_user_name(
            iv_url = ls_params-url iv_username = ls_params-username ).
          get_user( )->set_repo_login(
            iv_url = ls_params-url iv_login = ls_params-username ).
          zcl_abapgit_login_manager=>set_basic(
            iv_uri      = ls_params-url
            iv_username = ls_params-username
            iv_password = ls_params-password ).
        ENDIF.

        " Find repository by URL - use injected dependency
        get_repo_srv( )->get_repo_from_url(
          EXPORTING iv_url = ls_params-url
          IMPORTING ei_repo = li_repo ).

        IF li_repo IS NOT BOUND.
          rv_result = '{"success":"","error":"Repository not found"}'.
          RETURN.
        ENDIF.

        " Cast to online repo for push operations
        li_repo_online ?= li_repo.

        " Get package from repository
        lv_package = li_repo->get_package( ).

        " Build commit message if not provided
        IF ls_params-message IS INITIAL.
          lv_message = |feat: initial import from ABAP package { lv_package }|.
        ELSE.
          lv_message = ls_params-message.
        ENDIF.

        " Refresh repository
        li_repo->refresh( ).

        " Get local files
        lt_files = li_repo->get_files_local( ).

        " Count files
        lv_files_staged = lines( lt_files ).

        " Check if there are files to import
        IF lv_files_staged = 0.
          rv_result = '{"success":"","error":"No objects found in package"}'.
          RETURN.
        ENDIF.

        " Create stage
        CREATE OBJECT lo_stage.

        " Add all files to stage
        LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_file>).
          lo_stage->add(
            iv_path     = <ls_file>-file-path
            iv_filename = <ls_file>-file-filename
            iv_data     = <ls_file>-file-data ).
        ENDLOOP.

        " Get user details for committer - use injected dependency
        li_user = get_user( ).
        lv_committer_name = li_user->get_default_git_user_name( ).
        lv_committer_email = li_user->get_default_git_user_email( ).

        " Prepare commit
        DATA: ls_comment TYPE zif_abapgit_git_definitions=>ty_comment.
        ls_comment-committer-name  = lv_committer_name.
        ls_comment-committer-email = lv_committer_email.
        ls_comment-comment         = lv_message.

        " Commit and push
        li_repo_online->push(
          is_comment = ls_comment
          io_stage   = lo_stage ).

        COMMIT WORK.

        rv_result = '{"success":"X","files_staged":"' && lv_files_staged && '","commit_message":"' && lv_message && '"}'.

      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        rv_result = '{"success":"","error":"' && lx_error->get_text( ) && '"}'.
      CATCH cx_root INTO DATA(lx_other).
        rv_result = '{"success":"","error":"' && lx_other->get_text( ) && '"}'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
