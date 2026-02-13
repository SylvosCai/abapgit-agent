*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_import DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_import_params,
             url TYPE string,
             message TYPE string,
           END OF ty_import_params.

ENDCLASS.

CLASS zcl_abgagt_command_import IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = 'IMPORT'.
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

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Validate URL
    IF ls_params-url IS INITIAL.
      rv_result = '{"success":"","error":"URL is required"}'.
      RETURN.
    ENDIF.

    " Find repository by URL
    TRY.
        zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
          EXPORTING iv_url = ls_params-url
          IMPORTING ei_repo = li_repo ).
      CATCH zcx_abapgit_exception.
        rv_result = '{"success":"","error":"Repository not found"}'.
        RETURN.
    ENDTRY.

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

    " Get user details for committer
    DATA(li_user) = zcl_abapgit_persist_factory=>get_user( ).
    DATA(lv_committer_name) = li_user->get_default_git_user_name( ).
    DATA(lv_committer_email) = li_user->get_default_git_user_email( ).

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

    rv_result = '{"success":"X","files_staged":"' && lv_files_staged && '","message":"Objects imported successfully"}'.

  ENDMETHOD.

ENDCLASS.
