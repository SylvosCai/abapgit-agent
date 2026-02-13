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
          lv_package TYPE devclass,
          lv_message TYPE string,
          lv_files_staged TYPE i,
          lv_commit_sha TYPE string.

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
        li_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_url = ls_params-url ).
      CATCH zcx_abapgit_exception.
        rv_result = '{"success":"","error":"Repository not found. Run create command or create in abapGit UI first."}'.
        RETURN.
    ENDTRY.

    " Get package from repository
    lv_package = li_repo->get_package( ).

    " Build commit message if not provided
    IF ls_params-message IS INITIAL.
      lv_message = |feat: initial import from ABAP package { lv_package }|.
    ELSE.
      lv_message = ls_params-message.
    ENDIF.

    " Perform the import: stage, commit, and push
    TRY.
        " Refresh repository to get latest local files
        li_repo->refresh( ).

        " Get local files
        DATA(lo_files) = li_repo->get_files_local( ).

        " Check if there are files to import
        IF lo_files->is_empty( ) = abap_true.
          rv_result = '{"success":"","error":"No objects found in package"}'.
          RETURN.
        ENDIF.

        " Create stage
        DATA(lo_stage) = zcl_abapgit_stage=>new( ).

        " Add all files to stage
        DATA(lt_files) = lo_files->get_files( ).
        LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_file>).
          lo_stage->add(
            iv_path     = <ls_file>-path
            iv_file     = <ls_file>-file
            iv_obj_type = <ls_file>-obj_type
            iv_obj_name = <ls_file>-obj_name ).
        ENDLOOP.

        lv_files_staged = lines( lt_files ).

        " Get user details for committer
        DATA(lo_user) = zcl_abapgit_persist_factory=>get_user( )->read( iv_name = sy-uname ).
        DATA(lv_committer_name) = lo_user-name.
        DATA(lv_committer_email) = lo_user-email.

        " Set committer info and commit
        zcl_abapgit_services_git=>commit(
          io_repo   = li_repo
          io_stage   = lo_stage
          iv_comment = lv_message
          iv_committer_name  = lv_committer_name
          iv_committer_email = lv_committer_email ).

        " Get commit SHA
        lv_commit_sha = li_repo->get_current_remote_commit( ).

        rv_result = '{"success":"X","files_staged":"' && lv_files_staged && '","commit_sha":"' && lv_commit_sha && '","message":"Objects imported successfully"}'.

      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        rv_result = '{"success":"","error":"' && lx_error->get_text( ) && '"}'.
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
