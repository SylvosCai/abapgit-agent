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
        rv_result = '{"success":"","error":"Repository not found. Run create command or create in abapGit UI first."}'.
        RETURN.
    ENDTRY.

    IF li_repo IS NOT BOUND.
      rv_result = '{"success":"","error":"Repository not found"}'.
      RETURN.
    ENDIF.

    " Get package from repository
    lv_package = li_repo->get_package( ).

    " Build commit message if not provided
    IF ls_params-message IS INITIAL.
      lv_message = |feat: initial import from ABAP package { lv_package }|.
    ELSE.
      lv_message = ls_params-message.
    ENDIF.

    " Refresh repository to get latest local files
    li_repo->refresh( ).

    " Get local files count
    DATA(lo_files) = li_repo->get_files_local( ).
    DATA(lt_all_files) = lo_files->get_files( ).
    lv_files_staged = lines( lt_all_files ).

    " Check if there are files to import
    IF lv_files_staged = 0.
      rv_result = '{"success":"","error":"No objects found in package"}'.
      RETURN.
    ENDIF.

    " Stage and commit files
    DATA(lo_stage) = zcl_abapgit_stage=>new( ii_repo = li_repo ).

    LOOP AT lt_all_files ASSIGNING FIELD-SYMBOL(<ls_file>).
      lo_stage->add(
        iv_path     = <ls_file>-path
        iv_file     = <ls_file>-file ).
    ENDLOOP.

    " Get user details for committer
    DATA(lo_user) = zcl_abapgit_persist_factory=>get_user( )->read( iv_name = sy-uname ).
    DATA(lv_committer_name) = lo_user-name.
    DATA(lv_committer_email) = lo_user-email.

    " Commit and push
    zcl_abapgit_services_git=>commit(
      io_repo   = li_repo
      io_stage   = lo_stage
      iv_comment = lv_message
      iv_committer_name  = lv_committer_name
      iv_committer_email = lv_committer_email ).

    rv_result = '{"success":"X","files_staged":"' && lv_files_staged && '","message":"Objects imported successfully"}'.

  ENDMETHOD.

ENDCLASS.
