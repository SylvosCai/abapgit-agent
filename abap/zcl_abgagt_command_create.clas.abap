*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_create DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_create_params,
             url TYPE string,
             branch TYPE string,
             package TYPE devclass,
             display_name TYPE string,
             name TYPE string,
             folder_logic TYPE string DEFAULT 'PREFIX',
             username TYPE string,
             password TYPE string,
           END OF ty_create_params.

ENDCLASS.

CLASS zcl_abgagt_command_create IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_create.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_create_params,
          li_repo TYPE REF TO zif_abapgit_repo,
          lx_error TYPE REF TO zcx_abapgit_exception.

    " Parse parameters from is_param
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Validate required parameters
    IF ls_params-url IS INITIAL.
      rv_result = '{"success":"","error":"URL is required"}'.
      RETURN.
    ENDIF.

    IF ls_params-package IS INITIAL.
      rv_result = '{"success":"","error":"Package is required"}'.
      RETURN.
    ENDIF.

    " Configure credentials if provided
    IF ls_params-username IS NOT INITIAL AND ls_params-password IS NOT INITIAL.
      zcl_abapgit_persist_factory=>get_user( )->set_repo_git_user_name(
        iv_url = ls_params-url iv_username = ls_params-username ).
      zcl_abapgit_persist_factory=>get_user( )->set_repo_login(
        iv_url = ls_params-url iv_login = ls_params-username ).
      zcl_abapgit_login_manager=>set_basic(
        iv_uri      = ls_params-url
        iv_username = ls_params-username
        iv_password = ls_params-password ).
    ENDIF.

    " Create online repository using abapGit API
    TRY.
        DATA: ls_repo TYPE REF TO zif_abapgit_repo_online.

        ls_repo ?= zcl_abapgit_repo_srv=>get_instance( )->new_online(
          iv_url            = ls_params-url
          iv_branch_name    = ls_params-branch
          iv_display_name   = ls_params-display_name
          iv_name           = ls_params-name
          iv_package        = ls_params-package
          iv_folder_logic   = ls_params-folder_logic ).

        " Build response
        DATA: BEGIN OF ls_response,
                success TYPE string,
                repo_key TYPE string,
                repo_name TYPE string,
                message TYPE string,
              END OF ls_response.

        ls_response-success = 'X'.
        ls_response-repo_key = ls_repo->get_key( ).
        ls_response-repo_name = ls_repo->get_name( ).
        ls_response-message = 'Repository created successfully'.

        rv_result = /ui2/cl_json=>serialize( data = ls_response ).

      CATCH zcx_abapgit_exception INTO lx_error.
        DATA: BEGIN OF ls_error_resp,
                success TYPE string,
                error TYPE string,
              END OF ls_error_resp.

        ls_error_resp-success = ''.
        ls_error_resp-error = lx_error->get_text( ).
        rv_result = /ui2/cl_json=>serialize( data = ls_error_resp ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
