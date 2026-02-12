*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_create DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_create_params,
             url TYPE string,
             branch TYPE string,
             package TYPE string,
             display_name TYPE string,
             name TYPE string,
             folder_logic TYPE string,
             username TYPE string,
             password TYPE string,
           END OF ty_create_params.

ENDCLASS.

CLASS zcl_abgagt_command_create IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = 'CREATE'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_create_params,
          lv_package TYPE devclass,
          li_repo TYPE REF TO zif_abapgit_repo.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-folder_logic IS INITIAL.
      ls_params-folder_logic = 'PREFIX'.
    ENDIF.

    IF ls_params-url IS INITIAL.
      rv_result = '{"success":"","error":"URL is required"}'.
      RETURN.
    ENDIF.

    IF ls_params-package IS INITIAL.
      rv_result = '{"success":"","error":"Package is required"}'.
      RETURN.
    ENDIF.

    lv_package = ls_params-package.

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

    TRY.
        li_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
          iv_url            = ls_params-url
          iv_branch_name    = ls_params-branch
          iv_display_name   = ls_params-display_name
          iv_name           = ls_params-name
          iv_package        = lv_package
          iv_folder_logic   = ls_params-folder_logic ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        rv_result = '{"success":"","error":"' && lx_error->get_text( ) && '"}'.
        RETURN.
    ENDTRY.

    DATA lv_response TYPE string.
    lv_response = '{"success":"X",'.
    lv_response = lv_response && '"repo_key":"' && li_repo->get_key( ) && '",'.
    lv_response = lv_response && '"repo_name":"' && li_repo->get_name( ) && '",'.
    lv_response = lv_response && '"message":"Repository created successfully"}'.

    rv_result = lv_response.
  ENDMETHOD.

ENDCLASS.
