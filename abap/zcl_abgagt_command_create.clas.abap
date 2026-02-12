*"*"Use this source text module for your ABAP class
*"*"Local class for implementing ABAP Git Agent create command
CLASS zcl_abgagt_command_create DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
ENDCLASS.

CLASS zcl_abgagt_command_create IMPLEMENTATION.
  METHOD zif_abgagt_command~get_name.
    rv_name = 'CREATE'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute
    RAISING zcx_abapgit_exception.
    DATA ls_params TYPE string.
    DATA lv_package TYPE devclass.
    DATA li_repo TYPE REF TO zif_abapgit_repo.
    DATA ls_response TYPE string.

    IF is_param IS SUPPLIED.
      ls_params = is_param.
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

    li_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url            = ls_params-url
      iv_branch_name    = ls_params-branch
      iv_display_name   = ls_params-display_name
      iv_name           = ls_params-name
      iv_package        = lv_package
      iv_folder_logic   = ls_params-folder_logic ).

    ls_response = '{"success":"X",'.
    ls_response = ls_response && '"repo_key":"' && li_repo->get_key( ) && '",'.
    ls_response = ls_response && '"repo_name":"' && li_repo->get_name( ) && '",'.
    ls_response = ls_response && '"message":"Repository created successfully"}'.

    rv_result = ls_response.
  ENDMETHOD.
ENDCLASS.
