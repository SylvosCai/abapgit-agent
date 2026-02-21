*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_create DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    METHODS constructor
      IMPORTING
        io_repo_srv TYPE REF TO zif_abapgit_repo_srv OPTIONAL
        io_user     TYPE REF TO zif_abapgit_persist_user OPTIONAL.

    TYPES: BEGIN OF ty_create_params,
             url TYPE string,
             branch TYPE string,
             package TYPE string,
             display_name TYPE string,
             name TYPE string,
             folder_logic TYPE string,
             folder TYPE string,
             username TYPE string,
             password TYPE string,
           END OF ty_create_params.

  PRIVATE SECTION.
    DATA mo_repo_srv TYPE REF TO zif_abapgit_repo_srv.
    DATA mo_user TYPE REF TO zif_abapgit_persist_user.

    METHODS get_repo_srv
      RETURNING VALUE(ro_srv) TYPE REF TO zif_abapgit_repo_srv.
    METHODS get_user
      RETURNING VALUE(ro_user) TYPE REF TO zif_abapgit_persist_user.

ENDCLASS.

CLASS zcl_abgagt_command_create IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = 'CREATE'.
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

    " Use injected dependencies for credentials
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

    TRY.
        li_repo = get_repo_srv( )->new_online(
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

    " Set starting folder if provided
    IF ls_params-folder IS NOT INITIAL.
      DATA(lo_dot) = li_repo->get_dot_abapgit( ).
      lo_dot->set_starting_folder( ls_params-folder ).
      li_repo->set_dot_abapgit( lo_dot ).
      COMMIT WORK AND WAIT.
    ENDIF.

    DATA lv_response TYPE string.
    lv_response = '{"success":"X",'.
    lv_response = lv_response && '"repo_key":"' && li_repo->get_key( ) && '",'.
    lv_response = lv_response && '"repo_name":"' && li_repo->get_name( ) && '",'.
    lv_response = lv_response && '"message":"Repository created successfully"}'.

    rv_result = lv_response.
  ENDMETHOD.

ENDCLASS.
