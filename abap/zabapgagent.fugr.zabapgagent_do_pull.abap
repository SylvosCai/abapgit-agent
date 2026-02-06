FUNCTION zabapgagent_do_pull.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_URL) TYPE STRING
*"    VALUE(IV_BRANCH) TYPE STRING DEFAULT 'main'
*"    VALUE(IV_TEST_RUN) TYPE CHAR1 DEFAULT ' '
*"    VALUE(IV_JOB_ID) TYPE STRING
*"    VALUE(IV_PACKAGE) TYPE DEVCLASS DEFAULT ' '
*"    VALUE(IV_FOLDER_LOGIC) TYPE STRING DEFAULT 'PREFIX'
*"    VALUE(IV_CREATE_NEW) TYPE CHAR1 DEFAULT ' '
*"    VALUE(IV_USERNAME) TYPE STRING OPTIONAL
*"    VALUE(IV_PASSWORD) TYPE STRING OPTIONAL
*"  RAISING
*"    ERROR
*"    ZCX_ABAPGIT_EXCEPTION
*"----------------------------------------------------------------------
*  Case 1: Repository already exists in abapGit
*  Case 2: Create new repository (requires IV_PACKAGE)
*"----------------------------------------------------------------------

  DATA: lv_success TYPE char1.
  DATA: lv_message TYPE string.
  DATA: li_repo TYPE REF TO zif_abapgit_repo.
  DATA: lv_reason TYPE string.
  DATA: lv_login TYPE string.

  WRITE: / 'Starting pull for URL:', iv_url.
  WRITE: / 'Branch:', iv_branch.
  WRITE: / 'Job ID:', iv_job_id.
  WRITE: / 'Package:', iv_package.
  WRITE: / 'Create New:', iv_create_new.
  IF iv_username IS NOT INITIAL.
    WRITE: / 'Username provided: Yes'.
  ENDIF.
  ULINE.

  IF iv_test_run = 'X'.
    WRITE: / 'TEST MODE - Skipping actual pull.'.
    lv_success = 'X'.
    lv_message = 'Test mode - no changes made'.
  ELSE.
    " Set global credentials for password popup
    gv_auth_user = iv_username.
    gv_auth_pass = iv_password.

    " Configure credentials if provided
    IF iv_username IS NOT INITIAL AND iv_password IS NOT INITIAL.
      WRITE: / 'Configuring git credentials...'.
      zcl_abapgit_persist_factory=>get_user( )->set_repo_git_user_name(
        iv_url = iv_url iv_username = iv_username ).
      zcl_abapgit_persist_factory=>get_user( )->set_repo_login(
        iv_url = iv_url iv_login = iv_username ).
    ENDIF.

    " Check if repo already exists
    WRITE: / 'Checking if repository exists...'.

    zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
      EXPORTING
        iv_url    = iv_url
      IMPORTING
        ei_repo   = li_repo
        ev_reason = lv_reason ).

    IF li_repo IS BOUND.
      " Case 1: Repo exists - pull changes
      WRITE: / 'Repository found.'.
      WRITE: / 'Package:', li_repo->get_package( ).

      PERFORM pull_repo USING li_repo iv_url CHANGING lv_success lv_message.

    ELSEIF iv_create_new = 'X' AND iv_package IS NOT INITIAL.
      " Case 2: Create new repository
      WRITE: / 'Creating new repository...'.

      TRY.
          PERFORM create_new_repo USING iv_url iv_branch iv_package iv_folder_logic
                                CHANGING li_repo lv_success lv_message.
        CATCH zcx_abapgit_exception INTO DATA(lx_new).
          lv_success = ' '.
          lv_message = |Failed to create repo: { lx_new->get_text( ) }|.
          WRITE: / 'ERROR:', lv_message.
      ENDTRY.

      IF li_repo IS BOUND.
        PERFORM pull_repo USING li_repo iv_url CHANGING lv_success lv_message.
      ENDIF.

    ELSE.
      " Repo not found and create_new is not set
      lv_success = ' '.
      lv_message = |Repository not found: { iv_url }| &
                 |. Set IV_CREATE_NEW = 'X' and provide IV_PACKAGE to create new repo.|.
      WRITE: / 'ERROR:', lv_message.
    ENDIF.
  ENDIF.

  ULINE.
  WRITE: / 'Result:', lv_message.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  PULL_REPO
*&---------------------------------------------------------------------*
FORM pull_repo USING li_repo TYPE REF TO zif_abapgit_repo
                    iv_url TYPE string
            CHANGING cv_success TYPE char1
                     cv_message TYPE string.

  DATA: ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
  DATA: lo_settings TYPE REF TO zcl_abapgit_settings.
  DATA: lv_activation_setting TYPE zif_abapgit_persist_user=>ty_s_user_settings-activate_wo_popup.
  DATA: lv_login TYPE string.
  FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ls_checks-overwrite.

  WRITE: / 'Refreshing repository...'.
  li_repo->refresh( ).

  " Check if credentials are configured
  TRY.
      lv_login = zcl_abapgit_persist_factory=>get_user( )->get_repo_login( iv_url = iv_url ).
    CATCH zcx_abapgit_exception.
      lv_login = ''.
  ENDTRY.

  IF lv_login IS INITIAL.
    cv_success = ' '.
    cv_message = |Credentials not configured for { iv_url }| &
               |. Please provide IV_USERNAME and IV_PASSWORD parameters.|.
    WRITE: / 'ERROR:', cv_message.
    RETURN.
  ENDIF.

  WRITE: / 'Credentials found for repository.'.

  WRITE: / 'Getting deserialize checks...'.
  ls_checks = li_repo->deserialize_checks( ).

  " Set all overwrite decisions to YES (non-GUI mode)
  LOOP AT ls_checks-overwrite ASSIGNING <ls_overwrite>.
    <ls_overwrite>-decision = zif_abapgit_definitions=>c_yes.
  ENDLOOP.

  " Enable activate without popup
  lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
  lv_activation_setting = lo_settings->get_activate_wo_popup( ).
  lo_settings->set_activate_wo_popup( abap_true ).

  WRITE: / 'Pulling changes...'.

  " Create log and deserialize (non-GUI)
  li_repo->create_new_log( ).

  TRY.
      " Delete unnecessary objects
      zcl_abapgit_services_repo=>delete_unnecessary_objects(
        ii_repo   = li_repo
        is_checks = ls_checks
        ii_log    = li_repo->get_log( ) ).

      " Deserialize (pull)
      li_repo->deserialize(
        is_checks = ls_checks
        ii_log    = li_repo->get_log( ) ).

      cv_success = 'X'.
      cv_message = 'Pull completed successfully'.
      WRITE: / 'Pull completed.'.

    CATCH zcx_abapgit_exception INTO DATA(lx_git).
      cv_success = ' '.
      cv_message = |abapGit error: { lx_git->get_text( ) }|.
      WRITE: / 'ERROR:', cv_message.
    CATCH cx_root INTO DATA(lx_error).
      cv_success = ' '.
      cv_message = |Error: { lx_error->get_text( ) }|.
      WRITE: / 'ERROR:', cv_message.
  ENDTRY.

  " Restore setting
  lo_settings->set_activate_wo_popup( lv_activation_setting ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CREATE_NEW_REPO
*&---------------------------------------------------------------------*
FORM create_new_repo USING iv_url TYPE string
                          iv_branch TYPE string
                          iv_package TYPE devclass
                          iv_folder_logic TYPE string
                CHANGING ci_repo TYPE REF TO zif_abapgit_repo
                         cv_success TYPE char1
                         cv_message TYPE string
                RAISING zcx_abapgit_exception.

  DATA: li_repo TYPE REF TO zif_abapgit_repo.
  DATA: lv_folder_logic TYPE string.

  " Validate package
  WRITE: / 'Validating package:', iv_package.

  " Determine folder logic
  IF iv_folder_logic IS INITIAL OR iv_folder_logic = 'PREFIX'.
    lv_folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
  ELSEIF iv_folder_logic = 'FULL'.
    lv_folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full.
  ELSE.
    lv_folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
  ENDIF.

  " Create new online repository
  WRITE: / 'Creating online repository...'.
  WRITE: / 'URL:', iv_url.
  WRITE: / 'Branch:', iv_branch.
  WRITE: / 'Package:', iv_package.
  WRITE: / 'Folder Logic:', lv_folder_logic.

  li_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
    iv_url          = iv_url
    iv_branch_name  = iv_branch
    iv_package      = iv_package
    iv_folder_logic = lv_folder_logic ).

  WRITE: / 'Repository created successfully.'.
  WRITE: / 'Package:', li_repo->get_package( ).
  WRITE: / 'Key:', li_repo->get_key( ).

  cv_success = 'X'.
  cv_message = |Repository created: { iv_package }|.
  ci_repo = li_repo.

ENDFORM.
