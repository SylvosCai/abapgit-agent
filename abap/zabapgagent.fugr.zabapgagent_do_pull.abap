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
  DATA: lv_key TYPE zif_abapgit_persistence=>ty_repo-key.
  DATA: lv_reason TYPE string.

  WRITE: / 'Starting pull for URL:', iv_url.
  WRITE: / 'Branch:', iv_branch.
  WRITE: / 'Job ID:', iv_job_id.
  WRITE: / 'Package:', iv_package.
  WRITE: / 'Create New:', iv_create_new.
  ULINE.

  IF iv_test_run = 'X'.
    WRITE: / 'TEST MODE - Skipping actual pull.'.
    lv_success = 'X'.
    lv_message = 'Test mode - no changes made'.
  ELSE.
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
      lv_key = li_repo->get_key( ).

      PERFORM pull_using_services USING lv_key CHANGING lv_success lv_message.

    ELSEIF iv_create_new = 'X' AND iv_package IS NOT INITIAL.
      " Case 2: Create new repository
      WRITE: / 'Creating new repository...'.

      PERFORM create_new_repo USING iv_url iv_branch iv_package iv_folder_logic
                            CHANGING li_repo lv_success lv_message.

      IF li_repo IS BOUND.
        lv_key = li_repo->get_key( ).
        PERFORM pull_using_services USING lv_key CHANGING lv_success lv_message.
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
*&      Form  PULL_USING_SERVICES
*&---------------------------------------------------------------------*
FORM pull_using_services USING iv_key TYPE zif_abapgit_persistence=>ty_repo-key
                     CHANGING cv_success TYPE char1
                              cv_message TYPE string.

  WRITE: / 'Pulling changes using services...'.

  TRY.
      " Use non-GUI pull method
      zcl_abapgit_services_git=>pull( iv_key = iv_key ).

      cv_success = 'X'.
      cv_message = 'Pull completed successfully'.
      WRITE: / 'Pull completed.'.

    CATCH zcx_abapgit_exception INTO DATA(lx_error).
      cv_success = ' '.
      cv_message = |abapGit error: { lx_error->get_text( ) }|.
      WRITE: / 'ERROR:', cv_message.
    CATCH cx_root INTO DATA(lx_error).
      cv_success = ' '.
      cv_message = |Error: { lx_error->get_text( ) }|.
      WRITE: / 'ERROR:', cv_message.
  ENDTRY.

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
                         cv_message TYPE string.

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
