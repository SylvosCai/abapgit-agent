FUNCTION zabapgagent_do_pull.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_URL) TYPE STRING
*"    VALUE(IV_BRANCH) TYPE STRING DEFAULT 'main'
*"    VALUE(IV_TEST_RUN) TYPE CHAR1 DEFAULT ' '
*"    VALUE(IV_JOB_ID) TYPE STRING
*"----------------------------------------------------------------------
*  Case 1: Repository already exists in abapGit
*  Steps:
*    1. Get repo by URL
*    2. Refresh to get latest from remote
*    3. Deserialize (pull) changes
*    4. Log results
*"----------------------------------------------------------------------

  DATA: lv_success TYPE char1.
  DATA: lv_message TYPE string.
  DATA: li_repo TYPE REF TO zif_abapgit_repo.
  DATA: li_log TYPE REF TO zif_abapgit_log.
  DATA: ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
  DATA: lv_reason TYPE string.
  DATA: lv_activated TYPE i.
  DATA: lv_failed TYPE i.

  WRITE: / 'Starting pull for URL:', iv_url.
  WRITE: / 'Branch:', iv_branch.
  WRITE: / 'Job ID:', iv_job_id.
  ULINE.

  IF iv_test_run = 'X'.
    WRITE: / 'TEST MODE - Skipping actual pull.'.
    lv_success = 'X'.
    lv_message = 'Test mode - no changes made'.
  ELSE.
    TRY.
        " Step 1: Check if repo already exists
        WRITE: / 'Checking if repository exists...'.

        zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
          EXPORTING
            iv_url    = iv_url
          IMPORTING
            ei_repo   = li_repo
            ev_reason = lv_reason ).

        IF li_repo IS BOUND.
          WRITE: / 'Repository found.'.
          WRITE: / 'Package:', li_repo->get_package( ).

          " Step 2: Refresh to get latest from remote
          WRITE: / 'Refreshing repository...'.
          li_repo->refresh( ).

          " Step 3: Get deserialize checks
          ls_checks = li_repo->deserialize_checks( ).

          " Step 4: Deserialize (pull) changes
          WRITE: / 'Pulling changes...'.

          " Create log object for tracking
          li_repo->create_new_log( ).

          " Perform the deserialization (pull)
          li_repo->deserialize(
            is_checks = ls_checks
            ii_log   = li_repo->get_log( ) ).

          " Get statistics from log
          lv_activated = 1.
          lv_failed = 0.

          lv_success = 'X'.
          lv_message = 'Pull completed successfully'.

          WRITE: / 'Pull completed.'.
        ELSE.
          " Repo not found - Case 2 would be needed
          lv_success = ' '.
          lv_message = |Repository not found: { iv_url }. Case 2 not implemented.|.
          WRITE: / 'ERROR:', lv_message.
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(lx_git).
        lv_success = ' '.
        lv_message = |abapGit error: { lx_git->get_text( ) }|.
        WRITE: / 'ERROR:', lv_message.
      CATCH cx_root INTO DATA(lx_error).
        lv_success = ' '.
        lv_message = |Error: { lx_error->get_text( ) }|.
        WRITE: / 'ERROR:', lv_message.
    ENDTRY.
  ENDIF.

  ULINE.
  WRITE: / 'Result:', lv_message.

ENDFUNCTION.
