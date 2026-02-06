*&---------------------------------------------------------------------*
*& Report  ZABAPGAGENT_PULL_JOB
*&---------------------------------------------------------------------*
*& Standalone program to pull and activate ABAP code from git
*&---------------------------------------------------------------------*
REPORT zabapgagent_pull_job.

PARAMETERS:
  p_url      TYPE string LOWER CASE OBLIGATORY,
  p_branch   TYPE string LOWER CASE DEFAULT 'main',
  p_test     TYPE char1 AS CHECKBOX DEFAULT ' ',
  p_job_id   TYPE string,
  p_pkg      TYPE devclass,
  p_folder   TYPE string DEFAULT 'PREFIX',
  p_new      TYPE char1 AS CHECKBOX DEFAULT ' ',
  p_username TYPE string,
  p_password TYPE string.

START-OF-SELECTION.

  WRITE: / 'ABAP Git Agent - Pull Job'.
  WRITE: / 'URL:', p_url.
  WRITE: / 'Branch:', p_branch.
  WRITE: / 'Job ID:', p_job_id.
  WRITE: / 'Package:', p_pkg.
  WRITE: / 'Folder Logic:', p_folder.
  WRITE: / 'Create New:', p_new.
  IF p_username IS NOT INITIAL.
    WRITE: / 'Username provided: Yes'.
  ENDIF.
  IF p_test = 'X'.
    WRITE: / 'TEST MODE - No changes will be made.'.
  ENDIF.
  ULINE.

  CALL FUNCTION 'ZABAPGAGENT_DO_PULL'
    EXPORTING
      iv_url         = p_url
      iv_branch      = p_branch
      iv_test_run    = p_test
      iv_job_id      = p_job_id
      iv_package     = p_pkg
      iv_folder_logic = p_folder
      iv_create_new  = p_new
      iv_username    = p_username
      iv_password    = p_password.

END-OF-SELECTION.
