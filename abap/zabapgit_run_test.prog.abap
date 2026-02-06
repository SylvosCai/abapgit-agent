*&---------------------------------------------------------------------*
*& Report  ZABAPGIT_RUN_TEST
*&---------------------------------------------------------------------*
*& Helper to run credential test with password preserving case
*&---------------------------------------------------------------------*
REPORT zabapgit_run_test.

PARAMETERS:
  p_url TYPE string LOWER CASE DEFAULT 'https://www.google.com',
  p_user TYPE string,
  p_pass TYPE string LOWER CASE.

START-OF-SELECTION.

  " Export password to memory ID (preserves case)
  EXPORT p_pass TO MEMORY ID 'ZGIT_PASS'.

  WRITE: / 'Password exported to memory.'.
  WRITE: / 'Submitting test report...'.
  ULINE.

  " Submit the test report
  SUBMIT zabapgit_test_creds WITH p_url = p_url
                       WITH p_user = p_user
                       AND RETURN.

  " Clear password from memory
  FREE MEMORY ID 'ZGIT_PASS'.
  WRITE: / 'Memory cleared.'.

END-OF-SELECTION.
