*&---------------------------------------------------------------------*
*& Report  ZABAPGIT_TEST_CREDS
*&---------------------------------------------------------------------*
*& Test git credentials for abapGit
*&---------------------------------------------------------------------*
REPORT zabapgit_test_creds.

PARAMETERS:
  p_url     TYPE string LOWER CASE DEFAULT 'https://github.tools.sap/I045696/abap-ai-bridge.git',
  p_user    TYPE string,
  p_pass    TYPE string.

START-OF-SELECTION.
  WRITE: / 'Testing git credentials'.
  WRITE: / 'URL:', p_url.
  WRITE: / 'User:', p_user.
  ULINE.

  DATA: lo_http_client TYPE REF TO if_http_client.
  DATA: lv_code TYPE i.
  DATA: lv_reason TYPE string.

  cl_http_client=>create_by_destination( EXPORTING destination = 'NONE' IMPORTING client = lo_http_client ).

  lo_http_client->request->set_method( 'GET' ).
  lo_http_client->request->set_uri_path( '/I045696/abap-ai-bridge.git/info/refs' ).
  lo_http_client->set_authentication( iv_user = p_user iv_password = p_pass ).

  WRITE: / 'Sending request...'.

  lo_http_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state = 2 ).

  IF sy-subrc <> 0.
    WRITE: / 'Communication error!' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  lo_http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state = 2 ).

  IF sy-subrc <> 0.
    WRITE: / 'Receive error!' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  DATA(lo_response) = lo_http_client->response.
  lo_response->get_status( IMPORTING code = lv_code reason = lv_reason ).

  WRITE: / 'HTTP Status:', lv_code, lv_reason.

  IF lv_code = 200.
    WRITE: / 'SUCCESS: Credentials are valid!' COLOR COL_POSITIVE.
  ELSEIF lv_code = 401.
    WRITE: / 'FAILED: Invalid credentials (HTTP 401)' COLOR COL_NEGATIVE.
  ELSEIF lv_code = 403.
    WRITE: / 'FAILED: Access forbidden (HTTP 403)' COLOR COL_NEGATIVE.
    WRITE: / 'Note: For github.tools.sap, use a Personal Access Token instead of password'.
  ELSE.
    WRITE: / 'FAILED: Unexpected status' COLOR COL_NEGATIVE.
  ENDIF.

END-OF-SELECTION.
