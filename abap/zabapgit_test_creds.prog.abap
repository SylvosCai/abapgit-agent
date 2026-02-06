*&---------------------------------------------------------------------*
*& Report  ZABAPGIT_TEST_CREDS
*&---------------------------------------------------------------------*
*& Test git credentials for abapGit
*&---------------------------------------------------------------------*
REPORT zabapgit_test_creds.

PARAMETERS:
  p_url     TYPE string LOWER CASE DEFAULT 'https://github.tools.sap',
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
  DATA: lv_auth TYPE string.

  " Create Basic Auth header
  lv_auth = p_user && ':' && p_pass.
  DATA(lv_encoded) = cl_http_utility=>encode_base64( lv_auth ).

  cl_http_client=>create_by_url(
    EXPORTING
      url    = p_url
    IMPORTING
      client = lo_http_client ).

  lo_http_client->request->set_method( 'GET' ).
  lo_http_client->request->set_header_field( name = 'Authorization' value = 'Basic ' && lv_encoded ).

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
    WRITE: / 'SUCCESS: Server accessible!' COLOR COL_POSITIVE.
  ELSEIF lv_code = 401.
    WRITE: / 'FAILED: Invalid credentials (HTTP 401)' COLOR COL_NEGATIVE.
  ELSEIF lv_code = 403.
    WRITE: / 'FAILED: Access forbidden (HTTP 403)' COLOR COL_NEGATIVE.
  ELSEIF lv_code = 404.
    WRITE: / 'FAILED: Not found (HTTP 404)' COLOR COL_NEGATIVE.
  ELSE.
    WRITE: / 'Status:', lv_code COLOR COL_GROUP.
  ENDIF.

END-OF-SELECTION.
