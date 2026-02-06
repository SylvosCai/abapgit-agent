*&---------------------------------------------------------------------*
*& Report  ZABAPGIT_TEST_CREDS
*&---------------------------------------------------------------------*
*& Test git credentials for abapGit
*&---------------------------------------------------------------------*
REPORT zabapgit_test_creds.

PARAMETERS:
  p_url TYPE string LOWER CASE DEFAULT 'https://www.google.com'.

START-OF-SELECTION.
  WRITE: / 'Testing HTTP access'.
  WRITE: / 'URL:', p_url.
  ULINE.

  DATA: lo_http_client TYPE REF TO if_http_client.
  DATA: lv_code TYPE i.
  DATA: lv_reason TYPE string.

  cl_http_client=>create_by_url(
    EXPORTING
      url    = p_url
    IMPORTING
      client = lo_http_client ).

  lo_http_client->request->set_method( 'GET' ).

  WRITE: / 'Sending request...'.

  lo_http_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state = 2 ).

  IF sy-subrc <> 0.
    WRITE: / 'SEND ERROR: Communication failed!' COLOR COL_NEGATIVE.
    WRITE: / 'sy-subrc:', sy-subrc.
    RETURN.
  ENDIF.

  WRITE: / 'Send successful, waiting for response...'.

  lo_http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state = 2 ).

  IF sy-subrc <> 0.
    WRITE: / 'RECEIVE ERROR!' COLOR COL_NEGATIVE.
    WRITE: / 'This means SAP cannot connect to the URL.'.
    WRITE: / 'Check:'.
    WRITE: / '1. Firewall/proxy settings'.
    WRITE: / '2. SICF transaction - internet HTTP access may be blocked'.
    WRITE: / '3. SM59 - RFC destination for HTTP'.
    RETURN.
  ENDIF.

  DATA(lo_response) = lo_http_client->response.
  lo_response->get_status( IMPORTING code = lv_code reason = lv_reason ).

  WRITE: / 'HTTP Status:', lv_code, lv_reason.

  IF lv_code = 200.
    WRITE: / 'SUCCESS: URL is accessible!' COLOR COL_POSITIVE.
  ELSE.
    WRITE: / 'Response received but status is:', lv_code COLOR COL_GROUP.
  ENDIF.

END-OF-SELECTION.
