*&---------------------------------------------------------------------*
*& Report  ZABAPGIT_TEST_CREDS
*&---------------------------------------------------------------------*
*& Test git credentials for abapGit
*&---------------------------------------------------------------------*
REPORT zabapgit_test_creds.

PARAMETERS:
  p_url TYPE string LOWER CASE DEFAULT 'https://www.google.com',
  p_user TYPE string.

" Note: Password must be passed via memory ID to preserve case
" Example: SET PARAMETER ID 'ZGIT_PASS' FIELD 'MySecretPassword'.
"          Then execute SE38 -> ZABAPGIT_TEST_CREDS

START-OF-SELECTION.
  WRITE: / 'Testing HTTP access using abapGit HTTP approach'.
  WRITE: / 'URL:', p_url.
  ULINE.

  " Get password from memory ID (preserves case)
  DATA: lv_pass TYPE string.
  IMPORT lv_pass FROM MEMORY ID 'ZGIT_PASS'.

  IF lv_pass IS INITIAL.
    WRITE: / 'ERROR: Password not set.' COLOR COL_NEGATIVE.
    WRITE: / 'To set password, run this ABAP in SE80:' COLOR COL_GROUP.
    WRITE: / '  DATA: lv_pass TYPE string.'.
    WRITE: / '  lv_pass = |your_password|.'.
    WRITE: / '  EXPORT lv_pass TO MEMORY ID ''ZGIT_PASS''.'.
    WRITE: / '  SUBMIT zabapgit_test_creds AND RETURN.'.
    RETURN.
  ENDIF.

  WRITE: / 'Password: [PROTECTED]'.

  DATA: lo_http_client TYPE REF TO if_http_client.
  DATA: lv_code TYPE i.
  DATA: lv_reason TYPE string.

  " Create HTTP client using abapGit's get_http_client method
  " This handles SSL, proxy properly
  TRY.
      DATA(lv_host) = p_url.
      " Remove path from URL to get just the host
      FIND REGEX '^https?://([^/]+)' IN p_url SUBMATCHES lv_host.
      IF lv_host IS INITIAL.
        lv_host = p_url.
      ENDIF.

      " Create HTTP client - let abapGit handle SSL/proxy setup
      cl_http_client=>create_by_url(
        EXPORTING
          url    = lv_host
        IMPORTING
          client = lo_http_client ).

      " Build request URI from original URL
      DATA(lv_path) = p_url.
      IF lv_path CP 'http://*' OR lv_path CP 'https://*'.
        SHIFT lv_path LEFT BY strlen( lv_host ) PLACES.
      ENDIF.
      IF lv_path IS INITIAL.
        lv_path = '/'.
      ENDIF.

      " Disable logon popup (like abapGit does)
      lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

      " Set request method using header field (like abapGit does)
      lo_http_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).

      " Set user agent (like abapGit does)
      lo_http_client->request->set_header_field(
        name  = 'user-agent'
        value = 'git/2.0 (abapGit)' ).

      " Set request URI
      lo_http_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_path ).

      " Set HTTP version
      lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

      " Set Basic Auth if credentials provided
      IF p_user IS NOT INITIAL.
        " Use the standard authenticate method
        lo_http_client->authenticate(
          username = p_user
          password = p_pass ).
      ENDIF.

      WRITE: / 'Sending request to:', lv_path.

      lo_http_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state = 2
          http_processing_failed = 3
          http_invalid_timeout = 4 ).

      IF sy-subrc <> 0.
        WRITE: / 'SEND ERROR!' COLOR COL_NEGATIVE.
        DATA: lv_err_code TYPE i.
        DATA: lv_err_msg TYPE string.
        lo_http_client->get_last_error(
          IMPORTING code = lv_err_code message = lv_err_msg ).
        WRITE: / 'Error code:', lv_err_code.
        WRITE: / 'Error message:', lv_err_msg.
        WRITE: / 'Check SMICM for HTTP trace logs'.
        RETURN.
      ENDIF.

      WRITE: / 'Send successful, waiting for response...'.

      lo_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state = 2
          http_processing_failed = 3 ).

      IF sy-subrc <> 0.
        WRITE: / 'RECEIVE ERROR!' COLOR COL_NEGATIVE.
        lo_http_client->get_last_error(
          IMPORTING code = lv_err_code message = lv_err_msg ).
        WRITE: / 'Error code:', lv_err_code.
        WRITE: / 'Error message:', lv_err_msg.
        WRITE: / 'This means SAP cannot connect to the URL.'.
        WRITE: / 'Check:'.
        WRITE: / '1. Firewall/proxy settings'.
        WRITE: / '2. SICF transaction - internet HTTP access may be blocked'.
        WRITE: / '3. SM59 - RFC destination for HTTP'.
        WRITE: / '4. STRUST - SSL certificates for HTTPS'.
        RETURN.
      ENDIF.

      DATA(lo_response) = lo_http_client->response.
      lo_response->get_status( IMPORTING code = lv_code reason = lv_reason ).

      WRITE: / 'HTTP Status:', lv_code, lv_reason.

      IF lv_code = 200.
        WRITE: / 'SUCCESS: URL is accessible!' COLOR COL_POSITIVE.
      ELSEIF lv_code = 401.
        WRITE: / 'Authentication required - check credentials' COLOR COL_NEGATIVE.
      ELSEIF lv_code = 403.
        WRITE: / 'Forbidden - check permissions' COLOR COL_NEGATIVE.
      ELSEIF lv_code = 404.
        WRITE: / 'Not found - check URL' COLOR COL_NEGATIVE.
      ELSE.
        WRITE: / 'Response received but status is:', lv_code COLOR COL_GROUP.
      ENDIF.

      " Show response content (first 500 chars)
      DATA(lv_content) = lo_response->get_cdata( ).
      IF lv_content IS NOT INITIAL.
        ULINE.
        WRITE: / 'Response content (first 500 chars):'.
        WRITE: / lv_content(500).
      ENDIF.

      lo_http_client->close( ).

    CATCH cx_root INTO DATA(lo_exception).
      WRITE: / 'EXCEPTION:' COLOR COL_NEGATIVE.
      WRITE: / lo_exception->get_text( ).
  ENDTRY.

END-OF-SELECTION.
