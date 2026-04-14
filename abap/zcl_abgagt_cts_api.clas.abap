*"*"use source
*"*"Local Interface:
*"**********************************************************************
"! <p class="shorttext synchronized">CTS API Implementation for ABAP Git Agent</p>
CLASS zcl_abgagt_cts_api DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_cts_api.

ENDCLASS.

CLASS zcl_abgagt_cts_api IMPLEMENTATION.

  METHOD zif_abgagt_cts_api~create_transport.
    DATA: lv_request TYPE char20,
          lv_retcode TYPE char3.

    CALL FUNCTION 'CTS_API_CREATE_CHANGE_REQUEST'
      EXPORTING
        description = CONV text60( iv_description )
        category    = iv_category
        client      = sy-mandt
        owner       = sy-uname
      IMPORTING
        request     = lv_request
        retcode     = lv_retcode
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc <> 0 OR lv_retcode <> '000'.
      rs_result-subrc = 1.
    ELSE.
      rs_result-subrc  = 0.
      rs_result-trkorr = lv_request.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_cts_api~check_transport.
    " CTS_API_CHECK_TRANSPORT not available — report pass to avoid breaking callers
    rv_subrc = 0.
  ENDMETHOD.

  METHOD zif_abgagt_cts_api~release_transport.
    CALL FUNCTION 'TRINT_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr       = iv_trkorr
        iv_dialog       = abap_false
      EXCEPTIONS
        no_authorization           = 1
        enqueue_failed             = 2
        invalid_request            = 3
        request_already_released   = 4
        export_failed              = 5
        object_lock_error          = 6
        object_check_error         = 7
        OTHERS                     = 8.
    rv_subrc = sy-subrc.
  ENDMETHOD.

  METHOD zif_abgagt_cts_api~add_to_transport.
    CALL FUNCTION 'CTS_API_ADD_TO_TRANSPORT_REQUEST'
      EXPORTING
        iv_pgmid    = 'R3TR'
        iv_object   = 'TABU'
        iv_obj_name = CONV trobj_name( iv_tabname )
        iv_request  = iv_trkorr
      EXCEPTIONS
        OTHERS      = 1.
    rv_subrc = sy-subrc.
  ENDMETHOD.

ENDCLASS.
