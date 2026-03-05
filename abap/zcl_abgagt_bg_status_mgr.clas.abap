CLASS zcl_abgagt_bg_status_mgr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_job_status_mgr.

  PRIVATE SECTION.
    METHODS get_memory_id
      IMPORTING
        iv_job_number TYPE btcjobcnt
      RETURNING
        VALUE(rv_id) TYPE char40.
ENDCLASS.

CLASS zcl_abgagt_bg_status_mgr IMPLEMENTATION.

  METHOD get_memory_id.
    " Create unique memory ID for job
    rv_id = |JOBSTATUS_{ iv_job_number }|.
  ENDMETHOD.

  METHOD zif_abgagt_job_status_mgr~update_status.
    " Store status in shared buffer
    DATA lv_memory_id TYPE char40.

    lv_memory_id = get_memory_id( is_status-job_number ).

    " Export to SAP shared buffer (cross-user memory)
    EXPORT status = is_status TO SHARED BUFFER indx(zz) ID lv_memory_id.
  ENDMETHOD.

  METHOD zif_abgagt_job_status_mgr~get_status.
    " Retrieve status from shared buffer
    DATA lv_memory_id TYPE char40.

    lv_memory_id = get_memory_id( iv_job_number ).

    " Import from SAP shared buffer
    IMPORT status = rs_status FROM SHARED BUFFER indx(zz) ID lv_memory_id.
    IF sy-subrc <> 0.
      CLEAR rs_status.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_job_status_mgr~delete_status.
    " Delete status from shared buffer
    DATA lv_memory_id TYPE char40.

    lv_memory_id = get_memory_id( iv_job_number ).

    " Free shared buffer
    DELETE FROM SHARED BUFFER indx(zz) ID lv_memory_id.
  ENDMETHOD.
ENDCLASS.
