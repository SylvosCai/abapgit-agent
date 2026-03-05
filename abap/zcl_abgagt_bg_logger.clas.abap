CLASS zcl_abgagt_bg_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_bg_logger.

    METHODS constructor
      IMPORTING
        io_status_mgr TYPE REF TO zif_abgagt_job_status_mgr
        iv_job_number TYPE btcjobcnt.

  PRIVATE SECTION.
    DATA mo_status_mgr TYPE REF TO zif_abgagt_job_status_mgr.
    DATA mv_job_number TYPE btcjobcnt.
ENDCLASS.

CLASS zcl_abgagt_bg_logger IMPLEMENTATION.
  METHOD constructor.
    mo_status_mgr = io_status_mgr.
    mv_job_number = iv_job_number.
  ENDMETHOD.

  METHOD zif_abgagt_bg_logger~on_progress.
    " Get current timestamp
    DATA lv_timestamp TYPE timestamp.
    GET TIME STAMP FIELD lv_timestamp.

    " Build status update structure from event parameters
    DATA(ls_status) = VALUE zif_abgagt_job_status_mgr=>ty_job_status(
      job_number = mv_job_number
      status     = 'running'
      stage      = iv_stage
      message    = iv_message
      progress   = iv_progress
      current    = iv_current
      total      = iv_total
      updated_at = lv_timestamp
    ).

    " Update status via status manager
    mo_status_mgr->update_status( ls_status ).
  ENDMETHOD.
ENDCLASS.
