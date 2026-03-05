"----------------------------------------------------------------------
" Test Doubles - Implementations
"----------------------------------------------------------------------

CLASS lcl_progressable_stub IMPLEMENTATION.
  METHOD trigger_progress.
    " Raise the progress_update event from this object's context
    RAISE EVENT zif_abgagt_progressable~progress_update
      EXPORTING
        iv_stage    = iv_stage
        iv_message  = iv_message
        iv_progress = iv_progress
        iv_current  = iv_current
        iv_total    = iv_total.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_status_mgr_stub IMPLEMENTATION.
  METHOD zif_abgagt_job_status_mgr~update_status.
    " Record the last update for test verification
    ms_last_status = is_status.
  ENDMETHOD.

  METHOD zif_abgagt_job_status_mgr~get_status.
    " Not used in logger tests
    CLEAR rs_status.
  ENDMETHOD.

  METHOD zif_abgagt_job_status_mgr~delete_status.
    " Not used in logger tests
  ENDMETHOD.

  METHOD get_last_status.
    rs_status = ms_last_status.
  ENDMETHOD.
ENDCLASS.
