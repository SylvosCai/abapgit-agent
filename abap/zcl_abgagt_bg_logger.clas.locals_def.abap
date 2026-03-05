"----------------------------------------------------------------------
" Test Doubles - Class Definitions
"----------------------------------------------------------------------

" Progressable command stub (raises progress events)
CLASS lcl_progressable_stub DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_progressable.

    " Helper method to trigger progress event
    METHODS trigger_progress
      IMPORTING
        iv_stage    TYPE string
        iv_message  TYPE string
        iv_progress TYPE i
        iv_current  TYPE i OPTIONAL
        iv_total    TYPE i OPTIONAL.
ENDCLASS.

" Status manager stub (records update calls)
CLASS lcl_status_mgr_stub DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_job_status_mgr.

    " Test helper method
    METHODS get_last_status
      RETURNING VALUE(rs_status) TYPE zif_abgagt_job_status_mgr=>ty_job_status.

  PRIVATE SECTION.
    DATA ms_last_status TYPE zif_abgagt_job_status_mgr=>ty_job_status.
ENDCLASS.
