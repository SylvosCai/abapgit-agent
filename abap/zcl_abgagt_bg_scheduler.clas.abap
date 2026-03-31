CLASS zcl_abgagt_bg_scheduler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_bg_scheduler.

  PRIVATE SECTION.
    CONSTANTS c_program_name TYPE btcprog VALUE 'Z_ABGAGT_BG_EXECUTOR'.

    METHODS serialize_data
      IMPORTING
        is_data           TYPE any
      RETURNING
        VALUE(rv_json)    TYPE string.
ENDCLASS.

CLASS zcl_abgagt_bg_scheduler IMPLEMENTATION.
  METHOD zif_abgagt_bg_scheduler~schedule_command.
    DATA lv_jobname TYPE btcjob.
    DATA lv_jobcount TYPE btcjobcnt.

    " Generate unique job name
    lv_jobname = |ABGAGT_{ iv_command_type }_{ sy-datum }{ sy-uzeit }|.

    " Open job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_jobname
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      " Job creation failed
      RAISE EXCEPTION TYPE cx_sy_no_handler.
    ENDIF.

    " Serialize command data to JSON
    DATA(lv_command_json) = serialize_data( is_command_data ).

    " Submit program to job (run as current user)
    SUBMIT (c_program_name)
      WITH p_cmd = iv_command_type
      WITH p_data = lv_command_json
      USER sy-uname
      VIA JOB lv_jobname NUMBER lv_jobcount
      AND RETURN.

    " Close and schedule job immediately
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobcount
        jobname              = lv_jobname
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.

    IF sy-subrc <> 0.
      " Job creation failed
      RAISE EXCEPTION TYPE cx_sy_no_handler.
    ENDIF.

    " Return job info
    CLEAR rs_result.
    rs_result-job_name   = lv_jobname.
    rs_result-job_number = lv_jobcount.
  ENDMETHOD.

  METHOD serialize_data.
    " Serialize any structure to JSON
    /ui2/cl_json=>serialize(
      EXPORTING
        data        = is_data
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = rv_json
    ).
  ENDMETHOD.
ENDCLASS.
