*&---------------------------------------------------------------------*
*& Report Z_ABGAGT_BG_EXECUTOR
*&---------------------------------------------------------------------*
*& Generic Background Job Executor for ABAP Git Agent Commands
*&---------------------------------------------------------------------*
REPORT z_abgagt_bg_executor.

PARAMETERS: p_cmd   TYPE string LOWER CASE,  " Command type (e.g., 'IMPORT')
            p_data  TYPE string LOWER CASE.  " Serialized command data (JSON)

DATA: lo_factory      TYPE REF TO zif_abgagt_cmd_factory,
      lo_command      TYPE REF TO zif_abgagt_command,
      lo_status_mgr   TYPE REF TO zif_abgagt_job_status_mgr,
      lo_logger       TYPE REF TO zif_abgagt_bg_logger,
      lo_progressable TYPE REF TO zif_abgagt_progressable,
      lv_result       TYPE string,
      lv_job_number   TYPE btcjobcnt,
      ls_status       TYPE zif_abgagt_job_status_mgr=>ty_job_status,
      lv_timestamp    TYPE timestamp.

START-OF-SELECTION.
  TRY.
      " Get current job number
      CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
        IMPORTING
          jobcount = lv_job_number
        EXCEPTIONS
          OTHERS   = 1.

      IF sy-subrc <> 0.
        lv_job_number = '00000000'.
      ENDIF.

      " Initialize status manager
      lo_status_mgr = NEW zcl_abgagt_bg_status_mgr( ).

      " Get current timestamp
      GET TIME STAMP FIELD lv_timestamp.

      " Update initial status
      ls_status = VALUE #(
        job_number  = lv_job_number
        status      = 'STARTING'
        stage       = 'INITIALIZATION'
        message     = 'Initializing command execution'
        progress    = 0
        started_at  = lv_timestamp
        updated_at  = lv_timestamp
      ).
      lo_status_mgr->update_status( ls_status ).

      " Get command factory instance
      lo_factory = zcl_abgagt_cmd_factory=>get_instance( ).

      " Get command instance from factory
      lo_command = lo_factory->get_command( p_cmd ).

      " Check if command implements progressable interface
      TRY.
          lo_progressable ?= lo_command.

          " Command supports progress reporting - create logger
          lo_logger = NEW zcl_abgagt_bg_logger(
            io_status_mgr = lo_status_mgr
            iv_job_number = lv_job_number
          ).

          " Register logger as event handler
          SET HANDLER lo_logger->on_progress FOR lo_progressable.

        CATCH cx_sy_move_cast_error.
          " Command does not support progress reporting - that's ok
      ENDTRY.

      " Execute command with JSON data
      " Command will handle deserializing JSON if needed
      GET TIME STAMP FIELD lv_timestamp.
      ls_status = VALUE #(
        job_number = lv_job_number
        status     = 'running'
        stage      = 'EXECUTION'
        message    = 'Executing command'
        progress   = 0
        updated_at = lv_timestamp
      ).
      lo_status_mgr->update_status( ls_status ).

      " Pass JSON string as parameter - command will deserialize if needed
      lv_result = lo_command->execute( is_param = p_data ).

      " Update final status - success
      GET TIME STAMP FIELD lv_timestamp.
      ls_status = VALUE #(
        job_number   = lv_job_number
        status       = 'completed'
        stage        = 'FINISHED'
        message      = 'Command completed successfully'
        progress     = 100
        result       = lv_result
        updated_at   = lv_timestamp
        completed_at = lv_timestamp
      ).
      lo_status_mgr->update_status( ls_status ).

    CATCH cx_root INTO DATA(lx_error).
      " Update final status - error
      GET TIME STAMP FIELD lv_timestamp.
      ls_status = VALUE #(
        job_number    = lv_job_number
        status        = 'error'
        stage         = 'FAILED'
        message       = 'Command execution failed'
        error_message = lx_error->get_text( )
        updated_at    = lv_timestamp
        completed_at  = lv_timestamp
      ).
      lo_status_mgr->update_status( ls_status ).

      " Re-raise exception so job shows as failed
      RAISE EXCEPTION lx_error.
  ENDTRY.
