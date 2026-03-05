"! <p class="shorttext synchronized">Background Job Scheduler Interface</p>
INTERFACE zif_abgagt_bg_scheduler PUBLIC.

  TYPES:
    BEGIN OF ty_job_info,
      job_name   TYPE btcjob,
      job_number TYPE btcjobcnt,
    END OF ty_job_info.

  "! Schedule a command for background execution
  "! @parameter iv_command_type | Command type constant (e.g., 'IMPORT', 'PULL')
  "! @parameter is_command_data | Command request data (will be serialized)
  "! @parameter rs_result | Job information (name and number)
  "! @raising cx_root | If job scheduling fails
  METHODS schedule_command
    IMPORTING
      iv_command_type  TYPE string
      is_command_data  TYPE any
    RETURNING
      VALUE(rs_result) TYPE ty_job_info
    RAISING
      cx_root.

ENDINTERFACE.
