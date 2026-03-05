"! <p class="shorttext synchronized">Progressable Command Interface</p>
"! Commands that implement this interface can report progress during execution
"! and will automatically be executed in background jobs.
INTERFACE zif_abgagt_progressable PUBLIC.

  "! Progress update event - raised during command execution
  EVENTS progress_update
    EXPORTING
      VALUE(iv_stage)    TYPE string
      VALUE(iv_message)  TYPE string
      VALUE(iv_progress) TYPE i
      VALUE(iv_current)  TYPE i OPTIONAL
      VALUE(iv_total)    TYPE i OPTIONAL.

ENDINTERFACE.
