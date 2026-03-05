"! <p class="shorttext synchronized">Background Job Decision Logic Interface</p>
INTERFACE zif_abgagt_bg_decision PUBLIC.

  TYPES:
    BEGIN OF ty_bg_config,
      force_background TYPE abap_bool,
      force_sync       TYPE abap_bool,
      estimated_time   TYPE i,
    END OF ty_bg_config.

  "! Determine if command should run in background
  "! @parameter io_command | Command instance
  "! @parameter is_request_data | Request data
  "! @parameter is_config | Resource-specific configuration
  "! @parameter rv_result | True = background, False = synchronous
  METHODS should_run_in_background
    IMPORTING
      io_command       TYPE REF TO zif_abgagt_command
      is_request_data  TYPE any
      is_config        TYPE ty_bg_config
    RETURNING
      VALUE(rv_result) TYPE abap_bool.

ENDINTERFACE.
