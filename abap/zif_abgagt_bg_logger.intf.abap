"! <p class="shorttext synchronized">Background Job Progress Logger Interface</p>
INTERFACE zif_abgagt_bg_logger PUBLIC.

  "! Event handler for command progress updates
  "! @parameter iv_stage | Current stage name
  "! @parameter iv_message | Progress message
  "! @parameter iv_progress | Progress percentage (0-100)
  "! @parameter iv_current | Current item number
  "! @parameter iv_total | Total items
  METHODS on_progress
    FOR EVENT progress_update OF zif_abgagt_progressable
    IMPORTING
      iv_stage
      iv_message
      iv_progress
      iv_current
      iv_total.

ENDINTERFACE.
