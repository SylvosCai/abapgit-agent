"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">Command Factory Interface for ABAP Git Agent</p>
"! Factory interface for creating command instances.
INTERFACE zif_abgagt_cmd_factory PUBLIC.

  "! Get command instance by name
  "! @parameter iv_command | Command name (e.g., 'PULL', 'INSPECT', 'UNIT')
  "! @return ro_command | Command instance
  METHODS get_command
    IMPORTING
      iv_command TYPE string
    RETURNING
      VALUE(ro_command) TYPE REF TO object.

ENDINTERFACE.
