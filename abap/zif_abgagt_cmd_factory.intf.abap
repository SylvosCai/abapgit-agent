"! <p class="shorttext synchronized">Command Factory Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_cmd_factory PUBLIC.

  "! Create and return a command instance by type
  "! @parameter iv_command | Command type constant (e.g., 'PULL', 'SYNTAX')
  "! @parameter ro_command | Command instance (unbound if type is unknown)
  METHODS get_command
    IMPORTING iv_command TYPE string
    RETURNING VALUE(ro_command) TYPE REF TO zif_abgagt_command.

ENDINTERFACE.
