"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">Command Factory Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_cmd_factory PUBLIC.
  METHODS get_command
    IMPORTING iv_command TYPE string
    RETURNING VALUE(ro_command) TYPE REF TO zif_abgagt_command.
ENDINTERFACE.
