"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">Command Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_command PUBLIC.
  METHODS get_name
    RETURNING VALUE(rv_name) TYPE string.
ENDINTERFACE.
