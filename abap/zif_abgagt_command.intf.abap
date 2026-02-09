"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">Command Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_command PUBLIC.

  " Get command name
  METHODS get_name
    RETURNING VALUE(rv_name) TYPE string.

  " Execute command with multiple files
  " @parameter it_files | List of file paths to process
  " @parameter rv_result | JSON result string
  METHODS execute
    IMPORTING it_files TYPE string_table
    RETURNING VALUE(rv_result) TYPE string.

ENDINTERFACE.
