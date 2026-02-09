"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">Command Interface for ABAP Git Agent</p>
"! Interface for all command implementations in ABAP Git Agent.
INTERFACE zif_abgagt_command PUBLIC.

  "! Command type constants
  CONSTANTS co_pull    TYPE string VALUE 'PULL'.
  CONSTANTS co_inspect TYPE string VALUE 'INSPECT'.
  CONSTANTS co_unit   TYPE string VALUE 'UNIT'.

  "! Execute the command
  "! @parameter it_files | List of files to process
  "! @parameter iv_package | Target package (optional)
  "! @return rs_result | Command result as ref to data
  METHODS execute
    IMPORTING
      it_files   TYPE string_table
      iv_package TYPE devclass OPTIONAL
    RETURNING
      VALUE(rs_result) TYPE REF TO data.

  "! Get command name
  "! @return rv_name | Command name
  METHODS get_name
    RETURNING
      VALUE(rv_name) TYPE string.

ENDINTERFACE.
