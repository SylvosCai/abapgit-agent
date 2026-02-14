"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">Command Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_command PUBLIC.

  CONSTANTS:
    gc_pull TYPE string VALUE 'PULL',
    gc_inspect TYPE string VALUE 'INSPECT',
    gc_unit TYPE string VALUE 'UNIT',
    gc_create TYPE string VALUE 'CREATE',
    gc_import TYPE string VALUE 'IMPORT',
    gc_tree TYPE string VALUE 'TREE'.

  " Get command name
  METHODS get_name
    RETURNING VALUE(rv_name) TYPE string.

  " Execute command with optional parameters
  " @parameter is_param | Generic parameters (structure varies by command)
  " @parameter rv_result | JSON result string
  METHODS execute
    IMPORTING is_param TYPE data OPTIONAL
    RETURNING VALUE(rv_result) TYPE string.

ENDINTERFACE.
