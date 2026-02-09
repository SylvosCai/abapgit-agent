CLASS zcl_abgagt_inspect DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    METHODS get_name RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.

CLASS zcl_abgagt_inspect IMPLEMENTATION.
  METHOD get_name.
    rv_name = 'INSPECT'.
  ENDMETHOD.
ENDCLASS.
