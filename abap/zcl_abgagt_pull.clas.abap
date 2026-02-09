CLASS zcl_abgagt_pull DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    METHODS get_name RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.

CLASS zcl_abgagt_pull IMPLEMENTATION.
  METHOD get_name.
    rv_name = 'PULL'.
  ENDMETHOD.
ENDCLASS.
