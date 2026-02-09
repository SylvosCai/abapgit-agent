CLASS zcl_abgagt_unit DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    METHODS get_name RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.

CLASS zcl_abgagt_unit IMPLEMENTATION.
  METHOD get_name.
    rv_name = 'UNIT'.
  ENDMETHOD.
ENDCLASS.
