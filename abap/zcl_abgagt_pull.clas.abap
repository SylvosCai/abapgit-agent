CLASS zcl_abgagt_pull DEFINITION.
  PUBLIC SECTION.
    METHODS get_name RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.

CLASS zcl_abgagt_pull IMPLEMENTATION.
  METHOD get_name.
    rv_name = 'PULL'.
  ENDMETHOD.
ENDCLASS.
