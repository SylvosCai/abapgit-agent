CLASS zcl_abgagt_cmd_factory DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_cmd_factory.
    METHODS get_command
      IMPORTING iv_command TYPE string
      RETURNING VALUE(ro_command) TYPE REF TO object.
ENDCLASS.

CLASS zcl_abgagt_cmd_factory IMPLEMENTATION.
  METHOD get_command.
  ENDMETHOD.
ENDCLASS.
