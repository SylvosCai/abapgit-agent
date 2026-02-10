CLASS zcl_abgagt_cmd_factory DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_cmd_factory.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_factory) TYPE REF TO zif_abgagt_cmd_factory.
  PRIVATE SECTION.
    METHODS constructor.
ENDCLASS.

CLASS zcl_abgagt_cmd_factory IMPLEMENTATION.
  METHOD get_instance.
    DATA lo_factory TYPE REF TO zcl_abgagt_cmd_factory.
    CREATE OBJECT lo_factory.
    ro_factory = lo_factory.
  ENDMETHOD.

  METHOD constructor.
  ENDMETHOD.

  METHOD zif_abgagt_cmd_factory~get_command.
    CASE iv_command.
      WHEN zif_abgagt_command=>gc_pull.
        ro_command = NEW zcl_abgagt_pull( ).
      WHEN zif_abgagt_command=>gc_inspect.
        ro_command = NEW zcl_abgagt_inspect( ).
      WHEN zif_abgagt_command=>gc_unit.
        ro_command = NEW zcl_abgagt_unit( ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
