CLASS zcl_abgagt_cmd_factory DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_cmd_factory.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_factory) TYPE REF TO zcl_abgagt_cmd_factory.
    METHODS get_command.
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

  METHOD get_command.
    CASE iv_command.
      WHEN 'PULL'.
        ro_command = NEW zcl_abgagt_pull( ).
      WHEN 'INSPECT'.
        ro_command = NEW zcl_abgagt_inspect( ).
      WHEN 'UNIT'.
        ro_command = NEW zcl_abgagt_unit( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
