"! <p class="shorttext synchronized">Command Factory for ABAP Git Agent</p>
"! Factory class for creating command instances. Uses singleton pattern.
CLASS zcl_abgagt_cmd_factory DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_cmd_factory.

    "! Get singleton instance
    "! @return ro_factory | Factory instance
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_factory) TYPE REF TO zcl_abgagt_cmd_factory.

    METHODS get_command
      REDEFINITION.

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
    " Private constructor for singleton
  ENDMETHOD.

  METHOD get_command.
    CASE iv_command.
      WHEN 'PULL'.
        ro_command = zcl_abgagt_pull=>create( ).
      WHEN 'INSPECT'.
        ro_command = zcl_abgagt_inspect=>create( ).
      WHEN 'UNIT'.
        ro_command = zcl_abgagt_unit=>create( ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
