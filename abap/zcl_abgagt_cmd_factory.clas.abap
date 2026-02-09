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
    DATA mo_util TYPE REF TO zif_abgagt_util.

    METHODS constructor.

    METHODS create_pull
      RETURNING
        VALUE(ro_command) TYPE REF TO zif_abgagt_command.

    METHODS create_inspect
      RETURNING
        VALUE(ro_command) TYPE REF TO zif_abgagt_command.

    METHODS create_unit
      RETURNING
        VALUE(ro_command) TYPE REF TO zif_abgagt_command.

ENDCLASS.

CLASS zcl_abgagt_cmd_factory IMPLEMENTATION.

  METHOD get_instance.
    DATA lo_factory TYPE REF TO zcl_abgagt_cmd_factory.
    CREATE OBJECT lo_factory.
    ro_factory = lo_factory.
  ENDMETHOD.

  METHOD constructor.
    mo_util = zcl_abgagt_util=>get_instance( ).
  ENDMETHOD.

  METHOD get_command.
    CASE iv_command.
      WHEN zif_abgagt_command=>co_pull.
        ro_command = create_pull( ).
      WHEN zif_abgagt_command=>co_inspect.
        ro_command = create_inspect( ).
      WHEN zif_abgagt_command=>co_unit.
        ro_command = create_unit( ).
    ENDCASE.
  ENDMETHOD.

  METHOD create_pull.
    ro_command = zcl_abgagt_pull=>create( mo_util ).
  ENDMETHOD.

  METHOD create_inspect.
    ro_command = zcl_abgagt_inspect=>create( mo_util ).
  ENDMETHOD.

  METHOD create_unit.
    ro_command = zcl_abgagt_unit=>create( mo_util ).
  ENDMETHOD.

ENDCLASS.
