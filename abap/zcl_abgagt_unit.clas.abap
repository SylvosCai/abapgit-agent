"! <p class="shorttext synchronized">Unit Command for ABAP Git Agent</p>
"! Command for running ABAP unit tests (AUnit).
CLASS zcl_abgagt_unit DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    "! Create command instance
    "! @return ro_command | Command instance
    CLASS-METHODS create
      RETURNING
        VALUE(ro_command) TYPE REF TO zif_abgagt_command.

    METHODS get_name
      REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_unit IMPLEMENTATION.

  METHOD create.
    DATA lo_unit TYPE REF TO zcl_abgagt_unit.
    CREATE OBJECT lo_unit.
    ro_command = lo_unit.
  ENDMETHOD.

  METHOD get_name.
    rv_name = zif_abgagt_command=>co_unit.
  ENDMETHOD.

ENDCLASS.
