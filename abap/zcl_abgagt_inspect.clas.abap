"! <p class="shorttext synchronized">Inspect Command for ABAP Git Agent</p>
"! Command for inspecting ABAP source files for issues.
CLASS zcl_abgagt_inspect DEFINITION PUBLIC CREATE PRIVATE.
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

CLASS zcl_abgagt_inspect IMPLEMENTATION.

  METHOD create.
    DATA lo_inspect TYPE REF TO zcl_abgagt_inspect.
    CREATE OBJECT lo_inspect.
    ro_command = lo_inspect.
  ENDMETHOD.

  METHOD get_name.
    rv_name = zif_abgagt_command=>co_inspect.
  ENDMETHOD.

ENDCLASS.
