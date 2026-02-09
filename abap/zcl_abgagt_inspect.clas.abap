"! <p class="shorttext synchronized">Inspect Command for ABAP Git Agent</p>
"! Command for inspecting ABAP source files for issues.
CLASS zcl_abgagt_inspect DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    METHODS get_name REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_inspect IMPLEMENTATION.

  METHOD get_name.
    rv_name = 'INSPECT'.
  ENDMETHOD.

ENDCLASS.
