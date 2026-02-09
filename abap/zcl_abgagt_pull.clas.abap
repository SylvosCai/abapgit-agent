"! <p class="shorttext synchronized">Pull Command for ABAP Git Agent</p>
"! Command for pulling and activating ABAP code from git repository.
CLASS zcl_abgagt_pull DEFINITION PUBLIC CREATE PRIVATE.
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

CLASS zcl_abgagt_pull IMPLEMENTATION.

  METHOD create.
    DATA lo_pull TYPE REF TO zcl_abgagt_pull.
    CREATE OBJECT lo_pull.
    ro_command = lo_pull.
  ENDMETHOD.

  METHOD get_name.
    rv_name = zif_abgagt_command=>co_pull.
  ENDMETHOD.

ENDCLASS.
