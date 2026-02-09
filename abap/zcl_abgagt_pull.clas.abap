"! <p class="shorttext synchronized">Pull Command for ABAP Git Agent</p>
"! Command for pulling and activating ABAP code from git repository.
CLASS zcl_abgagt_pull DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    METHODS get_name.
ENDCLASS.

CLASS zcl_abgagt_pull IMPLEMENTATION.
  METHOD get_name.
    rv_name = 'PULL'.
  ENDMETHOD.
ENDCLASS.
