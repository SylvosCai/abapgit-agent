"! <p class="shorttext synchronized">Unit Command for ABAP Git Agent</p>
"! Command for running ABAP unit tests (AUnit).
CLASS zcl_abgagt_unit DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    METHODS get_name.
ENDCLASS.

CLASS zcl_abgagt_unit IMPLEMENTATION.
  METHOD get_name.
    rv_name = 'UNIT'.
  ENDMETHOD.
ENDCLASS.
