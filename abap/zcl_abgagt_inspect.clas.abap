"! <p class="shorttext synchronized">Inspect Command for ABAP Git Agent</p>
"! Command for inspecting ABAP source files for issues (syntax check).
CLASS zcl_abgagt_inspect DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    "! Create command instance
    "! @parameter io_util | Utility instance
    "! @return ro_command | Command instance
    CLASS-METHODS create
      IMPORTING
        io_util TYPE REF TO zif_abgagt_util OPTIONAL
      RETURNING
        VALUE(ro_command) TYPE REF TO zif_abgagt_command.

    METHODS get_name
      REDEFINITION.

  PRIVATE SECTION.
    DATA mo_util TYPE REF TO zif_abgagt_util.

    METHODS constructor
      IMPORTING io_util TYPE REF TO zif_abgagt_util.

ENDCLASS.

CLASS zcl_abgagt_inspect IMPLEMENTATION.

  METHOD create.
    DATA lo_util TYPE REF TO zif_abgagt_util.
    IF io_util IS BOUND.
      lo_util = io_util.
    ELSE.
      lo_util = zcl_abgagt_util=>get_instance( ).
    ENDIF.
    DATA lo_inspect TYPE REF TO zcl_abgagt_inspect.
    CREATE OBJECT lo_inspect EXPORTING io_util = lo_util.
    ro_command = lo_inspect.
  ENDMETHOD.

  METHOD constructor.
    mo_util = io_util.
  ENDMETHOD.

  METHOD get_name.
    rv_name = zif_abgagt_command=>co_inspect.
  ENDMETHOD.

ENDCLASS.
