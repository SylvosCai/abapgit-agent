"! <p class="shorttext synchronized">Unit Command for ABAP Git Agent</p>
"! Command for running ABAP unit tests (AUnit).
CLASS zcl_abgagt_unit DEFINITION PUBLIC CREATE PRIVATE.
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

    METHODS run_unit_tests
      IMPORTING
        it_files TYPE string_table
      RETURNING
        VALUE(rs_result) TYPE REF TO data.

ENDCLASS.

CLASS zcl_abgagt_unit IMPLEMENTATION.

  METHOD create.
    DATA lo_util TYPE REF TO zif_abgagt_util.
    IF io_util IS BOUND.
      lo_util = io_util.
    ELSE.
      lo_util = zcl_abgagt_util=>get_instance( ).
    ENDIF.
    DATA lo_unit TYPE REF TO zcl_abgagt_unit.
    CREATE OBJECT lo_unit EXPORTING io_util = lo_util.
    ro_command = lo_unit.
  ENDMETHOD.

  METHOD constructor.
    mo_util = io_util.
  ENDMETHOD.

  METHOD get_name.
    rv_name = zif_abgagt_command=>co_unit.
  ENDMETHOD.

  METHOD execute.
    " Execute unit tests for multiple files
    " This is a stub - actual implementation would use CL_SUT_AUNIT_RUNNER
    rs_result = run_unit_tests( it_files ).
  ENDMETHOD.

  METHOD run_unit_tests.
    " Run unit tests implementation
    " This would use CL_SUT_AUNIT_RUNNER or CL_AUCV_TEST_RUNNER_STANDARD
    CREATE DATA rs_result TYPE zif_abgagt_agent=>ty_unit_result.
  ENDMETHOD.

ENDCLASS.
