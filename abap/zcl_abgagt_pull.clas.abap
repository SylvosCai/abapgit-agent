"! <p class="shorttext synchronized">Pull Command for ABAP Git Agent</p>
"! Command for pulling and activating ABAP code from git repository.
"! This is the STABLE command - kept minimal to avoid syntax errors.
CLASS zcl_abgagt_pull DEFINITION PUBLIC CREATE PRIVATE.
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

    METHODS pull_files
      IMPORTING
        it_files TYPE string_table
      RETURNING
        VALUE(rs_result) TYPE REF TO data.

ENDCLASS.

CLASS zcl_abgagt_pull IMPLEMENTATION.

  METHOD create.
    DATA lo_util TYPE REF TO zif_abgagt_util.
    IF io_util IS BOUND.
      lo_util = io_util.
    ELSE.
      lo_util = zcl_abgagt_util=>get_instance( ).
    ENDIF.
    DATA lo_pull TYPE REF TO zcl_abgagt_pull.
    CREATE OBJECT lo_pull EXPORTING io_util = lo_util.
    ro_command = lo_pull.
  ENDMETHOD.

  METHOD constructor.
    mo_util = io_util.
  ENDMETHOD.

  METHOD get_name.
    rv_name = zif_abgagt_command=>co_pull.
  ENDMETHOD.

  METHOD execute.
    " Execute pull for multiple files
    " For now, return success as this is a stub
    " The actual implementation would delegate to zcl_abgagt_agent=>pull
    rs_result = pull_files( it_files ).
  ENDMETHOD.

  METHOD pull_files.
    " Stub implementation - delegate to existing agent
    " This would return a ty_result structure
    " For now, return empty result
    CREATE DATA rs_result TYPE zif_abgagt_agent=>ty_result.
  ENDMETHOD.

ENDCLASS.
