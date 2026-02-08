*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_unit_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS run_tests
      IMPORTING
        iv_package TYPE devclass OPTIONAL
      RETURNING
        VALUE(rv_json) TYPE string.
ENDCLASS.

CLASS zcl_abapgit_agent_unit_agent IMPLEMENTATION.

  METHOD run_tests.
    rv_json = '{"success":"X","message":"Unit test pending implementation"}'.
  ENDMETHOD.

ENDCLASS.
