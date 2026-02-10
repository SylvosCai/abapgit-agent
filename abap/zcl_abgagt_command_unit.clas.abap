*"*"use source
*"*"Local Interface:
*"**********************************************************************
" UNIT command implementation - delegates to agent for AUnit tests
CLASS zcl_abgagt_command_unit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

ENDCLASS.

CLASS zcl_abgagt_command_unit IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_unit.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: lv_package TYPE devclass,
          lv_json TYPE string,
          lo_agent TYPE REF TO zcl_abgagt_agent,
          ls_result TYPE zif_abgagt_agent=>ty_unit_result,
          ls_response TYPE zif_abgagt_agent=>ty_unit_result.

    IF lines( it_files ) = 1.
      READ TABLE it_files INDEX 1 INTO lv_json.
      IF lv_json CP '*{*' OR lv_json CP '*"*'.
        DATA: ls_params TYPE zif_abgagt_agent=>ty_pull_params.
        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_json
          CHANGING data = ls_params ).
        lv_package = ls_params-package.
      ENDIF.
    ENDIF.

    lo_agent = NEW zcl_abgagt_agent( ).
    ls_result = lo_agent->zif_abgagt_agent~run_tests(
      iv_package = lv_package ).

    ls_response-success = ls_result-success.
    ls_response-message = ls_result-message.
    ls_response-test_count = ls_result-test_count.
    ls_response-passed_count = ls_result-passed_count.
    ls_response-failed_count = ls_result-failed_count.
    ls_response-results = ls_result-results.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.

ENDCLASS.
