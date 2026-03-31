*"* use this source file for your ABAP unit test classes
*"* Integration tests for background job infrastructure

" ======================================================================
" Test Suite 1: Progressable Command Detection
" ======================================================================
CLASS ltc_import_is_progressable DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_decision TYPE REF TO zif_abgagt_bg_decision.
    DATA mo_command TYPE REF TO zif_abgagt_command.

    METHODS setup.

    " Test that import command is recognized as progressable
    METHODS test_import_cmd_progressable FOR TESTING.
ENDCLASS.

CLASS ltc_import_is_progressable IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_decision TYPE zcl_abgagt_bg_decision.
    CREATE OBJECT mo_command TYPE zcl_abgagt_command_import.
  ENDMETHOD.

  METHOD test_import_cmd_progressable.
    " Given: Import command (which implements progressable)
    DATA ls_config TYPE zif_abgagt_bg_decision=>ty_bg_config.
    DATA ls_request TYPE string.

    " When: Decision maker evaluates it
    DATA(lv_result) = mo_decision->should_run_in_background(
      io_command      = mo_command
      is_request_data = ls_request
      is_config       = ls_config
    ).

    " Then: Should recommend background execution (Priority 3)
    cl_abap_unit_assert=>assert_true(
      act = lv_result
      msg = 'Import command should run in background (implements progressable)'
    ).
  ENDMETHOD.
ENDCLASS.

" ======================================================================
" Test Suite 2: Non-Progressable Command Detection
" ======================================================================
CLASS ltc_non_progressable_cmd DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_decision TYPE REF TO zif_abgagt_bg_decision.
    DATA mo_command TYPE REF TO zif_abgagt_command.

    METHODS setup.

    " Test that non-progressable commands run synchronously
    METHODS test_pull_cmd_synchronous FOR TESTING.
ENDCLASS.

CLASS ltc_non_progressable_cmd IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_decision TYPE zcl_abgagt_bg_decision.
    CREATE OBJECT mo_command TYPE zcl_abgagt_command_pull.
  ENDMETHOD.

  METHOD test_pull_cmd_synchronous.
    " Given: Pull command (does NOT implement progressable)
    DATA ls_config TYPE zif_abgagt_bg_decision=>ty_bg_config.
    DATA ls_request TYPE string.

    " When: Decision maker evaluates it
    DATA(lv_result) = mo_decision->should_run_in_background(
      io_command      = mo_command
      is_request_data = ls_request
      is_config       = ls_config
    ).

    " Then: Should run synchronously (no progressable interface)
    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = 'Pull command should run synchronously (no progressable interface)'
    ).
  ENDMETHOD.
ENDCLASS.

" ======================================================================
" Test Suite 3: Background Job Status Flow
" ======================================================================
CLASS ltc_bg_status_flow DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_status_mgr TYPE REF TO zif_abgagt_job_status_mgr.

    METHODS setup.

    " Test that status can be written and read
    METHODS test_status_roundtrip FOR TESTING.
ENDCLASS.

CLASS ltc_bg_status_flow IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_status_mgr TYPE zcl_abgagt_bg_status_mgr.
  ENDMETHOD.

  METHOD test_status_roundtrip.
    " Given: A job status
    DATA(lv_job_number) = '99999999'.
    DATA ls_status TYPE zif_abgagt_job_status_mgr=>ty_job_status.
    ls_status-job_number = lv_job_number.
    ls_status-status     = 'running'.
    ls_status-stage      = 'TEST_STAGE'.
    ls_status-message    = 'Integration test'.
    ls_status-progress   = 50.

    " When: Status is written and read back
    mo_status_mgr->update_status( ls_status ).
    DATA(ls_read_status) = mo_status_mgr->get_status( lv_job_number ).

    " Then: Status should match
    cl_abap_unit_assert=>assert_equals(
      act = ls_read_status-status
      exp = 'running'
      msg = 'Status should be preserved in shared buffer'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_read_status-progress
      exp = 50
      msg = 'Progress should be preserved'
    ).

    " Cleanup
    mo_status_mgr->delete_status( lv_job_number ).
  ENDMETHOD.
ENDCLASS.
