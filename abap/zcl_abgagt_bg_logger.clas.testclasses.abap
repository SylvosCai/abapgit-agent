*"* use this source file for your ABAP unit test classes
CLASS ltc_bg_logger DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zif_abgagt_bg_logger.
    DATA mo_status_mgr TYPE REF TO zif_abgagt_job_status_mgr.
    DATA mo_progressable TYPE REF TO lcl_progressable_stub.

    METHODS setup.

    " Test that logger updates status when progress event is raised
    METHODS test_on_progress_update FOR TESTING.

    " Test that logger handles all optional parameters
    METHODS test_full_params FOR TESTING.

    " Test that logger handles minimal parameters
    METHODS test_minimal_params FOR TESTING.
ENDCLASS.

CLASS ltc_bg_logger IMPLEMENTATION.
  METHOD setup.
    " Create status manager stub
    CREATE OBJECT mo_status_mgr TYPE lcl_status_mgr_stub.

    " Create logger under test
    CREATE OBJECT mo_cut TYPE zcl_abgagt_bg_logger
      EXPORTING
        io_status_mgr = mo_status_mgr
        iv_job_number = '12345678'.

    " Create progressable command stub
    CREATE OBJECT mo_progressable TYPE lcl_progressable_stub.

    " Register logger as event handler
    SET HANDLER mo_cut->on_progress FOR mo_progressable.
  ENDMETHOD.

  METHOD test_on_progress_update.
    " When: Progress event is raised with all parameters
    mo_progressable->trigger_progress(
      iv_stage    = 'STAGE1'
      iv_message  = 'Processing files'
      iv_progress = 50
      iv_current  = 5
      iv_total    = 10
    ).

    " Then: Status manager should be called with correct data
    DATA(lo_stub) = CAST lcl_status_mgr_stub( mo_status_mgr ).
    DATA(ls_status) = lo_stub->get_last_status( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_status-stage
      exp = 'STAGE1'
      msg = 'Stage should match event parameter'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_status-message
      exp = 'Processing files'
      msg = 'Message should match event parameter'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_status-progress
      exp = 50
      msg = 'Progress should match event parameter'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_status-current
      exp = 5
      msg = 'Current should match event parameter'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_status-total
      exp = 10
      msg = 'Total should match event parameter'
    ).
  ENDMETHOD.

  METHOD test_full_params.
    " When: Progress event is raised with all optional parameters
    mo_progressable->trigger_progress(
      iv_stage    = 'STAGE2'
      iv_message  = 'Finalizing'
      iv_progress = 100
      iv_current  = 10
      iv_total    = 10
    ).

    " Then: All parameters should be stored
    DATA(lo_stub) = CAST lcl_status_mgr_stub( mo_status_mgr ).
    DATA(ls_status) = lo_stub->get_last_status( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_status-stage
      exp = 'STAGE2'
      msg = 'Stage should be stored'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_status-progress
      exp = 100
      msg = 'Progress 100% should be stored'
    ).
  ENDMETHOD.

  METHOD test_minimal_params.
    " When: Progress event is raised without optional parameters
    mo_progressable->trigger_progress(
      iv_stage    = 'STAGE3'
      iv_message  = 'Starting'
      iv_progress = 0
    ).

    " Then: Logger should handle missing optional parameters
    DATA(lo_stub) = CAST lcl_status_mgr_stub( mo_status_mgr ).
    DATA(ls_status) = lo_stub->get_last_status( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_status-stage
      exp = 'STAGE3'
      msg = 'Stage should be stored even without optional params'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_status-progress
      exp = 0
      msg = 'Progress 0% should be stored'
    ).
  ENDMETHOD.
ENDCLASS.
