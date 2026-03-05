*"* use this source file for your ABAP unit test classes
CLASS ltc_bg_status_mgr DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zif_abgagt_job_status_mgr.

    METHODS setup.
    METHODS teardown.

    " Test status update and retrieval
    METHODS test_update_and_get_status FOR TESTING.

    " Test that get returns empty status for non-existent job
    METHODS test_get_nonexistent_job FOR TESTING.

    " Test status update overwrites previous status
    METHODS test_update_overwrites FOR TESTING.

    " Test delete removes status
    METHODS test_delete_status FOR TESTING.
ENDCLASS.

CLASS ltc_bg_status_mgr IMPLEMENTATION.
  METHOD setup.
    " Create status manager under test
    mo_cut = NEW zcl_abgagt_bg_status_mgr( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up test data
    DATA(lv_job_number) = '99999999'.
    mo_cut->delete_status( lv_job_number ).
  ENDMETHOD.

  METHOD test_update_and_get_status.
    " Given: A job status
    DATA(ls_status) = VALUE zif_abgagt_job_status_mgr=>ty_job_status(
      job_name     = 'TEST_JOB'
      job_number   = '99999999'
      status       = 'RUNNING'
      stage        = 'STAGE1'
      message      = 'Processing files'
      progress     = 50
      current      = 5
      total        = 10
    ).

    " When: Status is updated
    mo_cut->update_status( ls_status ).

    " Then: Status can be retrieved
    DATA(ls_result) = mo_cut->get_status( '99999999' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-job_name
      exp = 'TEST_JOB'
      msg = 'Job name should match'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-status
      exp = 'RUNNING'
      msg = 'Status should match'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-stage
      exp = 'STAGE1'
      msg = 'Stage should match'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-progress
      exp = 50
      msg = 'Progress should match'
    ).
  ENDMETHOD.

  METHOD test_get_nonexistent_job.
    " When: Getting status for non-existent job
    DATA(ls_result) = mo_cut->get_status( '00000000' ).

    " Then: Should return empty status
    cl_abap_unit_assert=>assert_initial(
      act = ls_result-job_number
      msg = 'Non-existent job should return empty status'
    ).
  ENDMETHOD.

  METHOD test_update_overwrites.
    " Given: Initial status
    DATA(ls_status1) = VALUE zif_abgagt_job_status_mgr=>ty_job_status(
      job_number = '99999999'
      status     = 'RUNNING'
      progress   = 25
    ).
    mo_cut->update_status( ls_status1 ).

    " When: Status is updated again
    DATA(ls_status2) = VALUE zif_abgagt_job_status_mgr=>ty_job_status(
      job_number = '99999999'
      status     = 'RUNNING'
      progress   = 75
    ).
    mo_cut->update_status( ls_status2 ).

    " Then: New status should overwrite old
    DATA(ls_result) = mo_cut->get_status( '99999999' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-progress
      exp = 75
      msg = 'Progress should be updated'
    ).
  ENDMETHOD.

  METHOD test_delete_status.
    " Given: A stored status
    DATA(ls_status) = VALUE zif_abgagt_job_status_mgr=>ty_job_status(
      job_number = '99999999'
      status     = 'COMPLETED'
    ).
    mo_cut->update_status( ls_status ).

    " When: Status is deleted
    mo_cut->delete_status( '99999999' ).

    " Then: Status should no longer exist
    DATA(ls_result) = mo_cut->get_status( '99999999' ).

    cl_abap_unit_assert=>assert_initial(
      act = ls_result-job_number
      msg = 'Deleted status should not be retrievable'
    ).
  ENDMETHOD.
ENDCLASS.
