*"* use this source file for your ABAP unit test classes
CLASS ltc_bg_scheduler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zif_abgagt_bg_scheduler.

    METHODS setup.

    " Test successful job scheduling
    METHODS test_schedule_command FOR TESTING.

    " Test job scheduling with serialized data
    METHODS test_schedule_with_data FOR TESTING.
ENDCLASS.

CLASS ltc_bg_scheduler IMPLEMENTATION.
  METHOD setup.
    " Create scheduler under test
    CREATE OBJECT mo_cut TYPE zcl_abgagt_bg_scheduler.
  ENDMETHOD.

  METHOD test_schedule_command.
    " Given: Command type and data

    DATA lv_command_type TYPE string.
    lv_command_type = 'IMPORT'.
    DATA(ls_command_data) = ''.

    " When: Scheduling a command
    DATA(ls_result) = mo_cut->schedule_command(
      iv_command_type = lv_command_type
      is_command_data = ls_command_data
    ).

    " Then: Should return job info
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-job_name
      msg = 'Job name should be returned'
    ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-job_number
      msg = 'Job number should be returned'
    ).
  ENDMETHOD.

  METHOD test_schedule_with_data.
    " Given: Command with structured data
    TYPES: BEGIN OF ty_test_data,
             repo_key TYPE string,
             files    TYPE string_table,
           END OF ty_test_data.

    DATA ls_data TYPE ty_test_data.
    DATA ls_file TYPE string.
    ls_data-repo_key = 'test_repo'.
    ls_file = 'file1.abap'.
    APPEND ls_file TO ls_data-files.
    ls_file = 'file2.abap'.
    APPEND ls_file TO ls_data-files.

    " When: Scheduling with complex data
    DATA(ls_result) = mo_cut->schedule_command(
      iv_command_type = 'IMPORT'
      is_command_data = ls_data
    ).

    " Then: Should succeed
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-job_name
      msg = 'Should handle structured data'
    ).
  ENDMETHOD.
ENDCLASS.
