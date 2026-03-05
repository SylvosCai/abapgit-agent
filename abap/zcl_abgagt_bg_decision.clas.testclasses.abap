*"* use this source file for your ABAP unit test classes
CLASS ltc_bg_decision DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zif_abgagt_bg_decision.

    METHODS setup.

    " Priority 1: force_background
    METHODS test_force_background FOR TESTING.

    " Priority 2: force_sync
    METHODS test_force_sync FOR TESTING.

    " Priority 3: progressable interface
    METHODS test_progressable_command FOR TESTING.
    METHODS test_non_progressable_command FOR TESTING.

    " Default
    METHODS test_default_synchronous FOR TESTING.
ENDCLASS.

CLASS ltc_bg_decision IMPLEMENTATION.
  METHOD setup.
    " Create class under test
    mo_cut = NEW zcl_abgagt_bg_decision( ).
  ENDMETHOD.

  METHOD test_force_background.
    " Given: Config forces background execution
    DATA(ls_config) = VALUE zif_abgagt_bg_decision=>ty_bg_config(
      force_background = abap_true
    ).

    " And: A simple command (doesn't matter which)
    DATA(lo_command) = NEW lcl_command_stub( ).

    " And: Empty request data
    DATA ls_request TYPE string.

    " When: Deciding execution mode
    DATA(lv_result) = mo_cut->should_run_in_background(
      io_command = lo_command
      is_request_data = ls_request
      is_config = ls_config
    ).

    " Then: Should run in background (Priority 1 overrides everything)
    cl_abap_unit_assert=>assert_true(
      act = lv_result
      msg = 'force_background should return true'
    ).
  ENDMETHOD.

  METHOD test_force_sync.
    " Given: Config forces synchronous execution
    DATA(ls_config) = VALUE zif_abgagt_bg_decision=>ty_bg_config(
      force_sync = abap_true
    ).

    " And: A progressable command (should be ignored due to force_sync)
    DATA(lo_command) = NEW lcl_progressable_command_stub( ).

    " And: Empty request data
    DATA ls_request TYPE string.

    " When: Deciding execution mode
    DATA(lv_result) = mo_cut->should_run_in_background(
      io_command = lo_command
      is_request_data = ls_request
      is_config = ls_config
    ).

    " Then: Should run synchronously (Priority 2 overrides progressable)
    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = 'force_sync should return false even for progressable commands'
    ).
  ENDMETHOD.

  METHOD test_progressable_command.
    " Given: No forced config
    DATA(ls_config) = VALUE zif_abgagt_bg_decision=>ty_bg_config( ).

    " And: A command that implements progressable interface
    DATA(lo_command) = NEW lcl_progressable_command_stub( ).

    " And: Empty request data
    DATA ls_request TYPE string.

    " When: Deciding execution mode
    DATA(lv_result) = mo_cut->should_run_in_background(
      io_command = lo_command
      is_request_data = ls_request
      is_config = ls_config
    ).

    " Then: Should run in background (Priority 3)
    cl_abap_unit_assert=>assert_true(
      act = lv_result
      msg = 'Progressable command should run in background'
    ).
  ENDMETHOD.

  METHOD test_non_progressable_command.
    " Given: No forced config
    DATA(ls_config) = VALUE zif_abgagt_bg_decision=>ty_bg_config( ).

    " And: A command that does NOT implement progressable interface
    DATA(lo_command) = NEW lcl_command_stub( ).

    " And: Empty request data
    DATA ls_request TYPE string.

    " When: Deciding execution mode
    DATA(lv_result) = mo_cut->should_run_in_background(
      io_command = lo_command
      is_request_data = ls_request
      is_config = ls_config
    ).

    " Then: Should run synchronously (default)
    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = 'Non-progressable command should run synchronously by default'
    ).
  ENDMETHOD.

  METHOD test_default_synchronous.
    " Given: Empty config
    DATA(ls_config) = VALUE zif_abgagt_bg_decision=>ty_bg_config( ).

    " And: A simple command
    DATA(lo_command) = NEW lcl_command_stub( ).

    " And: Empty request data
    DATA ls_request TYPE string.

    " When: Deciding execution mode
    DATA(lv_result) = mo_cut->should_run_in_background(
      io_command = lo_command
      is_request_data = ls_request
      is_config = ls_config
    ).

    " Then: Should default to synchronous
    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = 'Default should be synchronous execution'
    ).
  ENDMETHOD.
ENDCLASS.
