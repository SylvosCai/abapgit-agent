*"* use this source file for your test class implementation
*"* local test class

"----------------------------------------------------------------------
" Local log double — implements zif_abapgit_log with configurable state
"----------------------------------------------------------------------
CLASS ltcl_log_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_log.
    DATA mv_status   TYPE sy-msgty.
    DATA mt_messages TYPE zif_abapgit_log=>ty_log_outs.
ENDCLASS.

CLASS ltcl_log_double IMPLEMENTATION.
  METHOD zif_abapgit_log~get_status.
    rv_status = mv_status.
  ENDMETHOD.
  METHOD zif_abapgit_log~get_messages.
    rt_msg = mt_messages.
  ENDMETHOD.
  METHOD zif_abapgit_log~add.
  ENDMETHOD.
  METHOD zif_abapgit_log~add_exception.
  ENDMETHOD.
  METHOD zif_abapgit_log~add_error.
  ENDMETHOD.
  METHOD zif_abapgit_log~add_warning.
  ENDMETHOD.
  METHOD zif_abapgit_log~add_info.
  ENDMETHOD.
  METHOD zif_abapgit_log~add_success.
  ENDMETHOD.
  METHOD zif_abapgit_log~count.
  ENDMETHOD.
  METHOD zif_abapgit_log~clear.
  ENDMETHOD.
  METHOD zif_abapgit_log~merge_with.
  ENDMETHOD.
  METHOD zif_abapgit_log~set_title.
  ENDMETHOD.
  METHOD zif_abapgit_log~get_title.
  ENDMETHOD.
  METHOD zif_abapgit_log~clone.
  ENDMETHOD.
  METHOD zif_abapgit_log~get_item_status.
  ENDMETHOD.
  METHOD zif_abapgit_log~get_log_level.
  ENDMETHOD.
ENDCLASS.

"----------------------------------------------------------------------
" Main test class
"----------------------------------------------------------------------
CLASS ltcl_agent DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut  TYPE REF TO zcl_abgagt_agent.
    DATA mo_repo TYPE REF TO zif_abapgit_repo.
    DATA mo_det  TYPE REF TO zif_abgagt_conflict_detector.
    DATA mo_log  TYPE REF TO ltcl_log_double.

    METHODS setup.

    " --- parse_file_to_object ---
    METHODS test_parse_file_class      FOR TESTING.
    METHODS test_parse_file_interface  FOR TESTING.
    METHODS test_parse_file_cds_view   FOR TESTING.
    METHODS test_parse_file_with_path  FOR TESTING.
    METHODS test_parse_file_invalid    FOR TESTING.

    " --- pull ---
    METHODS test_pull_no_url           FOR TESTING.
    METHODS test_pull_success          FOR TESTING.
    METHODS test_pull_with_errors      FOR TESTING.
    METHODS test_pull_activated_objs   FOR TESTING.
    METHODS test_pull_failed_objects   FOR TESTING.
    METHODS test_pull_transport        FOR TESTING.
    METHODS test_pull_conflict_ignored FOR TESTING.

    " --- get_repo_status ---
    METHODS test_get_repo_status_found FOR TESTING.
ENDCLASS.

CLASS ltcl_agent IMPLEMENTATION.

  METHOD setup.
    mo_log = NEW ltcl_log_double( ).
    mo_log->mv_status = zif_abapgit_log=>c_status-ok.

    mo_repo ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO' ).
    mo_det  ?= cl_abap_testdouble=>create( 'ZIF_ABGAGT_CONFLICT_DETECTOR' ).

    " Default: get_log / create_new_log return mo_log
    cl_abap_testdouble=>configure_call( mo_repo )->returning( mo_log ).
    mo_repo->get_log( ).
    cl_abap_testdouble=>configure_call( mo_repo )->returning( mo_log ).
    mo_repo->create_new_log( ).

    " Default: no conflicts
    DATA lt_empty TYPE zif_abgagt_conflict_detector=>ty_conflicts.
    cl_abap_testdouble=>configure_call( mo_det )->returning( lt_empty ).
    mo_det->check_conflicts( it_files = VALUE #( ) iv_branch = '' ).

    mo_cut = NEW zcl_abgagt_agent(
      io_repo              = mo_repo
      io_conflict_detector = mo_det ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " parse_file_to_object — class file
  "--------------------------------------------------------------------
  METHOD test_parse_file_class.
    DATA lv_type TYPE string.
    DATA lv_name TYPE string.

    mo_cut->parse_file_to_object(
      EXPORTING iv_file     = 'zcl_my_class.clas.abap'
      IMPORTING ev_obj_type = lv_type
                ev_obj_name = lv_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type exp = 'CLAS'
      msg = 'Object type must be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name exp = 'ZCL_MY_CLASS'
      msg = 'Object name must be ZCL_MY_CLASS' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " parse_file_to_object — interface file
  "--------------------------------------------------------------------
  METHOD test_parse_file_interface.
    DATA lv_type TYPE string.
    DATA lv_name TYPE string.

    mo_cut->parse_file_to_object(
      EXPORTING iv_file     = 'zif_my_intf.intf.abap'
      IMPORTING ev_obj_type = lv_type
                ev_obj_name = lv_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type exp = 'INTF'
      msg = 'Object type must be INTF' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name exp = 'ZIF_MY_INTF'
      msg = 'Object name must be ZIF_MY_INTF' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " parse_file_to_object — CDS view (.asddls)
  "--------------------------------------------------------------------
  METHOD test_parse_file_cds_view.
    DATA lv_type TYPE string.
    DATA lv_name TYPE string.

    mo_cut->parse_file_to_object(
      EXPORTING iv_file     = 'zc_my_view.ddls.asddls'
      IMPORTING ev_obj_type = lv_type
                ev_obj_name = lv_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type exp = 'DDLS'
      msg = 'Object type must be DDLS' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " parse_file_to_object — path prefix is stripped
  "--------------------------------------------------------------------
  METHOD test_parse_file_with_path.
    DATA lv_type TYPE string.
    DATA lv_name TYPE string.

    mo_cut->parse_file_to_object(
      EXPORTING iv_file     = 'src/zcl_foo.clas.abap'
      IMPORTING ev_obj_type = lv_type
                ev_obj_name = lv_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_name exp = 'ZCL_FOO'
      msg = 'Path prefix must be stripped; name must be ZCL_FOO' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " parse_file_to_object — invalid extension → exports are initial
  "--------------------------------------------------------------------
  METHOD test_parse_file_invalid.
    DATA lv_type TYPE string.
    DATA lv_name TYPE string.

    mo_cut->parse_file_to_object(
      EXPORTING iv_file     = 'invalid.xyz'
      IMPORTING ev_obj_type = lv_type
                ev_obj_name = lv_name ).

    cl_abap_unit_assert=>assert_initial(
      act = lv_type msg = 'Type must be initial for invalid file' ).
    cl_abap_unit_assert=>assert_initial(
      act = lv_name msg = 'Name must be initial for invalid file' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " pull — empty URL returns failure immediately
  "--------------------------------------------------------------------
  METHOD test_pull_no_url.
    DATA(ls_result) = mo_cut->zif_abgagt_agent~pull(
      iv_url = '' ).

    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Pull with empty URL must fail' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = ls_result-message exp = '*URL is required*'
      msg = 'Error message must mention URL' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " pull — repo injected, log ok → success
  "--------------------------------------------------------------------
  METHOD test_pull_success.
    DATA ls_check TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    cl_abap_testdouble=>configure_call( mo_repo )->returning( ls_check ).
    mo_repo->deserialize_checks( ).

    DATA(ls_result) = mo_cut->zif_abgagt_agent~pull(
      iv_url = 'https://example.com/repo.git' ).

    cl_abap_unit_assert=>assert_true(
      act = ls_result-success
      msg = 'Pull must succeed when log has no errors' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = ls_result-message exp = '*Pull completed successfully*'
      msg = 'Success message expected' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " pull — log reports error → success = false, error_detail populated
  "--------------------------------------------------------------------
  METHOD test_pull_with_errors.
    mo_log->mv_status = zif_abapgit_log=>c_status-error.
    DATA ls_msg TYPE zif_abapgit_log=>ty_log_out.
    ls_msg-type = 'E'.
    ls_msg-text = 'Syntax error in class'.
    APPEND ls_msg TO mo_log->mt_messages.

    DATA ls_check TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    cl_abap_testdouble=>configure_call( mo_repo )->returning( ls_check ).
    mo_repo->deserialize_checks( ).

    DATA(ls_result) = mo_cut->zif_abgagt_agent~pull(
      iv_url = 'https://example.com/repo.git' ).

    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Pull must fail when log contains errors' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = ls_result-error_detail exp = '*Syntax error in class*'
      msg = 'Error detail must contain log message text' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " pull — S log entry with obj_type/obj_name → activated_count = 1
  "--------------------------------------------------------------------
  METHOD test_pull_activated_objs.
    DATA ls_msg TYPE zif_abapgit_log=>ty_log_out.
    ls_msg-type     = 'S'.
    ls_msg-text     = 'Activated'.
    ls_msg-obj_type = 'CLAS'.
    ls_msg-obj_name = 'ZCL_MY_CLASS'.
    APPEND ls_msg TO mo_log->mt_messages.

    DATA ls_check TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    cl_abap_testdouble=>configure_call( mo_repo )->returning( ls_check ).
    mo_repo->deserialize_checks( ).

    DATA(ls_result) = mo_cut->zif_abgagt_agent~pull(
      iv_url = 'https://example.com/repo.git' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-activated_count exp = 1
      msg = 'activated_count must be 1' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-activated_objects[ 1 ]-obj_type exp = 'CLAS'
      msg = 'Activated object type must be CLAS' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-activated_objects[ 1 ]-obj_name exp = 'ZCL_MY_CLASS'
      msg = 'Activated object name must be ZCL_MY_CLASS' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " pull — E log entry with obj_type/obj_name → failed_count = 1
  "--------------------------------------------------------------------
  METHOD test_pull_failed_objects.
    mo_log->mv_status = zif_abapgit_log=>c_status-error.
    DATA ls_msg TYPE zif_abapgit_log=>ty_log_out.
    ls_msg-type     = 'E'.
    ls_msg-text     = 'Activation failed'.
    ls_msg-obj_type = 'CLAS'.
    ls_msg-obj_name = 'ZCL_BAD'.
    APPEND ls_msg TO mo_log->mt_messages.

    DATA ls_check TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    cl_abap_testdouble=>configure_call( mo_repo )->returning( ls_check ).
    mo_repo->deserialize_checks( ).

    DATA(ls_result) = mo_cut->zif_abgagt_agent~pull(
      iv_url = 'https://example.com/repo.git' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-failed_count exp = 1
      msg = 'failed_count must be 1' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-failed_objects[ 1 ]-obj_name exp = 'ZCL_BAD'
      msg = 'Failed object name must be ZCL_BAD' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " pull — transport request is echoed back in result
  "--------------------------------------------------------------------
  METHOD test_pull_transport.
    DATA ls_check TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    cl_abap_testdouble=>configure_call( mo_repo )->returning( ls_check ).
    mo_repo->deserialize_checks( ).

    DATA(ls_result) = mo_cut->zif_abgagt_agent~pull(
      iv_url               = 'https://example.com/repo.git'
      iv_transport_request = 'DEVK900001' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-transport_request exp = 'DEVK900001'
      msg = 'Transport request must be echoed in result' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " pull — conflict detected but mode = 'ignore' → pull proceeds, success
  "--------------------------------------------------------------------
  METHOD test_pull_conflict_ignored.
    DATA lt_conflicts TYPE zif_abgagt_conflict_detector=>ty_conflicts.
    DATA ls_conflict  TYPE zif_abgagt_conflict_detector=>ty_conflict.
    ls_conflict-obj_type      = 'CLAS'.
    ls_conflict-obj_name      = 'ZCL_CONFLICTED'.
    ls_conflict-conflict_type = 'SYSTEM_EDIT'.
    APPEND ls_conflict TO lt_conflicts.

    " Override default (empty) conflict return to return a real conflict
    cl_abap_testdouble=>configure_call( mo_det )->returning( lt_conflicts ).
    mo_det->check_conflicts( it_files = VALUE #( ) iv_branch = '' ).

    DATA ls_check TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    cl_abap_testdouble=>configure_call( mo_repo )->returning( ls_check ).
    mo_repo->deserialize_checks( ).

    DATA(ls_result) = mo_cut->zif_abgagt_agent~pull(
      iv_url           = 'https://example.com/repo.git'
      iv_conflict_mode = 'ignore' ).

    cl_abap_unit_assert=>assert_true(
      act = ls_result-success
      msg = 'Pull must succeed when conflict_mode = ignore' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " get_repo_status — repo injected → 'Found'
  "--------------------------------------------------------------------
  METHOD test_get_repo_status_found.
    DATA(lv_status) = mo_cut->zif_abgagt_agent~get_repo_status(
      iv_url = 'https://example.com/repo.git' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_status exp = 'Found'
      msg = 'Status must be Found when repo is injected' ).
  ENDMETHOD.

ENDCLASS.
