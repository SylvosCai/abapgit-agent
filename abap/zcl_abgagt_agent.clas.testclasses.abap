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
" Alias-based repo double — mimics ZCL_ABAPGIT_REPO_ONLINE where
" DESERIALIZE / DESERIALIZE_CHECKS are exposed via ALIASES.
" This is the production pattern that triggered the RTTI bug:
"   methods[ name = 'DESERIALIZE' ]-parameters is EMPTY for aliases;
"   the fixed code checks ZIF_ABAPGIT_REPO~DESERIALIZE instead.
"----------------------------------------------------------------------
CLASS ltcl_repo_with_aliases DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo.
    " Declare aliases exactly as production code does
    ALIASES deserialize
      FOR zif_abapgit_repo~deserialize.
    ALIASES deserialize_checks
      FOR zif_abapgit_repo~deserialize_checks.
    ALIASES get_log
      FOR zif_abapgit_repo~get_log.
    ALIASES create_new_log
      FOR zif_abapgit_repo~create_new_log.
    " Configurable log double returned by get_log / create_new_log
    DATA mo_log TYPE REF TO ltcl_log_double.
ENDCLASS.

CLASS ltcl_repo_with_aliases IMPLEMENTATION.
  METHOD zif_abapgit_repo~get_log.
    ri_log = mo_log.
  ENDMETHOD.
  METHOD zif_abapgit_repo~create_new_log.
    ri_log = mo_log.
  ENDMETHOD.
  METHOD zif_abapgit_repo~deserialize_checks.
    rs_checks = VALUE #( ).
  ENDMETHOD.
  METHOD zif_abapgit_repo~deserialize.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_key.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_name.
  ENDMETHOD.
  METHOD zif_abapgit_repo~is_offline.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_package.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_local_settings.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_tadir_objects.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local_filtered.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_remote.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_abapgit.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_dot_abapgit.
  ENDMETHOD.
  METHOD zif_abapgit_repo~find_remote_dot_abapgit.
  ENDMETHOD.
  METHOD zif_abapgit_repo~checksums.
  ENDMETHOD.
  METHOD zif_abapgit_repo~has_remote_source.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_apack.
  ENDMETHOD.
  METHOD zif_abapgit_repo~delete_checks.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_files_remote.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_local_settings.
  ENDMETHOD.
  METHOD zif_abapgit_repo~switch_repo_type.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_object.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_objects.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_data_config.
  ENDMETHOD.
  METHOD zif_abapgit_repo~bind_listener.
  ENDMETHOD.
ENDCLASS.

"----------------------------------------------------------------------
" RTTI provider mock — returns a pre-configured cl_abap_objectdescr.
" Inject into ZCL_ABGAGT_AGENT via io_rtti to control which descriptor
" filter_param_available() sees without depending on the real class type.
"----------------------------------------------------------------------
CLASS ltcl_rtti_mock DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_rtti_provider.
    DATA mo_desc       TYPE REF TO cl_abap_objectdescr.
    DATA mo_class_desc TYPE REF TO cl_abap_classdescr.
ENDCLASS.

CLASS ltcl_rtti_mock IMPLEMENTATION.
  METHOD zif_abgagt_rtti_provider~describe_object.
    ro_desc = mo_desc.
  ENDMETHOD.
  METHOD zif_abgagt_rtti_provider~describe_class.
    ro_desc = mo_class_desc.
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
    METHODS test_pull_no_url           FOR TESTING RAISING zcx_abapgit_exception.
    METHODS test_pull_success          FOR TESTING RAISING zcx_abapgit_exception.
    METHODS test_pull_with_errors      FOR TESTING RAISING zcx_abapgit_exception.
    METHODS test_pull_activated_objs   FOR TESTING RAISING zcx_abapgit_exception.
    METHODS test_pull_failed_objects   FOR TESTING RAISING zcx_abapgit_exception.
    METHODS test_pull_transport        FOR TESTING RAISING zcx_abapgit_exception.
    METHODS test_pull_conflict_ignored FOR TESTING RAISING zcx_abapgit_exception.

    " --- RTTI alias parameter detection ---
    METHODS test_rtti_alias_filter_param FOR TESTING.
    METHODS test_pull_alias_repo_filter  FOR TESTING RAISING zcx_abapgit_exception.

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

    zcl_abgagt_util=>get_instance( )->parse_file_to_object(
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

    zcl_abgagt_util=>get_instance( )->parse_file_to_object(
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

    zcl_abgagt_util=>get_instance( )->parse_file_to_object(
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

    zcl_abgagt_util=>get_instance( )->parse_file_to_object(
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

    zcl_abgagt_util=>get_instance( )->parse_file_to_object(
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

  "--------------------------------------------------------------------
  " RTTI alias detection — verifies filter_param_available() logic.
  "
  " Confirms that ZIF_ABAPGIT_REPO~DESERIALIZE exposes II_OBJ_FILTER
  " in the RTTI of ltcl_repo_with_aliases (which uses ALIASES just
  " like the production ZCL_ABAPGIT_REPO_ONLINE does), and that the
  " alias entry DESERIALIZE also appears in the methods table.
  "--------------------------------------------------------------------
  METHOD test_rtti_alias_filter_param.
    DATA lo_alias_repo TYPE REF TO ltcl_repo_with_aliases.
    lo_alias_repo = NEW ltcl_repo_with_aliases( ).

    DATA(lo_descr) = CAST cl_abap_objectdescr(
                       cl_abap_typedescr=>describe_by_object_ref( lo_alias_repo ) ).

    " The interface-prefixed name MUST have II_OBJ_FILTER — this is
    " what filter_param_available() checks.
    DATA(lv_prefixed_has_filter) = xsdbool(
      line_exists(
        lo_descr->methods[
          name = 'ZIF_ABAPGIT_REPO~DESERIALIZE' ]-parameters[
          name = 'II_OBJ_FILTER' ] ) ).

    cl_abap_unit_assert=>assert_true(
      act = lv_prefixed_has_filter
      msg = 'ZIF_ABAPGIT_REPO~DESERIALIZE must expose II_OBJ_FILTER in RTTI' ).

    " The alias entry itself MUST appear in the methods table
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( line_exists(
              lo_descr->methods[ name = 'DESERIALIZE' ] ) )
      msg = 'Alias DESERIALIZE must appear in the methods table' ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " End-to-end pull with alias-based repo — verifies filtered path.
  "
  " filter_param_available() calls describe_by_object_ref( mo_repo )
  " directly. By injecting ltcl_repo_with_aliases as io_repo, the
  " RTTI check sees the real descriptor of the alias-based class and
  " detects II_OBJ_FILTER on ZIF_ABAPGIT_REPO~DESERIALIZE — taking
  " the filtered dynamic CALL METHOD path.
  "--------------------------------------------------------------------
  METHOD test_pull_alias_repo_filter.
    DATA lo_alias_log TYPE REF TO ltcl_log_double.
    lo_alias_log = NEW ltcl_log_double( ).
    lo_alias_log->mv_status = zif_abapgit_log=>c_status-ok.

    DATA lo_alias_repo TYPE REF TO ltcl_repo_with_aliases.
    lo_alias_repo = NEW ltcl_repo_with_aliases( ).
    lo_alias_repo->mo_log = lo_alias_log.

    " Wire RTTI mock with the real descriptor of the alias-based repo.
    " This simulates the production RTTI lookup (ZCL_ABAPGIT_REPO_ONLINE)
    " without depending on the actual installed abapGit version.
    DATA lo_rtti_mock TYPE REF TO ltcl_rtti_mock.
    lo_rtti_mock = NEW ltcl_rtti_mock( ).
    lo_rtti_mock->mo_desc = CAST cl_abap_objectdescr(
                              cl_abap_typedescr=>describe_by_object_ref( lo_alias_repo ) ).
    lo_rtti_mock->mo_class_desc = CAST cl_abap_classdescr(
                                    cl_abap_typedescr=>describe_by_name( 'ZCL_ABAPGIT_OBJECT_FILTER_OBJ' ) ).

    DATA lo_det TYPE REF TO zif_abgagt_conflict_detector.
    lo_det ?= cl_abap_testdouble=>create( 'ZIF_ABGAGT_CONFLICT_DETECTOR' ).
    DATA lt_no_conflicts TYPE zif_abgagt_conflict_detector=>ty_conflicts.
    cl_abap_testdouble=>configure_call( lo_det )->returning( lt_no_conflicts ).
    lo_det->check_conflicts( it_files = VALUE #( ) iv_branch = '' ).

    DATA lo_cut TYPE REF TO zcl_abgagt_agent.
    lo_cut = NEW zcl_abgagt_agent(
      io_repo              = lo_alias_repo
      io_conflict_detector = lo_det
      io_rtti              = lo_rtti_mock ).

    DATA(ls_result) = lo_cut->zif_abgagt_agent~pull(
      iv_url   = 'https://example.com/repo.git'
      it_files = VALUE string_table( ( `src/zcl_foo.clas.abap` ) ) ).

    cl_abap_unit_assert=>assert_true(
      act = ls_result-success
      msg = 'Pull via alias-based repo with --files must succeed' ).
  ENDMETHOD.

ENDCLASS.
