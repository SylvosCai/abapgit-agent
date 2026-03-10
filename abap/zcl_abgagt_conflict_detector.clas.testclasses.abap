*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_conflict_detector DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA go_env TYPE REF TO if_osql_test_environment.
    DATA mo_cut TYPE REF TO zif_abgagt_conflict_detector.

    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS setup.

    " --- check_conflicts ---
    METHODS test_no_baseline_no_conflict    FOR TESTING.
    METHODS test_git_only_changed_safe      FOR TESTING.
    METHODS test_sys_only_local_edit        FOR TESTING.
    METHODS test_both_changed_type1         FOR TESTING.
    METHODS test_branch_switch_type2        FOR TESTING.
    METHODS test_branch_switch_same_sha     FOR TESTING.
    METHODS test_multiple_objects_mixed     FOR TESTING.
    METHODS test_brsw_same_user_safe               FOR TESTING.
    METHODS test_brsw_diff_user_abort              FOR TESTING.
    METHODS test_sys_idempotent_safe               FOR TESTING.

    " --- store_pull_metadata / read back via check ---
    METHODS test_store_creates_baseline     FOR TESTING.
    METHODS test_store_updates_existing     FOR TESTING.

    " --- get_conflict_report ---
    METHODS test_report_type1_contains_key  FOR TESTING.
    METHODS test_report_type2_contains_key  FOR TESTING.
    METHODS test_report_local_edit_key      FOR TESTING.
    METHODS test_report_empty_no_conflict   FOR TESTING.

    " --- calculate_sha ---
    METHODS test_sha_same_content           FOR TESTING.
    METHODS test_sha_different_content      FOR TESTING.
    METHODS test_sha_empty_content          FOR TESTING.

    " helpers
    METHODS make_file
      IMPORTING iv_obj_type      TYPE tadir-object
                iv_obj_name      TYPE tadir-obj_name
                iv_content       TYPE string
                iv_local_content TYPE string OPTIONAL
      RETURNING VALUE(rs_file) TYPE zif_abgagt_conflict_detector=>ty_file_entry.

    METHODS make_meta_row
      IMPORTING iv_obj_type      TYPE tadir-object
                iv_obj_name      TYPE tadir-obj_name
                iv_git_sha       TYPE string
                iv_branch        TYPE string
                iv_sys_ts        TYPE timestampl
                iv_last_pulled_by TYPE syuname OPTIONAL
      RETURNING VALUE(rs_row)   TYPE zabgagt_obj_meta.

ENDCLASS.

CLASS ltcl_conflict_detector IMPLEMENTATION.

  METHOD class_setup.
    go_env = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'ZABGAGT_OBJ_META' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    go_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    go_env->clear_doubles( ).
    mo_cut = zcl_abgagt_conflict_detector=>get_instance( ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — baseline missing → no conflict (bootstrap)
  "-------------------------------------------------------------------
  METHOD test_no_baseline_no_conflict.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.

    APPEND make_file( iv_obj_type = 'CLAS'
                      iv_obj_name = 'ZCL_TEST'
                      iv_content  = 'some source' ) TO lt_files.

    " No data inserted into ZABGAGT_OBJ_META → no baseline
    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'main' ).

    cl_abap_unit_assert=>assert_initial(
      act = lt_conflicts
      msg = 'First pull with no baseline must not report conflicts' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — git changed, same branch, system unchanged → safe
  " local_content SHA equals stored SHA → sys not changed
  "-------------------------------------------------------------------
  METHOD test_git_only_changed_safe.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lv_new_content  TYPE string VALUE 'new content v2'.
    DATA lv_old_content  TYPE string VALUE 'old content v1'.
    DATA(lv_sha_old) = mo_cut->calculate_sha( lv_old_content ).
    DATA(lv_sys_ts)  = CONV timestampl( '20260305103000.0000000' ).

    " local_content matches old baseline → system unchanged
    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_TEST'
                      iv_content       = lv_new_content
                      iv_local_content = lv_old_content ) TO lt_files.

    " Baseline: stored SHA = SHA of old content
    DATA lt_meta TYPE STANDARD TABLE OF zabgagt_obj_meta.
    APPEND make_meta_row( iv_obj_type = 'CLAS'
                          iv_obj_name = 'ZCL_TEST'
                          iv_git_sha  = lv_sha_old
                          iv_branch   = 'main'
                          iv_sys_ts   = lv_sys_ts ) TO lt_meta.
    go_env->insert_test_data( i_data = lt_meta ).

    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'main' ).

    cl_abap_unit_assert=>assert_initial(
      act = lt_conflicts
      msg = 'Git-only change (fast-forward) must not be a conflict' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — system changed, git unchanged → LOCAL_EDIT conflict
  " local_content differs from stored SHA → sys was modified in ADT
  "-------------------------------------------------------------------
  METHOD test_sys_only_local_edit.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lv_git_content   TYPE string VALUE 'same content as git'.
    DATA lv_local_content TYPE string VALUE 'modified locally in ADT'.
    DATA(lv_sha)          = mo_cut->calculate_sha( lv_git_content ).
    DATA(lv_sys_ts)       = CONV timestampl( '20260305103000.0000000' ).

    " local_content differs → system was modified
    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_TEST'
                      iv_content       = lv_git_content
                      iv_local_content = lv_local_content ) TO lt_files.

    DATA lt_meta TYPE STANDARD TABLE OF zabgagt_obj_meta.
    APPEND make_meta_row( iv_obj_type = 'CLAS'
                          iv_obj_name = 'ZCL_TEST'
                          iv_git_sha  = lv_sha
                          iv_branch   = 'main'
                          iv_sys_ts   = lv_sys_ts ) TO lt_meta.
    go_env->insert_test_data( i_data = lt_meta ).

    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'main' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_conflicts ) exp = 1
      msg = 'Local system edit must be reported as conflict' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_conflicts[ 1 ]-conflict_type exp = 'LOCAL_EDIT'
      msg = 'Conflict type must be LOCAL_EDIT' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — BOTH changed, same branch → Type 1 SYSTEM_EDIT
  " local_content differs AND remote SHA also differs from baseline
  "-------------------------------------------------------------------
  METHOD test_both_changed_type1.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lv_new_git_content   TYPE string VALUE 'new git content'.
    DATA lv_local_adt_content TYPE string VALUE 'locally edited in ADT'.
    DATA lv_sha_old           TYPE string VALUE 'oldshavalue00000000000000000000000000000'.
    DATA(lv_sys_ts_baseline)  = CONV timestampl( '20260305103000.0000000' ).

    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_TEST'
                      iv_content       = lv_new_git_content
                      iv_local_content = lv_local_adt_content ) TO lt_files.

    DATA lt_meta TYPE STANDARD TABLE OF zabgagt_obj_meta.
    APPEND make_meta_row( iv_obj_type = 'CLAS'
                          iv_obj_name = 'ZCL_TEST'
                          iv_git_sha  = lv_sha_old
                          iv_branch   = 'main'
                          iv_sys_ts   = lv_sys_ts_baseline ) TO lt_meta.
    go_env->insert_test_data( i_data = lt_meta ).

    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'main' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_conflicts ) exp = 1
      msg = 'Exactly 1 conflict expected' ).

    DATA(ls_conflict) = lt_conflicts[ 1 ].
    cl_abap_unit_assert=>assert_equals(
      act = ls_conflict-conflict_type exp = 'SYSTEM_EDIT'
      msg = 'Conflict type must be SYSTEM_EDIT' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_conflict-obj_type exp = 'CLAS'
      msg = 'Object type must match' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_conflict-obj_name exp = 'ZCL_TEST'
      msg = 'Object name must match' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_conflict-git_sha_old
      msg = 'Old git SHA must be populated' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_conflict-git_sha_new
      msg = 'New git SHA must be populated' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — git changed + branch switched, different user → BRANCH_SWITCH
  " last_pulled_by is a different user → unexpected branch state, must abort
  " local_content SHA equals stored SHA → sys NOT changed (isolates branch switch)
  "-------------------------------------------------------------------
  METHOD test_branch_switch_type2.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lv_new_content   TYPE string VALUE 'content on branch b2'.
    DATA lv_old_content   TYPE string VALUE 'content on branch b1'.
    DATA(lv_sha_old)      = mo_cut->calculate_sha( lv_old_content ).
    DATA(lv_sys_ts)       = CONV timestampl( '20260305103000.0000000' ).

    " local_content = old content → system unchanged
    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_TEST'
                      iv_content       = lv_new_content
                      iv_local_content = lv_old_content ) TO lt_files.

    DATA lt_meta TYPE STANDARD TABLE OF zabgagt_obj_meta.
    APPEND make_meta_row( iv_obj_type      = 'CLAS'
                          iv_obj_name      = 'ZCL_TEST'
                          iv_git_sha       = lv_sha_old
                          iv_branch        = 'b1'
                          iv_sys_ts        = lv_sys_ts
                          iv_last_pulled_by = 'OTHER_USER' ) TO lt_meta.
    go_env->insert_test_data( i_data = lt_meta ).

    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'b2' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_conflicts ) exp = 1
      msg = 'Exactly 1 conflict expected for branch switch by a different user' ).

    DATA(ls_conflict) = lt_conflicts[ 1 ].
    cl_abap_unit_assert=>assert_equals(
      act = ls_conflict-conflict_type exp = 'BRANCH_SWITCH'
      msg = 'Conflict type must be BRANCH_SWITCH' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_conflict-branch_old exp = 'b1'
      msg = 'Old branch must be populated' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_conflict-branch_new exp = 'b2'
      msg = 'New branch must be populated' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — branch switched but git content identical → safe
  "-------------------------------------------------------------------
  METHOD test_branch_switch_same_sha.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lv_content TYPE string VALUE 'identical content on both branches'.
    DATA(lv_sha)    = mo_cut->calculate_sha( lv_content ).
    DATA(lv_sys_ts) = CONV timestampl( '20260305103000.0000000' ).

    " local_content same as remote → system unchanged, git also unchanged
    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_TEST'
                      iv_content       = lv_content
                      iv_local_content = lv_content ) TO lt_files.

    DATA lt_meta TYPE STANDARD TABLE OF zabgagt_obj_meta.
    APPEND make_meta_row( iv_obj_type = 'CLAS'
                          iv_obj_name = 'ZCL_TEST'
                          iv_git_sha  = lv_sha
                          iv_branch   = 'b1'
                          iv_sys_ts   = lv_sys_ts ) TO lt_meta.
    go_env->insert_test_data( i_data = lt_meta ).

    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'b2' ).

    cl_abap_unit_assert=>assert_initial(
      act = lt_conflicts
      msg = 'Branch switch with identical content must not be a conflict' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — multiple objects: 1 conflict, 1 safe
  "-------------------------------------------------------------------
  METHOD test_multiple_objects_mixed.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lt_meta  TYPE STANDARD TABLE OF zabgagt_obj_meta.
    DATA(lv_sys_ts_base) = CONV timestampl( '20260305103000.0000000' ).

    " Object 1: BOTH changed → SYSTEM_EDIT conflict
    " local_content differs from stored SHA AND remote SHA also differs
    DATA lv_content1       TYPE string VALUE 'changed git content'.
    DATA lv_local_content1 TYPE string VALUE 'modified in ADT'.
    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_CONFLICT'
                      iv_content       = lv_content1
                      iv_local_content = lv_local_content1 ) TO lt_files.
    APPEND make_meta_row( iv_obj_type = 'CLAS'
                          iv_obj_name = 'ZCL_CONFLICT'
                          iv_git_sha  = 'oldshaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
                          iv_branch   = 'main'
                          iv_sys_ts   = lv_sys_ts_base ) TO lt_meta.

    " Object 2: git only changed → safe
    " local_content SHA equals stored baseline SHA → sys not changed
    DATA lv_old_content2 TYPE string VALUE 'old content of safe object'.
    DATA(lv_sha_safe)    = mo_cut->calculate_sha( lv_old_content2 ).
    DATA lv_new_content2 TYPE string VALUE 'updated content from git'.
    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_SAFE'
                      iv_content       = lv_new_content2
                      iv_local_content = lv_old_content2 ) TO lt_files.
    APPEND make_meta_row( iv_obj_type = 'CLAS'
                          iv_obj_name = 'ZCL_SAFE'
                          iv_git_sha  = lv_sha_safe
                          iv_branch   = 'main'
                          iv_sys_ts   = lv_sys_ts_base ) TO lt_meta.

    go_env->insert_test_data( i_data = lt_meta ).

    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'main' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_conflicts ) exp = 1
      msg = 'Only ZCL_CONFLICT should be reported' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_conflicts[ 1 ]-obj_name exp = 'ZCL_CONFLICT'
      msg = 'Conflicting object name mismatch' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " store_pull_metadata → creates baseline in ZABGAGT_OBJ_META
  "-------------------------------------------------------------------
  METHOD test_store_creates_baseline.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lv_content TYPE string VALUE 'class source code'.

    APPEND make_file( iv_obj_type = 'CLAS'
                      iv_obj_name = 'ZCL_STORE_TEST'
                      iv_content  = lv_content ) TO lt_files.

    mo_cut->store_pull_metadata(
      it_files  = lt_files
      iv_branch = 'main' ).

    " Read back directly from the doubled table
    SELECT SINGLE last_git_sha, last_branch, last_pulled_at
      FROM zabgagt_obj_meta
      WHERE obj_type = 'CLAS' AND obj_name = 'ZCL_STORE_TEST'
      INTO @DATA(ls_stored).

    cl_abap_unit_assert=>assert_subrc( msg = 'Row must exist in ZABGAGT_OBJ_META after store' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_stored-last_git_sha
      msg = 'Git SHA must be stored' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_stored-last_branch exp = 'main'
      msg = 'Branch must be stored' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_stored-last_pulled_at
      msg = 'Pull timestamp must be stored' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " store_pull_metadata → updates existing row (MODIFY = upsert)
  "-------------------------------------------------------------------
  METHOD test_store_updates_existing.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.

    APPEND make_file( iv_obj_type = 'CLAS'
                      iv_obj_name = 'ZCL_UPD_TEST'
                      iv_content  = 'version 1' ) TO lt_files.
    mo_cut->store_pull_metadata( it_files = lt_files iv_branch = 'main' ).

    SELECT SINGLE last_git_sha FROM zabgagt_obj_meta
      WHERE obj_type = 'CLAS' AND obj_name = 'ZCL_UPD_TEST'
      INTO @DATA(lv_sha_v1).

    " Store new version
    CLEAR lt_files.
    APPEND make_file( iv_obj_type = 'CLAS'
                      iv_obj_name = 'ZCL_UPD_TEST'
                      iv_content  = 'version 2 changed' ) TO lt_files.
    mo_cut->store_pull_metadata( it_files = lt_files iv_branch = 'feature/x' ).

    SELECT SINGLE last_git_sha, last_branch FROM zabgagt_obj_meta
      WHERE obj_type = 'CLAS' AND obj_name = 'ZCL_UPD_TEST'
      INTO @DATA(ls_v2).

    cl_abap_unit_assert=>assert_differs(
      act = ls_v2-last_git_sha exp = lv_sha_v1
      msg = 'SHA must be updated on second store' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_v2-last_branch exp = 'feature/x'
      msg = 'Branch must be updated on second store' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " get_conflict_report — Type 1 report contains key labels
  "-------------------------------------------------------------------
  METHOD test_report_type1_contains_key.
    DATA lt_conflicts TYPE zif_abgagt_conflict_detector=>ty_conflicts.
    DATA ls_conflict  TYPE zif_abgagt_conflict_detector=>ty_conflict.

    ls_conflict-conflict_type  = 'SYSTEM_EDIT'.
    ls_conflict-obj_type       = 'CLAS'.
    ls_conflict-obj_name       = 'ZCL_REPORT_TEST'.
    ls_conflict-git_sha_old    = 'abc123'.
    ls_conflict-git_sha_new    = 'def456'.
    ls_conflict-sys_changed_by = 'DEVELOPER2'.
    ls_conflict-last_pulled_by = 'ME'.
    APPEND ls_conflict TO lt_conflicts.

    DATA(lv_report) = mo_cut->get_conflict_report( lt_conflicts ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_report exp = '*ZCL_REPORT_TEST*'
      msg = 'Report must contain object name' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_report exp = '*SYSTEM_EDIT*'
      msg = 'Report must contain conflict type' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_report exp = '*DEVELOPER2*'
      msg = 'Report must contain who changed in system' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " get_conflict_report — Type 2 report contains branch labels
  "-------------------------------------------------------------------
  METHOD test_report_type2_contains_key.
    DATA lt_conflicts TYPE zif_abgagt_conflict_detector=>ty_conflicts.
    DATA ls_conflict  TYPE zif_abgagt_conflict_detector=>ty_conflict.

    ls_conflict-conflict_type  = 'BRANCH_SWITCH'.
    ls_conflict-obj_type       = 'CLAS'.
    ls_conflict-obj_name       = 'ZCL_BRANCH_TEST'.
    ls_conflict-branch_old     = 'b1'.
    ls_conflict-branch_new     = 'b2'.
    ls_conflict-last_pulled_by = 'ME'.
    APPEND ls_conflict TO lt_conflicts.

    DATA(lv_report) = mo_cut->get_conflict_report( lt_conflicts ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_report exp = '*ZCL_BRANCH_TEST*'
      msg = 'Report must contain object name' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_report exp = '*b1*'
      msg = 'Report must contain old branch' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_report exp = '*b2*'
      msg = 'Report must contain new branch' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " get_conflict_report — LOCAL_EDIT report contains key labels
  "-------------------------------------------------------------------
  METHOD test_report_local_edit_key.
    DATA lt_conflicts TYPE zif_abgagt_conflict_detector=>ty_conflicts.
    DATA ls_conflict  TYPE zif_abgagt_conflict_detector=>ty_conflict.

    ls_conflict-conflict_type  = 'LOCAL_EDIT'.
    ls_conflict-obj_type       = 'CLAS'.
    ls_conflict-obj_name       = 'ZCL_LOCAL_TEST'.
    ls_conflict-last_pulled_by = 'PULLER'.
    APPEND ls_conflict TO lt_conflicts.

    DATA(lv_report) = mo_cut->get_conflict_report( lt_conflicts ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_report exp = '*ZCL_LOCAL_TEST*'
      msg = 'Report must contain object name' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_report exp = '*LOCAL_EDIT*'
      msg = 'Report must contain conflict type' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_report exp = '*overwrite*'
      msg = 'Report must warn about overwrite' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " get_conflict_report — empty conflicts → empty/initial report
  "-------------------------------------------------------------------
  METHOD test_report_empty_no_conflict.
    DATA lt_conflicts TYPE zif_abgagt_conflict_detector=>ty_conflicts.

    DATA(lv_report) = mo_cut->get_conflict_report( lt_conflicts ).

    cl_abap_unit_assert=>assert_initial(
      act = lv_report
      msg = 'Report for empty conflict list must be initial' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " calculate_sha — same content → same hash
  "-------------------------------------------------------------------
  METHOD test_sha_same_content.
    DATA(lv_sha1) = mo_cut->calculate_sha( 'CLASS zcl_test DEFINITION.' ).
    DATA(lv_sha2) = mo_cut->calculate_sha( 'CLASS zcl_test DEFINITION.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_sha1 exp = lv_sha2
      msg = 'Same content must produce same SHA' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_sha1
      msg = 'SHA must not be empty' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " calculate_sha — different content → different hash
  "-------------------------------------------------------------------
  METHOD test_sha_different_content.
    DATA(lv_sha1) = mo_cut->calculate_sha( 'content version 1' ).
    DATA(lv_sha2) = mo_cut->calculate_sha( 'content version 2' ).

    cl_abap_unit_assert=>assert_differs(
      act = lv_sha1 exp = lv_sha2
      msg = 'Different content must produce different SHA' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " calculate_sha — empty string → deterministic non-empty hash
  "-------------------------------------------------------------------
  METHOD test_sha_empty_content.
    DATA(lv_sha1) = mo_cut->calculate_sha( '' ).
    DATA(lv_sha2) = mo_cut->calculate_sha( '' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_sha1 exp = lv_sha2
      msg = 'Empty content must produce consistent SHA' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_sha1
      msg = 'SHA for empty content must not be empty' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — branch switched + git changed, same user → safe (no conflict)
  " Same developer intentionally switched branches — must not abort.
  "-------------------------------------------------------------------
  METHOD test_brsw_same_user_safe.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lv_new_content TYPE string VALUE 'content on feature branch'.
    DATA lv_old_content TYPE string VALUE 'content on main branch'.
    DATA(lv_sha_old)    = mo_cut->calculate_sha( lv_old_content ).
    DATA(lv_sys_ts)     = CONV timestampl( '20260305103000.0000000' ).

    " local_content = old content → system unchanged
    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_TEST'
                      iv_content       = lv_new_content
                      iv_local_content = lv_old_content ) TO lt_files.

    DATA lt_meta TYPE STANDARD TABLE OF zabgagt_obj_meta.
    " last_pulled_by = sy-uname (same user who is now pulling)
    APPEND make_meta_row( iv_obj_type       = 'CLAS'
                          iv_obj_name       = 'ZCL_TEST'
                          iv_git_sha        = lv_sha_old
                          iv_branch         = 'main'
                          iv_sys_ts         = lv_sys_ts
                          iv_last_pulled_by = sy-uname ) TO lt_meta.
    go_env->insert_test_data( i_data = lt_meta ).

    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'feature/my-work' ).

    cl_abap_unit_assert=>assert_initial(
      act = lt_conflicts
      msg = 'Same user switching branches intentionally must not cause a conflict' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — branch switched + git changed, different user → BRANCH_SWITCH
  " A colleague last pulled from a different branch — unexpected state, must abort.
  "-------------------------------------------------------------------
  METHOD test_brsw_diff_user_abort.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lv_new_content TYPE string VALUE 'content on feature branch'.
    DATA lv_old_content TYPE string VALUE 'content on main branch'.
    DATA(lv_sha_old)    = mo_cut->calculate_sha( lv_old_content ).
    DATA(lv_sys_ts)     = CONV timestampl( '20260305103000.0000000' ).

    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_TEST'
                      iv_content       = lv_new_content
                      iv_local_content = lv_old_content ) TO lt_files.

    DATA lt_meta TYPE STANDARD TABLE OF zabgagt_obj_meta.
    " last_pulled_by = a colleague, not the current user
    APPEND make_meta_row( iv_obj_type       = 'CLAS'
                          iv_obj_name       = 'ZCL_TEST'
                          iv_git_sha        = lv_sha_old
                          iv_branch         = 'main'
                          iv_sys_ts         = lv_sys_ts
                          iv_last_pulled_by = 'COLLEAGUE' ) TO lt_meta.
    go_env->insert_test_data( i_data = lt_meta ).

    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'feature/my-work' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_conflicts ) exp = 1
      msg = 'Different user last pulled from another branch — must report BRANCH_SWITCH' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_conflicts[ 1 ]-conflict_type exp = 'BRANCH_SWITCH'
      msg = 'Conflict type must be BRANCH_SWITCH' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_conflicts[ 1 ]-last_pulled_by exp = 'COLLEAGUE'
      msg = 'Conflict must record who last pulled' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " check_conflicts — system already has incoming content → idempotent pull
  " ADT edit → export to git → commit → pull: local_sha = current_sha → no conflict.
  "-------------------------------------------------------------------
  METHOD test_sys_idempotent_safe.
    DATA lt_files TYPE zif_abgagt_conflict_detector=>ty_file_entries.
    DATA lv_new_git_content   TYPE string VALUE 'new content committed to git'.
    DATA lv_old_baseline_sha  TYPE string VALUE 'oldshavalue00000000000000000000000000000'.
    DATA(lv_sys_ts)           = CONV timestampl( '20260305103000.0000000' ).

    " local_content = new_git_content → system already has the incoming version
    APPEND make_file( iv_obj_type      = 'CLAS'
                      iv_obj_name      = 'ZCL_TEST'
                      iv_content       = lv_new_git_content
                      iv_local_content = lv_new_git_content ) TO lt_files.

    " Baseline still points to old SHA → git changed, sys_changed also true
    " but local_sha = current_sha → idempotent early exit fires
    DATA lt_meta TYPE STANDARD TABLE OF zabgagt_obj_meta.
    APPEND make_meta_row( iv_obj_type = 'CLAS'
                          iv_obj_name = 'ZCL_TEST'
                          iv_git_sha  = lv_old_baseline_sha
                          iv_branch   = 'main'
                          iv_sys_ts   = lv_sys_ts ) TO lt_meta.
    go_env->insert_test_data( i_data = lt_meta ).

    DATA(lt_conflicts) = mo_cut->check_conflicts(
      it_files  = lt_files
      iv_branch = 'main' ).

    cl_abap_unit_assert=>assert_initial(
      act = lt_conflicts
      msg = 'System already has incoming content — pull is idempotent, no conflict' ).
  ENDMETHOD.

  "-------------------------------------------------------------------
  " Helpers
  "-------------------------------------------------------------------
  METHOD make_file.
    rs_file-obj_type      = iv_obj_type.
    rs_file-obj_name      = iv_obj_name.
    rs_file-content       = iv_content.
    rs_file-local_content = iv_local_content.
  ENDMETHOD.

  METHOD make_meta_row.
    rs_row-obj_type        = iv_obj_type.
    rs_row-obj_name        = iv_obj_name.
    rs_row-last_git_sha    = iv_git_sha.
    rs_row-last_branch     = iv_branch.
    rs_row-sys_changed_at  = iv_sys_ts.
    rs_row-last_pulled_by  = iv_last_pulled_by.
  ENDMETHOD.

ENDCLASS.
