*"* use this source file for your test class implementation
*"* local test class

"--------------------------------------------------------------------
" Manual double for ZIF_ABAPGIT_REPO_SRV
"--------------------------------------------------------------------
CLASS ltcl_repo_srv_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_srv.
    DATA mo_repo          TYPE REF TO zif_abapgit_repo.
    DATA mv_get_url_error TYPE abap_bool VALUE abap_false.
    DATA mv_delete_error  TYPE abap_bool VALUE abap_false.
ENDCLASS.

CLASS ltcl_repo_srv_double IMPLEMENTATION.
  METHOD zif_abapgit_repo_srv~get_repo_from_url.
    IF mv_get_url_error = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.
    ei_repo = mo_repo.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~delete.
    IF mv_delete_error = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~init. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~is_repo_installed. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~list. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~list_favorites. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~new_offline. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~new_online. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~purge. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~validate_package. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~validate_url. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get_repo_from_package. ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get_label_list. ENDMETHOD.
ENDCLASS.

"--------------------------------------------------------------------
" Manual double for ZIF_ABAPGIT_REPO
"--------------------------------------------------------------------
CLASS ltcl_repo_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo.
    DATA mv_key TYPE zif_abapgit_persistence=>ty_value.
ENDCLASS.

CLASS ltcl_repo_double IMPLEMENTATION.
  METHOD zif_abapgit_repo~get_key.
    rv_key = mv_key.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_name. ENDMETHOD.
  METHOD zif_abapgit_repo~is_offline. ENDMETHOD.
  METHOD zif_abapgit_repo~get_package. ENDMETHOD.
  METHOD zif_abapgit_repo~get_local_settings. ENDMETHOD.
  METHOD zif_abapgit_repo~get_tadir_objects. ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local_filtered. ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local. ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_remote. ENDMETHOD.
  METHOD zif_abapgit_repo~refresh. ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_abapgit. ENDMETHOD.
  METHOD zif_abapgit_repo~set_dot_abapgit. ENDMETHOD.
  METHOD zif_abapgit_repo~find_remote_dot_abapgit. ENDMETHOD.
  METHOD zif_abapgit_repo~deserialize. ENDMETHOD.
  METHOD zif_abapgit_repo~deserialize_checks. ENDMETHOD.
  METHOD zif_abapgit_repo~checksums. ENDMETHOD.
  METHOD zif_abapgit_repo~has_remote_source. ENDMETHOD.
  METHOD zif_abapgit_repo~get_log. ENDMETHOD.
  METHOD zif_abapgit_repo~create_new_log. ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_apack. ENDMETHOD.
  METHOD zif_abapgit_repo~delete_checks. ENDMETHOD.
  METHOD zif_abapgit_repo~set_files_remote. ENDMETHOD.
  METHOD zif_abapgit_repo~set_local_settings. ENDMETHOD.
  METHOD zif_abapgit_repo~switch_repo_type. ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_object. ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_objects. ENDMETHOD.
  METHOD zif_abapgit_repo~get_data_config. ENDMETHOD.
  METHOD zif_abapgit_repo~bind_listener. ENDMETHOD.
ENDCLASS.

"--------------------------------------------------------------------
" Test class
"--------------------------------------------------------------------
CLASS ltcl_zcl_abgagt_command_delete DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_delete.

    METHODS setup.
    METHODS test_get_name FOR TESTING.
    METHODS test_missing_url FOR TESTING.
    METHODS test_delete_by_url_success FOR TESTING RAISING zcx_abapgit_exception.
    METHODS test_delete_repo_error FOR TESTING RAISING zcx_abapgit_exception.
    METHODS test_repo_not_found FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_zcl_abgagt_command_delete IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_name.
    DATA(lv_name) = mo_cut->zif_abgagt_command~get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'DELETE'
      msg = 'Command name should be DELETE' ).
  ENDMETHOD.

  METHOD test_missing_url.
    " URL is empty - should return error
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE '',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error"*' ).
  ENDMETHOD.

  METHOD test_delete_by_url_success.
    " Step 1: Create manual double for repo service
    DATA lo_repo_srv_double TYPE REF TO ltcl_repo_srv_double.
    CREATE OBJECT lo_repo_srv_double.

    " Step 2: Create manual double for mock repo
    DATA lo_repo_double TYPE REF TO ltcl_repo_double.
    CREATE OBJECT lo_repo_double.
    lo_repo_double->mv_key = 'TEST_KEY'.

    " Step 3: Wire the repo srv double to return the mock repo
    lo_repo_srv_double->mo_repo = lo_repo_double.

    " Step 4: Create CUT with test double
    DATA lo_repo_srv TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv = lo_repo_srv_double.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_delete
      EXPORTING
        io_repo_srv = lo_repo_srv.

    " Step 5: Execute
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE 'https://github.com/test/repo.git',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert success
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"success":"X"*' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"repo_key":"TEST_KEY"*' ).
  ENDMETHOD.

  METHOD test_delete_repo_error.
    " Step 1: Create manual double for repo service
    DATA lo_repo_srv_double TYPE REF TO ltcl_repo_srv_double.
    CREATE OBJECT lo_repo_srv_double.

    " Step 2: Create manual double for mock repo
    DATA lo_repo_double TYPE REF TO ltcl_repo_double.
    CREATE OBJECT lo_repo_double.
    lo_repo_double->mv_key = 'TEST_KEY'.

    " Step 3: Wire the repo srv double to return the mock repo, but raise on delete
    lo_repo_srv_double->mo_repo = lo_repo_double.
    lo_repo_srv_double->mv_delete_error = abap_true.

    " Step 4: Create CUT with test double
    DATA lo_repo_srv TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv = lo_repo_srv_double.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_delete
      EXPORTING
        io_repo_srv = lo_repo_srv.

    " Step 5: Execute
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE 'https://github.com/test/repo.git',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert error response
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"success":""*' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error"*' ).
  ENDMETHOD.

  METHOD test_repo_not_found.
    " Step 1: Create manual double for repo service configured to raise on get_repo_from_url
    DATA lo_repo_srv_double TYPE REF TO ltcl_repo_srv_double.
    CREATE OBJECT lo_repo_srv_double.
    lo_repo_srv_double->mv_get_url_error = abap_true.

    " Step 2: Create CUT with test double
    DATA lo_repo_srv TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv = lo_repo_srv_double.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_delete
      EXPORTING
        io_repo_srv = lo_repo_srv.

    " Step 3: Execute
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE 'https://github.com/test/nonexistent.git',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert not found response
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"success":""*' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error"*' ).
  ENDMETHOD.

ENDCLASS.
