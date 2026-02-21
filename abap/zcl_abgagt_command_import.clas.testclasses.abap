*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_command_import DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_import.

    METHODS setup.
    METHODS test_get_name FOR TESTING.
    METHODS test_missing_url FOR TESTING.
    METHODS test_import_success FOR TESTING.
    METHODS test_repo_not_found FOR TESTING.
ENDCLASS.

CLASS ltcl_zcl_abgagt_command_import IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_name.
    DATA(lv_name) = mo_cut->zif_abgagt_command~get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'IMPORT'
      msg = 'Command name should be IMPORT' ).
  ENDMETHOD.

  METHOD test_missing_url.
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE '',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"URL is required"*' ).
  ENDMETHOD.

  METHOD test_import_success.
    " Step 1: Create test doubles
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 2: Create mock repo
    DATA lo_mock_repo TYPE REF TO zif_abapgit_repo.
    lo_mock_repo ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO' ).

    " Step 3: Configure get_repo_from_url to return mock repo
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->returning( lo_mock_repo ).
    lo_repo_srv_double->get_repo_from_url( EXPORTING iv_url = 'https://github.com/test/repo.git' ).

    " Step 4: Configure repo methods
    " get_package
    cl_abap_testdouble=>configure_call( lo_mock_repo )->returning( value = '$ZTEST' ).
    lo_mock_repo->get_package( ).

    " refresh
    lo_mock_repo->refresh( ).

    " get_files_local - return empty table (no files)
    DATA lt_files TYPE zif_abapgit_definitions=>ty_files_item_tt.
    cl_abap_testdouble=>configure_call( lo_mock_repo )->returning( value = lt_files ).
    lo_mock_repo->get_files_local( ).

    " Step 5: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_import( io_repo_srv = lo_repo_srv_double ).

    " Step 6: Execute
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE 'https://github.com/test/repo.git',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert - should return error because no files
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"No objects found*' ).
  ENDMETHOD.

  METHOD test_repo_not_found.
    " Step 1: Create test double for repo service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 2: Configure get_repo_from_url to return no repo (initial)
    DATA lo_initial TYPE REF TO zif_abapgit_repo.
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->returning( lo_initial ).
    lo_repo_srv_double->get_repo_from_url( EXPORTING iv_url = 'https://github.com/notfound/repo.git' ).

    " Step 3: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_import( io_repo_srv = lo_repo_srv_double ).

    " Step 4: Execute
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE 'https://github.com/notfound/repo.git',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert - should return error because repo not found
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"Repository not found"*' ).
  ENDMETHOD.

ENDCLASS.
