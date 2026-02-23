*"* use this source file for your test class implementation
*"* local test class
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
    " Step 1: Create test double for repo service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 2: Create mock repo
    DATA lo_mock_repo TYPE REF TO zif_abapgit_repo.
    lo_mock_repo ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO' ).

    " Step 3: Configure get_repo_from_url to return mock repo
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->returning( lo_mock_repo ).
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/repo.git'
      IMPORTING ei_repo = lo_mock_repo ).

    " Step 4: Configure get_key method
    cl_abap_testdouble=>configure_call( lo_mock_repo )->returning( 'TEST_KEY' ).
    lo_mock_repo->get_key( ).

    " Step 5: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_delete( io_repo_srv = lo_repo_srv_double ).

    " Step 6: Execute
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
    " Step 1: Create test double for repo service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 2: Create mock repo
    DATA lo_mock_repo TYPE REF TO zif_abapgit_repo.
    lo_mock_repo ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO' ).

    " Step 3: Configure get_repo_from_url to return mock repo
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->returning( lo_mock_repo ).
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/repo.git'
      IMPORTING ei_repo = lo_mock_repo ).

    " Step 4: Configure delete on repo service to raise exception
    DATA(lx_error) = NEW zcx_abapgit_exception( ).
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->raise_exception( lx_error ).
    lo_repo_srv_double->delete( ii_repo = lo_mock_repo ).

    " Step 5: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_delete( io_repo_srv = lo_repo_srv_double ).

    " Step 6: Execute
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
    " Step 1: Create test double for repo service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 2: Create mock repo for the IMPORTING parameter
    DATA lo_mock_repo TYPE REF TO zif_abapgit_repo.
    lo_mock_repo ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO' ).

    " Step 3: Configure get_repo_from_url to raise exception (repo not found)
    DATA(lx_error) = NEW zcx_abapgit_exception( ).
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->raise_exception( lx_error ).
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/nonexistent.git'
      IMPORTING ei_repo = lo_mock_repo ).

    " Step 4: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_delete( io_repo_srv = lo_repo_srv_double ).

    " Step 5: Execute
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
