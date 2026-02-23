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

    " Step 3: Configure get_repo_from_url to return mock repo via set_parameter
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->set_parameter(
      EXPORTING
        name  = 'EI_REPO'
        value = lo_mock_repo ).

    " Step 4: Register the method call with matching parameters
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/repo.git' ).

    " Step 5: Configure get_key method
    cl_abap_testdouble=>configure_call( lo_mock_repo )->returning( 'TEST_KEY' ).
    lo_mock_repo->get_key( ).

    " Step 6: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_delete( io_repo_srv = lo_repo_srv_double ).

    " Step 7: Execute
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
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->set_parameter(
      EXPORTING
        name  = 'EI_REPO'
        value = lo_mock_repo ).

    " Step 4: Register the method call with matching parameters
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/repo.git' ).

    " Step 5: Configure delete on repo service to raise exception
    DATA lx_error TYPE REF TO zcx_abapgit_exception.
    CREATE OBJECT lx_error.
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->raise_exception( lx_error ).

    " Step 6: Register delete call
    lo_repo_srv_double->delete( ii_repo = lo_mock_repo ).

    " Step 7: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_delete( io_repo_srv = lo_repo_srv_double ).

    " Step 8: Execute
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

    " Step 2: Configure to raise exception
    DATA lx_error TYPE REF TO zcx_abapgit_exception.
    CREATE OBJECT lx_error.
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->raise_exception( lx_error ).

    " Step 3: Register the method call with matching parameters
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/nonexistent.git' ).

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
