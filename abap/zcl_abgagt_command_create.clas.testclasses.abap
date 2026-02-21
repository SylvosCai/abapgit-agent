*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_command_create DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_create.
    DATA mo_repo_srv_double TYPE REF TO cl_abap_testdouble.

    METHODS setup.
    METHODS test_get_name FOR TESTING.
    METHODS test_missing_url FOR TESTING.
    METHODS test_missing_package FOR TESTING.
    METHODS test_create_repo_success FOR TESTING.
    METHODS test_create_repo_error FOR TESTING.
ENDCLASS.

CLASS ltcl_zcl_abgagt_command_create IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_name.
    DATA(lv_name) = mo_cut->zif_abgagt_command~get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'CREATE'
      msg = 'Command name should be CREATE' ).
  ENDMETHOD.

  METHOD test_missing_url.
    DATA: BEGIN OF ls_param,
            url     TYPE string VALUE '',
            package TYPE string VALUE '$ZTEST',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"URL is required"*' ).
  ENDMETHOD.

  METHOD test_missing_package.
    DATA: BEGIN OF ls_param,
            url     TYPE string VALUE 'https://github.com/test/repo.git',
            package TYPE string VALUE '',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"Package is required"*' ).
  ENDMETHOD.

  METHOD test_create_repo_success.
    " Create test double for repo service interface
    mo_repo_srv_double = cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_create( io_repo_srv = mo_repo_srv_double ).

    " Create mock repo object
    DATA(lo_mock_repo) = cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_ONLINE' ).

    " Configure new_online to return our mock repo
    cl_abap_testdouble=>configure_call( mo_repo_srv_double )->
      new_online(
        iv_url            = 'https://github.com/test/repo.git'
        iv_branch_name    = 'main'
        iv_display_name   = 'Test'
        iv_name           = 'test'
        iv_package        = '$ZTEST'
        iv_folder_logic   = 'PREFIX'
      )->returning( lo_mock_repo ).

    " Configure mock repo methods used in response
    cl_abap_testdouble=>configure_call( lo_mock_repo )->
      get_key( )->returning( 'TEST_KEY' ).
    cl_abap_testdouble=>configure_call( lo_mock_repo )->
      get_name( )->returning( 'Test Repo' ).

    " Execute
    DATA: BEGIN OF ls_param,
            url      TYPE string VALUE 'https://github.com/test/repo.git',
            branch   TYPE string VALUE 'main',
            package  TYPE string VALUE '$ZTEST',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert success response
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"success":"X"*' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"repo_key":"TEST_KEY"*' ).
  ENDMETHOD.

  METHOD test_create_repo_error.
    " Create test double for repo service interface
    mo_repo_srv_double = cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_create( io_repo_srv = mo_repo_srv_double ).

    " Configure mock to raise exception
    DATA(lx_error) = NEW zcx_abapgit_exception( text = 'Repository creation failed' ).
    cl_abap_testdouble=>configure_call( mo_repo_srv_double )->
      new_online(
        iv_url            = 'https://github.com/test/repo.git'
        iv_branch_name    = 'main'
        iv_display_name   = 'Test'
        iv_name           = 'test'
        iv_package        = '$ZTEST'
        iv_folder_logic   = 'PREFIX'
      )->raise( lx_error ).

    " Execute
    DATA: BEGIN OF ls_param,
            url     TYPE string VALUE 'https://github.com/test/repo.git',
            branch  TYPE string VALUE 'main',
            package TYPE string VALUE '$ZTEST',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert error response
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error"*' ).
  ENDMETHOD.

ENDCLASS.
