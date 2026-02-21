*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_command_create DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_create.

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
    " Step 1: Create test double for repo service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 2: Create mock repo
    DATA lo_mock_repo TYPE REF TO zif_abapgit_repo.
    lo_mock_repo ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO' ).

    " Step 3: Configure new_online to return mock repo
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->returning( lo_mock_repo ).

    " Step 4: Call method to register the configuration
    lo_repo_srv_double->new_online(
      EXPORTING
        iv_url            = 'https://github.com/test/repo.git'
        iv_branch_name    = 'main'
        iv_display_name   = 'Test'
        iv_name           = 'test'
        iv_package        = '$ZTEST'
        iv_folder_logic   = 'PREFIX' ).

    " Step 5: Configure get_key method
    cl_abap_testdouble=>configure_call( lo_mock_repo )->returning( 'TEST_KEY' ).
    lo_mock_repo->get_key( ).

    " Step 6: Configure get_name method
    cl_abap_testdouble=>configure_call( lo_mock_repo )->returning( 'Test Repo' ).
    lo_mock_repo->get_name( ).

    " Step 7: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_create( io_repo_srv = lo_repo_srv_double ).

    " Step 8: Execute
    DATA: BEGIN OF ls_param,
            url      TYPE string VALUE 'https://github.com/test/repo.git',
            branch   TYPE string VALUE 'main',
            package  TYPE string VALUE '$ZTEST',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"success":"X"*' ).
  ENDMETHOD.

  METHOD test_create_repo_error.
    " Step 1: Create test double for repo service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 2: Create mock repo for return
    DATA lo_mock_repo TYPE REF TO zif_abapgit_repo.
    lo_mock_repo ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO' ).

    " Step 3: Configure to return mock repo
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->returning( lo_mock_repo ).

    " Step 4: Call method to register configuration
    lo_repo_srv_double->new_online(
      EXPORTING
        iv_url            = 'https://github.com/test/repo.git'
        iv_branch_name    = 'main'
        iv_display_name   = 'Test'
        iv_name           = 'test'
        iv_package        = '$ZTEST'
        iv_folder_logic   = 'PREFIX' ).

    " Step 5: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_create( io_repo_srv = lo_repo_srv_double ).

    " Step 6: Execute
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
