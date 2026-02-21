*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_command_import DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_import.

    METHODS setup.
    METHODS test_get_name FOR TESTING.
    METHODS test_missing_url FOR TESTING.
    METHODS test_repo_not_found FOR TESTING.
    METHODS test_exception FOR TESTING.
    METHODS test_no_objects_found FOR TESTING.
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

  METHOD test_repo_not_found.
    " Step 1: Create test double for repo service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 2: Configure get_repo_from_url - do not configure returning
    " The method will return ei_repo as not bound (initial)
    cl_abap_testdouble=>configure_call( lo_repo_srv_double ).

    " Step 3: Register the method call with matching parameters
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/repo.git' ).

    " Step 4: Create CUT with test double injected
    mo_cut = NEW zcl_abgagt_command_import( io_repo_srv = lo_repo_srv_double ).

    " Step 5: Execute
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE 'https://github.com/test/repo.git',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert - repo not found because ei_repo is not bound
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"Repository not found"*' ).
  ENDMETHOD.

  METHOD test_exception.
    " Step 1: Create test double for repo service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 2: Configure to raise exception
    DATA lx_error TYPE REF TO zcx_abapgit_exception.
    CREATE OBJECT lx_error.
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->raise_exception( lx_error ).

    " Step 3: Register the method call with matching parameters
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/repo.git' ).

    " Step 4: Create CUT with test double injected
    mo_cut = NEW zcl_abgagt_command_import( io_repo_srv = lo_repo_srv_double ).

    " Step 5: Execute
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE 'https://github.com/test/repo.git',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert - error should be in result
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"*' ).
  ENDMETHOD.

  METHOD test_no_objects_found.
    " Step 1: Create test double for online repo (needed for cast at line 106)
    DATA lo_repo_double TYPE REF TO zif_abapgit_repo_online.
    lo_repo_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_ONLINE' ).

    " Step 2: Configure get_package to return a package (use parent interface prefix)
    DATA lv_package TYPE devclass VALUE '$TEST'.
    cl_abap_testdouble=>configure_call( lo_repo_double )->returning( lv_package ).
    lo_repo_double->zif_abapgit_repo~get_package( ).

    " Step 3: Configure refresh (no parameters) - use parent interface prefix
    cl_abap_testdouble=>configure_call( lo_repo_double ).
    lo_repo_double->zif_abapgit_repo~refresh( ).

    " Step 4: Configure get_files_local to return empty table - use parent interface prefix
    DATA lt_empty_files TYPE zif_abapgit_definitions=>ty_files_item_tt.
    cl_abap_testdouble=>configure_call( lo_repo_double )->returning( lt_empty_files ).
    lo_repo_double->zif_abapgit_repo~get_files_local( ).

    " Step 5: Create test double for repo service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Step 6: Configure get_repo_from_url to return the repo via set_parameter
    cl_abap_testdouble=>configure_call( lo_repo_srv_double )->set_parameter(
      EXPORTING
        name  = 'EI_REPO'
        value = lo_repo_double ).

    " Step 7: Register the method call with matching parameters
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/repo.git' ).

    " Step 8: Create CUT with test double
    mo_cut = NEW zcl_abgagt_command_import( io_repo_srv = lo_repo_srv_double ).

    " Step 9: Execute
    DATA: BEGIN OF ls_param,
            url TYPE string VALUE 'https://github.com/test/repo.git',
          END OF ls_param.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    " Assert - no objects found error
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"No objects found in package"*' ).
  ENDMETHOD.

ENDCLASS.
