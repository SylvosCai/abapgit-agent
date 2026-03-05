*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_command_import DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zif_abgagt_command.

    METHODS setup.
    METHODS test_get_name FOR TESTING.
    METHODS test_url_required FOR TESTING.
    METHODS test_repo_not_found FOR TESTING.
ENDCLASS.

CLASS ltcl_zcl_abgagt_command_import IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW zcl_abgagt_command_import( ).
  ENDMETHOD.

  METHOD test_get_name.
    " Test that command name is 'IMPORT'
    DATA(lv_name) = mo_cut->get_name( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'IMPORT'
      msg = 'Command name should be IMPORT' ).
  ENDMETHOD.

  METHOD test_url_required.
    " Test that command returns error when URL is missing
    DATA ls_param TYPE zcl_abgagt_command_import=>ty_import_params.
    " Leave url empty

    DATA(lv_result) = mo_cut->execute( is_param = ls_param ).

    " Assert - should return error about URL being required
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"URL is required"*'
      msg = 'Should return URL is required error' ).
  ENDMETHOD.

  METHOD test_repo_not_found.
    " Test that command returns error when repository is not found
    " Using test doubles to simulate repository service behavior

    " Create test double for repository service
    DATA lo_repo_srv_double TYPE REF TO zif_abapgit_repo_srv.
    lo_repo_srv_double ?= cl_abap_testdouble=>create( 'ZIF_ABAPGIT_REPO_SRV' ).

    " Configure test double: get_repo_from_url returns nothing (repo not found)
    cl_abap_testdouble=>configure_call( lo_repo_srv_double ).
    lo_repo_srv_double->get_repo_from_url(
      EXPORTING iv_url = 'https://github.com/test/repo.git' ).

    " Inject test double into command
    DATA lo_import_cmd TYPE REF TO zcl_abgagt_command_import.
    lo_import_cmd = NEW zcl_abgagt_command_import( io_repo_srv = lo_repo_srv_double ).
    mo_cut = lo_import_cmd.

    " Execute command with test URL
    DATA ls_param TYPE zcl_abgagt_command_import=>ty_import_params.
    ls_param-url = 'https://github.com/test/repo.git'.

    DATA(lv_result) = mo_cut->execute( is_param = ls_param ).

    " Assert - should return error about repository not found
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*"error":"Repository not found"*'
      msg = 'Should return repository not found error' ).
  ENDMETHOD.

ENDCLASS.
