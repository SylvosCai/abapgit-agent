*"* use this source file for your test class implementation
*"* local test class

"----------------------------------------------------------------------
" Manual double for ZIF_ABAPGIT_REPO_SRV.
" get_repo_from_url returns initial (repo not found) by default.
"----------------------------------------------------------------------
CLASS ltcl_repo_srv_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_srv.
    DATA mi_repo TYPE REF TO zif_abapgit_repo.
ENDCLASS.

CLASS ltcl_repo_srv_double IMPLEMENTATION.
  METHOD zif_abapgit_repo_srv~get_repo_from_url.
    ri_repo = mi_repo.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~list.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get_repo.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~new_repo.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~delete.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~purge.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~is_repo_installed.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_zcl_abgagt_command_import DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zif_abgagt_command.

    METHODS setup.
    METHODS test_get_name FOR TESTING.
    METHODS test_url_required FOR TESTING.
    METHODS test_repo_not_found FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_zcl_abgagt_command_import IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_import.
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

    " Create test double for repository service (returns initial = not found)
    DATA lo_repo_srv_double TYPE REF TO ltcl_repo_srv_double.
    CREATE OBJECT lo_repo_srv_double TYPE ltcl_repo_srv_double.
    " mi_repo stays initial → get_repo_from_url returns initial

    DATA lo_import_cmd TYPE REF TO zcl_abgagt_command_import.
    CREATE OBJECT lo_import_cmd TYPE zcl_abgagt_command_import
      EXPORTING io_repo_srv = lo_repo_srv_double.
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
