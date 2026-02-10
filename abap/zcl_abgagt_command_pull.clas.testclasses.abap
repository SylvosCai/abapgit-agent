*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_zcl_abgagt_command_pull DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_pull.

    METHODS setup.
    METHODS test_get_name FOR TESTING.
    METHODS test_exec_minimal FOR TESTING.
    METHODS test_exec_files FOR TESTING.
    METHODS test_interface FOR TESTING.
ENDCLASS.

CLASS ltcl_zcl_abgagt_command_pull IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_name.
    " Test that get_name returns the correct command name
    DATA(lv_name) = mo_cut->zif_abgagt_command~get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = zif_abgagt_command=>gc_pull
      msg = 'Command name should be PULL' ).
  ENDMETHOD.

  METHOD test_interface.
    " Test that the class implements the command interface
    DATA lo_interface TYPE REF TO zif_abgagt_command.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_pull.
    lo_interface = mo_cut.
    cl_abap_unit_assert=>assert_bound(
      act = lo_interface
      msg = 'Object should implement zif_abgagt_command interface' ).
  ENDMETHOD.

  METHOD test_exec_minimal.
    " Test execute with minimal parameters (url only)
    DATA: BEGIN OF ls_param,
            url TYPE string,
          END OF ls_param.

    ls_param-url = 'https://github.test/repo.git'.

    " Execute should return a JSON string
    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should be valid JSON (starts with { and ends with })
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*{*'
      msg = 'Result should be valid JSON object' ).
  ENDMETHOD.

  METHOD test_exec_files.
    " Test execute with files parameter
    DATA: BEGIN OF ls_param,
            url TYPE string,
            files TYPE string_table,
          END OF ls_param.

    ls_param-url = 'https://github.test/repo.git'.
    APPEND 'zcl_test.clas.abap' TO ls_param-files.
    APPEND 'zif_test.intf.abap' TO ls_param-files.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    " Result should be valid JSON
    DATA lv_json TYPE string.
    lv_json = lv_result.
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_json
      exp = '*{"success"*' " JSON-like structure
      msg = 'Result should be valid JSON' ).
  ENDMETHOD.

ENDCLASS.
