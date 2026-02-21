*"* use this source file for your test class implementation
*"* local test class
CLASS ltcl_cmd_tree DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abgagt_command_tree.
    CLASS-DATA: go_env TYPE REF TO if_osql_test_environment.

    CLASS-METHODS: class_setup, class_teardown.
    METHODS: setup.
    METHODS: test_get_name FOR TESTING.
    METHODS: test_interface FOR TESTING.
    METHODS: test_exec_no_package FOR TESTING.
    METHODS: test_exec_package_not_found FOR TESTING.
    METHODS: test_exec_single_package FOR TESTING.
    METHODS: test_exec_with_subpackages FOR TESTING.
    METHODS: test_exec_with_include_objects FOR TESTING.
    METHODS: test_exec_depth_limit FOR TESTING.
    METHODS: test_build_tree_method FOR TESTING.
    METHODS: test_get_object_count_method FOR TESTING.
    METHODS: test_get_obj_counts_by_type FOR TESTING.
ENDCLASS.

CLASS ltcl_cmd_tree IMPLEMENTATION.

  METHOD class_setup.
    go_env = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'TDEVC' )
        ( 'TADIR' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    go_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    go_env->clear_doubles( ).
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_get_name.
    DATA(lv_name) = mo_cut->zif_abgagt_command~get_name( ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = zif_abgagt_command=>gc_tree
      msg = 'Command name should be TREE' ).
  ENDMETHOD.

  METHOD test_interface.
    DATA lo_interface TYPE REF TO zif_abgagt_command.
    CREATE OBJECT mo_cut TYPE zcl_abgagt_command_tree.
    lo_interface = mo_cut.
    cl_abap_unit_assert=>assert_bound(
      act = lo_interface
      msg = 'Object should implement zif_abgagt_command interface' ).
  ENDMETHOD.

  METHOD test_exec_no_package.
    " Test execute with no package - should return error
    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
          END OF ls_param.

    ls_param-package = ''.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Result should indicate failure' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*Package*required*'
      msg = 'Error message should mention package required' ).
  ENDMETHOD.

  METHOD test_exec_package_not_found.
    " Test execute with non-existent package
    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
          END OF ls_param.

    ls_param-package = '$NONEXISTENT'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*false*'
      msg = 'Result should indicate failure' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*does not exist*'
      msg = 'Error message should mention package does not exist' ).
  ENDMETHOD.

  METHOD test_exec_single_package.
    " Test execute with valid package but no subpackages
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    lt_tdevc = VALUE #( ( devclass = '$TEST' parentcl = '' ) ).
    go_env->insert_test_data( i_data = lt_tdevc ).

    " Insert TADIR entries for the package
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    lt_tadir = VALUE #(
      ( object = 'CLAS' obj_name = 'ZCL_TEST' devclass = '$TEST' )
      ( object = 'INTF' obj_name = 'ZIF_TEST' devclass = '$TEST' )
      ( object = 'PROG' obj_name = 'ZTEST_PROG' devclass = '$TEST' ) ).
    go_env->insert_test_data( i_data = lt_tadir ).

    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
          END OF ls_param.

    ls_param-package = '$TEST'.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*$TEST*'
      msg = 'Result should contain package name' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*total_packages*1*'
      msg = 'Should have 1 package' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*total_objects*3*'
      msg = 'Should have 3 objects' ).
  ENDMETHOD.

  METHOD test_exec_with_subpackages.
    " Test execute with subpackages
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    lt_tdevc = VALUE #(
      ( devclass = '$PARENT' parentcl = '$SAP' )
      ( devclass = '$CHILD1' parentcl = '$PARENT' )
      ( devclass = '$CHILD2' parentcl = '$PARENT' )
      ( devclass = '$GRANDCHILD' parentcl = '$CHILD1' ) ).
    go_env->insert_test_data( i_data = lt_tdevc ).

    " Insert TADIR entries
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    lt_tadir = VALUE #(
      ( object = 'CLAS' obj_name = 'ZCL_PARENT' devclass = '$PARENT' )
      ( object = 'CLAS' obj_name = 'ZCL_CHILD1' devclass = '$CHILD1' )
      ( object = 'CLAS' obj_name = 'ZCL_CHILD2' devclass = '$CHILD2' )
      ( object = 'CLAS' obj_name = 'ZCL_GRANDCHILD' devclass = '$GRANDCHILD' ) ).
    go_env->insert_test_data( i_data = lt_tadir ).

    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
            depth TYPE i,
          END OF ls_param.

    ls_param-package = '$PARENT'.
    ls_param-depth = 3.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*total_packages*4*'
      msg = 'Should have 4 packages (parent + 2 children + 1 grandchild)' ).
  ENDMETHOD.

  METHOD test_exec_with_include_objects.
    " Test execute with include_objects flag
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    lt_tdevc = VALUE #( ( devclass = '$TEST' parentcl = '$SAP' ) ).
    go_env->insert_test_data( i_data = lt_tdevc ).

    " Insert TADIR entries
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    lt_tadir = VALUE #(
      ( object = 'CLAS' obj_name = 'ZCL_TEST1' devclass = '$TEST' )
      ( object = 'CLAS' obj_name = 'ZCL_TEST2' devclass = '$TEST' )
      ( object = 'INTF' obj_name = 'ZIF_TEST' devclass = '$TEST' ) ).
    go_env->insert_test_data( i_data = lt_tadir ).

    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
            depth TYPE i,
            include_objects TYPE abap_bool,
          END OF ls_param.

    ls_param-package = '$TEST'.
    ls_param-depth = 1.
    ls_param-include_objects = abap_true.

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*objects*CLAS*'
      msg = 'Should include CLAS in object counts' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*objects*INTF*'
      msg = 'Should include INTF in object counts' ).
  ENDMETHOD.

  METHOD test_exec_depth_limit.
    " Test that depth is limited to 10
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    lt_tdevc = VALUE #(
      ( devclass = '$PARENT' parentcl = '$SAP' )
      ( devclass = '$CHILD' parentcl = '$PARENT' ) ).
    go_env->insert_test_data( i_data = lt_tdevc ).

    DATA: BEGIN OF ls_param,
            package TYPE tdevc-devclass,
            depth TYPE i,
          END OF ls_param.

    ls_param-package = '$PARENT'.
    ls_param-depth = 100. " Should be limited to 10

    DATA(lv_result) = mo_cut->zif_abgagt_command~execute( is_param = ls_param ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_result
      msg = 'Result should not be initial' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*success*true*'
      msg = 'Result should indicate success' ).
  ENDMETHOD.

  METHOD test_build_tree_method.
    " Test build_tree method directly
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    lt_tdevc = VALUE #( ( devclass = '$TEST' parentcl = '$SAP' ) ).
    go_env->insert_test_data( i_data = lt_tdevc ).

    " Insert TADIR entries
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    lt_tadir = VALUE #(
      ( object = 'CLAS' obj_name = 'ZCL_TEST' devclass = '$TEST' ) ).
    go_env->insert_test_data( i_data = lt_tadir ).

    DATA ls_params TYPE zcl_abgagt_command_tree=>ty_tree_params.
    ls_params-package = '$TEST'.
    ls_params-depth = 1.
    ls_params-include_objects = abap_false.

    DATA(ls_result) = mo_cut->build_tree( ls_params ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-success
      exp = abap_true
      msg = 'Build tree should succeed' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-package
      exp = '$TEST'
      msg = 'Package should match' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-total_packages
      exp = 1
      msg = 'Should have 1 package' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-total_objects
      exp = 1
      msg = 'Should have 1 object' ).
  ENDMETHOD.

  METHOD test_get_object_count_method.
    " Test get_object_count method
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    lt_tdevc = VALUE #( ( devclass = '$TEST' parentcl = '$SAP' ) ).
    go_env->insert_test_data( i_data = lt_tdevc ).

    " Insert TADIR entries (excluding DEVC and PACK)
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    lt_tadir = VALUE #(
      ( object = 'CLAS' obj_name = 'ZCL_TEST1' devclass = '$TEST' )
      ( object = 'CLAS' obj_name = 'ZCL_TEST2' devclass = '$TEST' )
      ( object = 'DEVC' obj_name = '$TEST' devclass = '$TEST' ) ). " Should not be counted
    go_env->insert_test_data( i_data = lt_tadir ).

    DATA(lv_count) = mo_cut->get_object_count( '$TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_count
      exp = 2
      msg = 'Should count only CLAS objects (2), not DEVC' ).
  ENDMETHOD.

  METHOD test_get_obj_counts_by_type.
    " Test get_object_counts_by_type method
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.
    lt_tdevc = VALUE #( ( devclass = '$TEST' parentcl = '$SAP' ) ).
    go_env->insert_test_data( i_data = lt_tdevc ).

    " Insert TADIR entries with different object types
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    lt_tadir = VALUE #(
      ( object = 'CLAS' obj_name = 'ZCL_TEST1' devclass = '$TEST' )
      ( object = 'CLAS' obj_name = 'ZCL_TEST2' devclass = '$TEST' )
      ( object = 'CLAS' obj_name = 'ZCL_TEST3' devclass = '$TEST' )
      ( object = 'INTF' obj_name = 'ZIF_TEST' devclass = '$TEST' )
      ( object = 'INTF' obj_name = 'ZIF_TEST2' devclass = '$TEST' )
      ( object = 'PROG' obj_name = 'ZTEST_PROG' devclass = '$TEST' ) ).
    go_env->insert_test_data( i_data = lt_tadir ).

    DATA lt_counts TYPE zcl_abgagt_command_tree=>ty_object_counts.
    mo_cut->get_object_counts_by_type(
      EXPORTING iv_package = '$TEST'
      CHANGING ct_counts = lt_counts ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_counts )
      exp = 3
      msg = 'Should have 3 object types' ).

    " Check CLAS count
    READ TABLE lt_counts WITH KEY object = 'CLAS' INTO DATA(ls_clas).
    cl_abap_unit_assert=>assert_equals(
      act = ls_clas-count
      exp = 3
      msg = 'CLAS count should be 3' ).

    " Check INTF count
    READ TABLE lt_counts WITH KEY object = 'INTF' INTO DATA(ls_intf).
    cl_abap_unit_assert=>assert_equals(
      act = ls_intf-count
      exp = 2
      msg = 'INTF count should be 2' ).

    " Check PROG count
    READ TABLE lt_counts WITH KEY object = 'PROG' INTO DATA(ls_prog).
    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-count
      exp = 1
      msg = 'PROG count should be 1' ).
  ENDMETHOD.

ENDCLASS.
