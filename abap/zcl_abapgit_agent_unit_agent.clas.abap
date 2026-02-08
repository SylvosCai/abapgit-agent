*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_unit_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_test_result,
             object_type TYPE string,
             object_name TYPE string,
             method_name TYPE string,
             passed TYPE abap_bool,
             error TYPE string,
           END OF ty_test_result.

    TYPES ty_test_results TYPE STANDARD TABLE OF ty_test_result WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             test_count TYPE i,
             passed_count TYPE i,
             failed_count TYPE i,
             message TYPE string,
             results TYPE ty_test_results,
           END OF ty_result.

    TYPES: BEGIN OF ty_object,
             object_type TYPE string,
             object_name TYPE string,
           END OF ty_object.

    TYPES ty_object_list TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

    METHODS run_tests
      IMPORTING
        iv_package TYPE devclass OPTIONAL
        it_objects TYPE ty_object_list OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ty_result.

ENDCLASS.

CLASS zcl_abapgit_agent_unit_agent IMPLEMENTATION.

  METHOD run_tests.
    DATA lt_objects TYPE ty_object_list.

    rs_result-success = abap_true.

    " If package is provided, get all test objects from package
    IF iv_package IS NOT INITIAL.
      lt_objects = get_test_objects_from_package( iv_package ).
    ENDIF.

    " If specific objects are provided, add them to the list
    IF it_objects IS NOT INITIAL.
      APPEND LINES OF it_objects TO lt_objects.
    ENDIF.

    " Remove duplicates
    SORT lt_objects BY object_type object_name.
    DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING object_type object_name.

    IF lt_objects IS INITIAL.
      rs_result-success = abap_false.
      rs_result-message = 'No test objects found'.
      RETURN.
    ENDIF.

    " Run tests for each object
    DATA lt_all_results TYPE ty_test_results.
    DATA lv_total_tests TYPE i VALUE 0.
    DATA lv_passed_tests TYPE i VALUE 0.
    DATA lv_failed_tests TYPE i VALUE 0.

    LOOP AT lt_objects INTO DATA(ls_object).
      DATA lt_results TYPE ty_test_results.
      lt_results = run_tests_for_object(
        iv_object_type = ls_object-object_type
        iv_object_name = ls_object-object_name ).

      APPEND LINES OF lt_results TO lt_all_results.

      lv_total_tests = lv_total_tests + lines( lt_results ).
      LOOP AT lt_results INTO DATA(ls_result).
        IF ls_result-passed = abap_true.
          lv_passed_tests = lv_passed_tests + 1.
        ELSE.
          lv_failed_tests = lv_failed_tests + 1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    rs_result-test_count = lv_total_tests.
    rs_result-passed_count = lv_passed_tests.
    rs_result-failed_count = lv_failed_tests.
    rs_result-results = lt_all_results.

    IF lv_failed_tests > 0.
      rs_result-success = abap_false.
    ENDIF.

    IF lv_total_tests = 0.
      rs_result-message = 'No unit tests found in specified objects'.
    ELSE.
      rs_result-message = |{ lv_total_tests } tests executed: { lv_passed_tests } passed, { lv_failed_tests } failed|.
    ENDIF.

  ENDMETHOD.

  METHOD get_test_objects_from_package.
    DATA: BEGIN OF ls_obj,
            object_type TYPE string,
            object_name TYPE string,
          END OF ls_obj,
          lt_all_objects TYPE STANDARD TABLE OF ls_obj.

    " Query TADIR for classes in the package
    SELECT object AS object_type, obj_name AS object_name
      FROM tadir
      INTO TABLE @lt_all_objects
      WHERE pgmid = 'R3TR'
        AND object = 'CLAS'
        AND devclass = @iv_package
        AND obj_name LIKE 'Z%'.

    " Filter for classes that have test methods
    LOOP AT lt_all_objects INTO ls_obj.
      IF class_has_test_methods( ls_obj-object_name ) = abap_true.
        APPEND ls_obj TO rt_objects.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD class_has_test_methods.
    DATA: lv_class_name TYPE seoclsname,
          lt_methods TYPE seoo_intf_r_methods.

    lv_class_name = iv_object_name.

    " Get methods from class
    CALL METHOD cl_rtti_classdat=>get_all_method_implementations
      EXPORTING
        p_classname = lv_class_name
      RECEIVING
        p_methods   = lt_methods.

    " Check if any method has FOR TESTING
    LOOP AT lt_methods INTO DATA(ls_method).
      IF ls_method-method_key-ccat = '2'. " 2 = TEST
        rv_has_tests = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_has_tests = abap_false.
  ENDMETHOD.

  METHOD run_tests_for_object.
    DATA: lv_class_name TYPE seoclsname,
          lt_methods TYPE seoo_intf_r_methods,
          ls_result TYPE ty_test_result.

    lv_class_name = iv_object_name.

    " Get all test methods from the class
    CALL METHOD cl_rtti_classdat=>get_all_method_implementations
      EXPORTING
        p_classname = lv_class_name
      RECEIVING
        p_methods   = lt_methods.

    " Filter for test methods only
    LOOP AT lt_methods INTO DATA(ls_method) WHERE method_key-ccat = '2'. " TEST methods
      ls_result-object_type = iv_object_type.
      ls_result-object_name = iv_object_name.
      ls_result-method_name = ls_method-method_key-cpname.

      " Try to run the test method
      IF execute_test_method( lv_class_name = lv_class_name
                             iv_method_name = ls_method-method_key-cpname
                             ) = abap_true.
        ls_result-passed = abap_true.
      ELSE.
        ls_result-passed = abap_false.
        ls_result-error = 'Test method failed'.
      ENDIF.

      APPEND ls_result TO rt_results.
    ENDLOOP.

  ENDMETHOD.

  METHOD execute_test_method.
    DATA: lo_test TYPE REF TO object,
          lx_error TYPE REF TO cx_root.

    " Create instance of the test class
    CREATE OBJECT lo_test TYPE (lv_class_name).

    " Try to call the test method
    TRY.
        CALL METHOD lo_test->(iv_method_name).
        rv_success = abap_true.
      CATCH cx_root INTO lx_error.
        rv_success = abap_false.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
