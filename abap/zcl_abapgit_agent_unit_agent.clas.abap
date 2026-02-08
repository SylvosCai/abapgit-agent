*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_unit_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS run_tests
      IMPORTING
        iv_package TYPE devclass OPTIONAL
        it_objects TYPE ty_object_list OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ty_result.

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

    " Run tests using CL_AUNIT_API
    DATA lt_results TYPE ty_test_results.
    DATA lv_total_tests TYPE i VALUE 0.
    DATA lv_passed_tests TYPE i VALUE 0.
    DATA lv_failed_tests TYPE i VALUE 0.

    LOOP AT lt_objects INTO DATA(ls_object).
      DATA lt_obj_results TYPE ty_test_results.
      lt_obj_results = run_tests_for_object( ls_object-object_type, ls_object-object_name ).

      APPEND LINES OF lt_obj_results TO lt_results.

      lv_total_tests = lv_total_tests + lines( lt_obj_results ).
      LOOP AT lt_obj_results INTO DATA(ls_result).
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
    rs_result-results = lt_results.

    IF lv_failed_tests > 0.
      rs_result-success = abap_false.
    ENDIF.

    IF lv_total_tests = 0.
      rs_result-message = 'No unit tests found'.
    ELSE.
      rs_result-message = |{ lv_total_tests } tests: { lv_passed_tests } passed, { lv_failed_tests } failed|.
    ENDIF.
  ENDMETHOD.

  METHOD get_test_objects_from_package.
    " Query TADIR for classes in the package
    SELECT obj_name
      FROM tadir
      INTO TABLE @rt_objects
      WHERE pgmid = 'R3TR'
        AND object = 'CLAS'
        AND devclass = @iv_package
        AND obj_name LIKE 'Z%'.
  ENDMETHOD.

  METHOD run_tests_for_object.
    DATA: lo_aunit TYPE REF TO cl_aunit_api,
          lt_tasks TYPE if_aunit_types=>tt_task,
          lt_result TYPE if_aunit_types=>tt_result,
          ls_result TYPE ty_test_result.

    " Create AUnit API instance
    lo_aunit = cl_aunit_api=>create( ).

    " Add test class to task
    DATA(ls_task) = VALUE if_aunit_types=>ts_task( class = iv_object_name ).

    " Get task for the class
    lo_aunit->get_tasks(
      EXPORTING
        iv_classname = iv_object_name
      RECEIVING
        rt_tasks = lt_tasks ).

    " Run tasks
    lo_aunit->execute(
      EXPORTING
        it_tasks = lt_tasks
      RECEIVING
        rt_results = lt_result ).

    " Convert results
    LOOP AT lt_result INTO DATA(ls_aunit_result).
      ls_result-object_type = iv_object_type.
      ls_result-object_name = iv_object_name.
      ls_result-method_name = ls_aunit_result-method.

      IF ls_aunit_result-kind = if_aunit_constants=>critical.
        ls_result-passed = abap_false.
        ls_result-error = ls_aunit_result-message.
      ELSE.
        ls_result-passed = abap_true.
      ENDIF.

      APPEND ls_result TO rt_results.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
