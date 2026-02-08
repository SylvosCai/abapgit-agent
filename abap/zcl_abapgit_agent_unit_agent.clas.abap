*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_unit_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_test_result,
             object_type TYPE string,
             object_name TYPE string,
             test_method TYPE string,
             status TYPE string,
             message TYPE string,
             duration TYPE i,
           END OF ty_test_result.

    TYPES ty_test_results TYPE STANDARD TABLE OF ty_test_result WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             message TYPE string,
             test_count TYPE i,
             passed_count TYPE i,
             failed_count TYPE i,
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

  PRIVATE SECTION.
    METHODS get_test_classes
      IMPORTING
        iv_package TYPE devclass OPTIONAL
        it_objects TYPE ty_object_list OPTIONAL
      RETURNING
        VALUE(rt_classes) TYPE ty_object_list.

    METHODS run_aunit_tests
      IMPORTING
        it_classes TYPE ty_object_list
      RETURNING
        VALUE(rt_results) TYPE ty_test_results.

    METHODS count_results
      IMPORTING
        it_results TYPE ty_test_results
      RETURNING
        VALUE(rs_stats) TYPE ty_result.

ENDCLASS.

CLASS zcl_abapgit_agent_unit_agent IMPLEMENTATION.

  METHOD run_tests.
    " Initialize result
    rs_result-success = abap_false.
    rs_result-test_count = 0.
    rs_result-passed_count = 0.
    rs_result-failed_count = 0.

    " Get test classes to run
    DATA(lt_test_classes) = get_test_classes(
      iv_package = iv_package
      it_objects = it_objects ).

    IF lt_test_classes IS INITIAL.
      rs_result-message = 'No test classes found'.
      RETURN.
    ENDIF.

    " Run AUNIT tests
    rs_result-results = run_aunit_tests( lt_test_classes ).

    " Count results
    rs_result = count_results( rs_result-results ).

    IF rs_result-failed_count = 0.
      rs_result-success = abap_true.
      rs_result-message = |All { rs_result-test_count } tests passed|.
    ELSE.
      rs_result-message = |{ rs_result-failed_count } of { rs_result-test_count } tests failed|.
    ENDIF.

  ENDMETHOD.

  METHOD get_test_classes.
    " Return list of test class names (CLAS names)
    DATA: ls_tadir TYPE tadir,
          lt_tadir TYPE TABLE OF tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.

    IF iv_package IS NOT INITIAL.
      " Get all test classes from package
      SELECT * FROM tadir
        INTO TABLE lt_tadir
        WHERE devclass = iv_package
          AND object = 'CLAS'
          AND obj_name LIKE '%_TEST'
        ORDER BY obj_name.

      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        APPEND INITIAL LINE TO rt_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
        <ls_class>-object_type = 'CLAS'.
        <ls_class>-object_name = <ls_tadir>-obj_name.
      ENDLOOP.
    ENDIF.

    " Add specified objects
    IF it_objects IS NOT INITIAL.
      LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
        APPEND INITIAL LINE TO rt_classes ASSIGNING <ls_class>.
        <ls_class>-object_type = <ls_obj>-object_type.
        <ls_class>-object_name = <ls_obj>-object_name.
      ENDLOOP.
    ENDIF.

    " Remove duplicates
    SORT rt_classes BY object_name.
    DELETE ADJACENT DUPLICATES FROM rt_classes COMPARING object_name.

  ENDMETHOD.

  METHOD run_aunit_tests.
    " Use CL_AUNIT_API to run tests
    DATA: lt_results TYPE cl_aunit_task=>ty_results,
          ls_result LIKE LINE OF rt_results.

    DATA(lv_program) = 'SAUNIT_UNIT_TEST_DRIVER'. " Driver program

    " Create task
    TRY.
        DATA(lr_task) = NEW cl_aunit_task( driver_program = lv_program ).

        " Add all test classes to the task
        LOOP AT it_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
          lr_task->add_class( EXPORTING name = <ls_class>-object_name ).
        ENDLOOP.

        " Run the task
        lr_task->synchronously_run( ).

        " Get results
        lt_results = lr_task->get_result( ).

        " Convert results
        LOOP AT lt_results ASSIGNING FIELD-SYMBOL(<ls_aunit>).
          CLEAR ls_result.

          ls_result-object_type = 'CLAS'.
          ls_result-object_name = <ls_aunit>-classname.
          ls_result-test_method = <ls_aunit>-method.
          ls_result-duration = <ls_aunit>-duration.

          " Determine status and message
          CASE <ls_aunit>-kind.
            WHEN 'S' OR 'W'.  " Success or Warning
              ls_result-status = 'PASSED'.
            WHEN 'E' OR 'A'.  " Error or Abort
              ls_result-status = 'FAILED'.
            WHEN OTHERS.
              ls_result-status = 'UNKNOWN'.
          ENDCASE.

          ls_result-message = <ls_aunit>-message.

          APPEND ls_result TO rt_results.
        ENDLOOP.

      CATCH cx_aunit_internal cx_aunit_create_task INTO DATA(lx_error).
        " Return error result
        CLEAR ls_result.
        ls_result-status = 'ERROR'.
        ls_result-message = lx_error->get_text( ).
        APPEND ls_result TO rt_results.
    ENDTRY.

  ENDMETHOD.

  METHOD count_results.
    rs_stats-test_count = lines( it_results ).
    rs_stats-passed_count = 0.
    rs_stats-failed_count = 0.

    LOOP AT it_results ASSIGNING FIELD-SYMBOL(<ls_result>).
      CASE <ls_result>-status.
        WHEN 'PASSED'.
          rs_stats-passed_count = rs_stats-passed_count + 1.
        WHEN 'FAILED' OR 'ERROR' OR 'UNKNOWN'.
          rs_stats-failed_count = rs_stats-failed_count + 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
