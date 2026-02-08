*"*"use source
*"*"Local Interface
CLASS zcl_abapgit_agent_unit_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_test_result,
             object_type TYPE string,
             object_name TYPE string,
             test_method TYPE string,
             status TYPE string,
             message TYPE string,
             line TYPE string,
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
      CHANGING
        rs_stats TYPE ty_result.

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

    rs_result-message = |Found { lines( lt_test_classes ) } test class(es)|.

    " Run AUnit tests using CL_SUT_AUNIT_RUNNER
    rs_result-results = run_aunit_tests( lt_test_classes ).

    IF rs_result-results IS INITIAL.
      rs_result-message = |No test results - { rs_result-message }|.
      RETURN.
    ENDIF.

    " Count results
    count_results(
      EXPORTING it_results = rs_result-results
      CHANGING rs_stats = rs_result ).

    IF rs_result-failed_count = 0.
      rs_result-success = abap_true.
      rs_result-message = |All { rs_result-test_count } tests passed|.
    ELSE.
      rs_result-message = |{ rs_result-failed_count } of { rs_result-test_count } tests failed|.
    ENDIF.

  ENDMETHOD.

  METHOD get_test_classes.
    DATA: lt_tadir TYPE TABLE OF tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.

    " Get all test classes from package
    IF iv_package IS NOT INITIAL.
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
    " Run unit tests using CL_SUT_AUNIT_RUNNER
    DATA: lo_runner TYPE REF TO cl_sut_aunit_runner,
          lv_exc_text TYPE string,
          lv_stats TYPE string,
          lv_result TYPE string.

    DATA: BEGIN OF ls_aunit_result,
            type TYPE c LENGTH 1,
            sep TYPE c,
            text TYPE string,
          END OF ls_aunit_result.

    DATA lt_aunit_results LIKE TABLE OF ls_aunit_result.

    " Create runner instance
    CREATE OBJECT lo_runner.

    TRY.
        " Build test class string (space-separated)
        DATA(lv_test_classes).
        LOOP AT it_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
          IF lv_test_classes IS INITIAL.
            lv_test_classes = <ls_class>-object_name.
          ELSE.
            lv_test_classes = |{ lv_test_classes } { <ls_class>-object_name }|.
          ENDIF.
        ENDLOOP.

        " Run tests
        lo_runner->run_as_rfc_system(
          EXPORTING
            p_testclasses = lv_test_classes
            p_env         = 'ABAP'
          RECEIVING
            p_result      = lv_result
          EXCEPTIONS
            system_failure = 1
            communication_failure = 2
            OTHERS = 3 ).

        IF sy-subrc <> 0.
          " Get exception text
          lv_exc_text = lo_runner->get_raised_exc_text( ).
          RETURN.
        ENDIF.

        " Get test statistics
        lv_stats = lo_runner->get_test_stats( ).

        " Parse the result - format: P|ClassName.MethodName;...
        DATA lv_off TYPE i.
        DATA lv_len TYPE i.
        DATA lv_line TYPE string.
        DATA lv_rem TYPE string.

        lv_rem = lv_result.

        WHILE lv_rem IS NOT INITIAL.
          " Find separator
          FIND '|' IN lv_rem MATCH OFFSET DATA(lv_pos).
          IF sy-subrc = 0.
            " Get type
            DATA(lv_type) = lv_rem(lv_pos).
            lv_rem = lv_rem+lv_pos+1.

            " Find next separator or end
            FIND ';' IN lv_rem MATCH OFFSET DATA(lv_pos2).
            IF sy-subrc = 0.
              lv_line = lv_rem(lv_pos2).
              lv_rem = lv_rem+lv_pos2+1.
            ELSE.
              lv_line = lv_rem.
              CLEAR lv_rem.
            ENDIF.

            " Parse class and method
            FIND '=' IN lv_line MATCH OFFSET DATA(lv_eq_pos).
            IF sy-subrc = 0.
              DATA(lv_class) = lv_line(lv_eq_pos).
              DATA(lv_method) = lv_line+lv_eq_pos+1.
            ELSE.
              lv_class = lv_line.
              lv_method = ''.
            ENDIF.

            " Add to results
            DATA(ls_result) = VALUE ty_test_result(
              object_type = 'CLAS'
              object_name = lv_class
              test_method = lv_method
              status = lv_type
              message = lv_line
              line = ''
            ).
            APPEND ls_result TO rt_results.
          ELSE.
            CLEAR lv_rem.
          ENDIF.
        ENDWHILE.

      CATCH cx_root INTO DATA(lx_error).
        " Return empty on error
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD count_results.
    rs_stats-test_count = lines( it_results ).
    rs_stats-passed_count = 0.
    rs_stats-failed_count = 0.

    LOOP AT it_results ASSIGNING FIELD-SYMBOL(<ls_result>).
      CASE <ls_result>-status.
        WHEN 'P'.  " Passed
          rs_stats-passed_count = rs_stats-passed_count + 1.
        WHEN 'F' OR 'E' OR 'A'.  " Failed, Error, or Abort
          rs_stats-failed_count = rs_stats-failed_count + 1.
        WHEN OTHERS.
          " Check message for pass/fail
          IF <ls_result>-message CS 'Passed' OR <ls_result>-message CS 'passed'.
            rs_stats-passed_count = rs_stats-passed_count + 1.
          ELSE.
            rs_stats-failed_count = rs_stats-failed_count + 1.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
