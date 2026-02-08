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
             debug TYPE string,
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

    METHODS run_local_tests
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

    " Run local tests using ABAP Unit framework
    DATA(lt_results) = run_local_tests( lt_test_classes ).
    rs_result-results = lt_results.

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

  METHOD run_local_tests.
    " Run unit tests by discovering methods dynamically
    DATA: lv_class_name TYPE seoclsname.

    DATA: lo_test_class TYPE REF TO object.

    DATA lt_methods TYPE TABLE OF seocpdname.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF rt_results.
    FIELD-SYMBOLS: <ls_meth> LIKE LINE OF lt_methods.

    TRY.
        LOOP AT it_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
          lv_class_name = <ls_class>-object_name.

          " Create test class instance dynamically
          CREATE OBJECT lo_test_class TYPE (lv_class_name).

          " Get all methods using RTTI
          DATA(lo_type) = cl_abap_typedescr=>describe_by_object_ref( lo_test_class ).
          DATA(lo_class) = CAST cl_abap_classdescr( lo_type ).
          DATA(lt_all_methods) = lo_class->methods.

          " Filter for test methods (start with TEST or end with _TEST)
          LOOP AT lt_all_methods ASSIGNING FIELD-SYMBOL(<ls_method>).
            DATA(lv_mname) = <ls_method>-name.
            IF lv_mname CS 'TEST' OR lv_mname CS '_TEST'.
              APPEND lv_mname TO lt_methods.
            ENDIF.
          ENDLOOP.

          " Run each test method
          SORT lt_methods.
          DELETE ADJACENT DUPLICATES FROM lt_methods.

          LOOP AT lt_methods ASSIGNING <ls_meth>.
            " Call setup method if exists
            CALL METHOD lo_test_class->setup
              EXCEPTIONS OTHERS = 0.

            " Execute test method and catch exceptions
            CALL METHOD lo_test_class->(<ls_meth>)
              EXCEPTIONS
                method_not_found = 1
                OTHERS = 2.

            IF sy-subrc = 0.
              " Test passed (no exception)
              APPEND VALUE #(
                object_type = 'CLAS'
                object_name = lv_class_name
                test_method = <ls_meth>
                status = 'PASSED'
                message = |{ lv_class_name }=>{ <ls_meth> } passed|
                line = ''
              ) TO rt_results.
            ELSE.
              " Test failed or error
              APPEND VALUE #(
                object_type = 'CLAS'
                object_name = lv_class_name
                test_method = <ls_meth>
                status = 'FAILED'
                message = |{ lv_class_name }=>{ <ls_meth> } failed|
                line = ''
              ) TO rt_results.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

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
        WHEN 'S' OR 'P' OR 'PASSED'.  " Success or Passed
          rs_stats-passed_count = rs_stats-passed_count + 1.
        WHEN 'A' OR 'E' OR 'F' OR 'FAILED' OR 'ERROR'.  " Abort, Error, or Failed
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
