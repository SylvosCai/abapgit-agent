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

    " Run AUnit tests
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
    " Run unit tests using RAISE EVENT mechanism
    " Create a local class that implements the event handler

    DATA: lv_test_classes TYPE string.

    " Build space-separated list of test classes
    LOOP AT it_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
      IF lv_test_classes IS INITIAL.
        lv_test_classes = <ls_class>-object_name.
      ELSE.
        lv_test_classes = |{ lv_test_classes } { <ls_class>-object_name }|.
      ENDIF.
    ENDLOOP.

    " Use CL_AUNIT_ENGINE for execution
    DATA: lo_engine TYPE REF TO if_aunit_engine.

    lo_engine = cl_aunit_engine=>create( ).

    " Add test classes
    LOOP AT it_classes ASSIGNING <ls_class>.
      lo_engine->add_test_class(
        EXPORTING
          p_name = <ls_class>-object_name ).
    ENDLOOP.

    " Run tests
    lo_engine->run(
      EXPORTING
        p_howtorun = 'S'  " Short
      EXCEPTIONS
        OTHERS = 4 ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Get results
    DATA(lo_result) = lo_engine->get_result( ).

    " Get plain list
    DATA: lt_alv TYPE scit_alvlist.
    lo_result->get_plain_list(
      EXPORTING
        p_max_lines = 1000000
      IMPORTING
        p_list = lt_alv ).

    " Convert to our format
    LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<ls_alv>).
      DATA: lv_text TYPE string.
      lv_text = <ls_alv>-text.

      " Extract method name from text (format: CLASS=>METHOD)
      DATA: lv_class TYPE string,
            lv_method TYPE string.

      IF lv_text CS '=>'.
        DATA(lv_pos) = sy-fdpos.
        lv_class = lv_text(lv_pos).
        lv_method = lv_text+lv_pos+2.
      ELSE.
        lv_class = lv_text.
        lv_method = ''.
      ENDIF.

      DATA(ls_result) = VALUE ty_test_result(
        object_type = <ls_alv>-objtyp
        object_name = lv_class
        test_method = lv_method
        status = <ls_alv>-kind
        message = <ls_alv>-text
        line = <ls_alv>-line
      ).
      APPEND ls_result TO rt_results.
    ENDLOOP.

  ENDMETHOD.

  METHOD count_results.
    rs_stats-test_count = lines( it_results ).
    rs_stats-passed_count = 0.
    rs_stats-failed_count = 0.

    LOOP AT it_results ASSIGNING FIELD-SYMBOL(<ls_result>).
      CASE <ls_result>-status.
        WHEN 'P' OR 'S'.  " Passed or Success
          rs_stats-passed_count = rs_stats-passed_count + 1.
        WHEN 'A' OR 'E' OR 'F'.  " Abort, Error, or Failed
          rs_stats-failed_count = rs_stats-failed_count + 1.
        WHEN OTHERS.
          IF <ls_result>-message CS 'Passed' OR <ls_result>-message CS 'passed'.
            rs_stats-passed_count = rs_stats-passed_count + 1.
          ELSE.
            rs_stats-failed_count = rs_stats-failed_count + 1.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
