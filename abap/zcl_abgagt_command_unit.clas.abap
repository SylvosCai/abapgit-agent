*"*"use source
*"*"Local Interface:
*"**********************************************************************
" UNIT command implementation - runs AUnit tests directly (no abapGit API)
CLASS zcl_abgagt_command_unit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_unit_params,
             package TYPE string,
             files TYPE string_table,
           END OF ty_unit_params.

    TYPES: BEGIN OF ty_test_result,
             object_type TYPE string,
             object_name TYPE string,
             test_method TYPE string,
             status TYPE string,
             message TYPE string,
             line TYPE string,
           END OF ty_test_result.

    TYPES ty_test_results TYPE STANDARD TABLE OF ty_test_result WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_unit_result,
             success TYPE abap_bool,
             message TYPE string,
             test_count TYPE i,
             passed_count TYPE i,
             failed_count TYPE i,
             results TYPE ty_test_results,
           END OF ty_unit_result.

    TYPES ty_object_keys TYPE TABLE OF scir_objs WITH NON-UNIQUE DEFAULT KEY.

    METHODS get_test_classes
      IMPORTING
        iv_package TYPE devclass OPTIONAL
        it_objects TYPE ty_object_keys OPTIONAL
      RETURNING
        VALUE(rt_classes) TYPE ty_object_keys.

    METHODS run_aunit_tests
      IMPORTING
        it_classes TYPE ty_object_keys
      RETURNING
        VALUE(rt_results) TYPE ty_test_results.

    METHODS count_results
      IMPORTING
        it_results TYPE ty_test_results
      CHANGING
        rs_stats TYPE ty_unit_result.

ENDCLASS.

CLASS zcl_abgagt_command_unit IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_unit.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_unit_params,
          lv_package TYPE devclass,
          lv_file TYPE string,
          lv_obj_type TYPE string,
          lv_obj_name TYPE string,
          lo_util TYPE REF TO zcl_abgagt_util,
          ls_result TYPE ty_unit_result,
          lt_objects TYPE ty_object_keys.

    " Parse parameters from is_param
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Convert package string to devclass
    IF ls_params-package IS NOT INITIAL.
      lv_package = ls_params-package.
    ENDIF.

    " Parse files to objects
    IF ls_params-files IS NOT INITIAL.
      lo_util = zcl_abgagt_util=>get_instance( ).
      LOOP AT ls_params-files INTO lv_file.
        CLEAR: lv_obj_type, lv_obj_name.
        lo_util->zif_abgagt_util~parse_file_to_object(
          EXPORTING iv_file = lv_file
          IMPORTING ev_obj_type = lv_obj_type
                    ev_obj_name = lv_obj_name ).
        IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
          <ls_obj>-object_type = lv_obj_type.
          <ls_obj>-object_name = lv_obj_name.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Initialize result
    ls_result-success = abap_false.
    ls_result-test_count = 0.
    ls_result-passed_count = 0.
    ls_result-failed_count = 0.

    " Get test classes to run
    DATA(lt_test_classes) = get_test_classes(
      iv_package = lv_package
      it_objects = lt_objects ).

    IF lt_test_classes IS INITIAL.
      ls_result-message = 'No test classes found'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    ls_result-message = |Found { lines( lt_test_classes ) } test class(es)|.

    " Run AUnit tests
    ls_result-results = run_aunit_tests( lt_test_classes ).

    IF ls_result-results IS INITIAL.
      ls_result-message = |No test results - { ls_result-message }|.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Count results
    count_results(
      EXPORTING it_results = ls_result-results
      CHANGING rs_stats = ls_result ).

    IF ls_result-failed_count = 0.
      ls_result-success = abap_true.
      ls_result-message = |All { ls_result-test_count } tests passed|.
    ELSE.
      ls_result-message = |{ ls_result-failed_count } of { ls_result-test_count } tests failed|.
    ENDIF.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
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
    DATA: lo_runner TYPE REF TO cl_sut_aunit_runner.

    " Create runner using s_create
    cl_sut_aunit_runner=>s_create(
      EXPORTING
        p_cov       = abap_false
        i_flg_api   = abap_true
      RECEIVING
        r_ref_runner = lo_runner ).

    " Configure runner
    lo_runner->p_disp = abap_false.    " Don't show results UI
    lo_runner->p_save = abap_true.     " Save values
    lo_runner->p_runmd = 'E'.          " Execute only (not plan)

    " Set test classes
    DATA lv_test_classes TYPE string.
    LOOP AT it_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
      IF lv_test_classes IS INITIAL.
        lv_test_classes = <ls_class>-object_name.
      ELSE.
        lv_test_classes = |{ lv_test_classes } { <ls_class>-object_name }|.
      ENDIF.
    ENDLOOP.

    " Run tests
    TRY.
        lo_runner->run(
          EXPORTING
            i_flg_select_only = abap_false
          EXCEPTIONS
            OTHERS = 1 ).
      CATCH cx_sut_error.
        RETURN.
    ENDTRY.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Get results from tab_objects
    DATA(lt_objects) = lo_runner->tab_objects.

    IF lt_objects IS INITIAL.
      RETURN.
    ENDIF.

    " Process results - structure: OBJECT-TAB_TESTCLASSES-TAB_METHODS
    LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>).
      DATA(lv_obj_name) = <ls_object>-obj_name.

      " Loop through test classes
      LOOP AT <ls_object>-tab_testclasses ASSIGNING FIELD-SYMBOL(<ls_tcl>).
        DATA(lv_tcl_name) = <ls_tcl>-testclass.

        " Loop through test methods
        LOOP AT <ls_tcl>-tab_methods ASSIGNING FIELD-SYMBOL(<ls_method>).
          " Extract fields dynamically since structure names vary
          DATA: lv_methodname TYPE string,
                lv_kind TYPE string,
                lv_desc TYPE string,
                lv_src TYPE string.

          ASSIGN COMPONENT 'METHODNAME' OF STRUCTURE <ls_method> TO FIELD-SYMBOL(<lv_mname>).
          IF sy-subrc = 0 AND <lv_mname> IS ASSIGNED.
            lv_methodname = <lv_mname>.
          ENDIF.

          ASSIGN COMPONENT 'KIND' OF STRUCTURE <ls_method> TO FIELD-SYMBOL(<lv_kind>).
          IF sy-subrc = 0 AND <lv_kind> IS ASSIGNED.
            lv_kind = <lv_kind>.
          ENDIF.

          ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <ls_method> TO FIELD-SYMBOL(<lv_desc>).
          IF sy-subrc = 0 AND <lv_desc> IS ASSIGNED.
            lv_desc = <lv_desc>.
          ENDIF.

          ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <ls_method> TO FIELD-SYMBOL(<lv_src>).
          IF sy-subrc = 0 AND <lv_src> IS ASSIGNED.
            lv_src = <lv_src>.
          ENDIF.

          DATA(ls_result) = VALUE ty_test_result(
            object_type = 'CLAS'
            object_name = lv_obj_name
            test_method = lv_methodname
            status = lv_kind
            message = lv_desc
            line = lv_src
          ).
          APPEND ls_result TO rt_results.
        ENDLOOP.
      ENDLOOP.
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
