*"*"use source
*"*"Local Interface:
*"**********************************************************************
" UNIT command implementation - runs AUnit tests directly
CLASS zcl_abgagt_command_unit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_unit_params,
             package TYPE string,
             files TYPE string_table,
             coverage TYPE abap_bool,
           END OF ty_unit_params.

    " Error structure for failed test methods
    TYPES: BEGIN OF ty_error,
             class_name TYPE string,
             method_name TYPE string,
             error_kind TYPE string,
             error_text TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH DEFAULT KEY.

    " Coverage statistics
    TYPES: BEGIN OF ty_coverage_stats,
             total_lines TYPE i,
             covered_lines TYPE i,
             coverage_rate TYPE p LENGTH 10 DECIMALS 1,
           END OF ty_coverage_stats.

    " Coverage line details
    TYPES: BEGIN OF ty_coverage_line,
             program TYPE string,
             include TYPE string,
             line TYPE i,
             hits TYPE i,
           END OF ty_coverage_line.

    TYPES ty_coverage_lines TYPE STANDARD TABLE OF ty_coverage_line WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_unit_result,
             success TYPE abap_bool,
             message TYPE string,
             test_count TYPE i,
             passed_count TYPE i,
             failed_count TYPE i,
             errors TYPE ty_errors,
             coverage_stats TYPE ty_coverage_stats,
             coverage_lines TYPE ty_coverage_lines,
           END OF ty_unit_result.

    TYPES: BEGIN OF ty_key,
             obj_name TYPE tadir-obj_name,
             obj_type TYPE tadir-object,
           END OF ty_key.

    TYPES ty_keys TYPE STANDARD TABLE OF ty_key WITH DEFAULT KEY.

    METHODS run_aunit_tests
      IMPORTING
        iv_package TYPE devclass OPTIONAL
        it_keys TYPE ty_keys OPTIONAL
        iv_coverage TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rs_result) TYPE ty_unit_result.

  PRIVATE SECTION.
    " Methods for extracting errors from nested structure using CL_SUT_AUNIT_RUNNER types
    METHODS get_failed_methods
      IMPORTING
        it_tab_objects TYPE cl_sut_aunit_runner=>typ_tab_objects
      RETURNING
        VALUE(rt_errors) TYPE ty_errors.

    " Get coverage results from AUnit runner
    METHODS get_coverage
      IMPORTING
        io_runner TYPE REF TO cl_sut_aunit_runner
      RETURNING
        VALUE(rs_coverage_stats) TYPE ty_coverage_stats.

    METHODS extract_errors_from_object
      IMPORTING
        is_object TYPE cl_sut_aunit_runner=>typ_str_object
      RETURNING
        VALUE(rt_errors) TYPE ty_errors.

    METHODS extract_errors_from_testclass
      IMPORTING
        is_testclass TYPE cl_sut_aunit_runner=>typ_str_testclass
        iv_class_name TYPE seoclass-clsname
      RETURNING
        VALUE(rt_errors) TYPE ty_errors.

    METHODS extract_error_from_method
      IMPORTING
        is_method TYPE cl_sut_aunit_runner=>typ_str_method
        iv_method_name TYPE saunit_s_method_key-method
      RETURNING
        VALUE(rs_error) TYPE ty_error.

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
          lt_keys TYPE ty_keys,
          ls_key TYPE ty_key.

    " Parse parameters from is_param
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Convert package string to devclass
    IF ls_params-package IS NOT INITIAL.
      lv_package = ls_params-package.
    ENDIF.

    " Parse files to get object keys
    IF ls_params-files IS NOT INITIAL.
      lo_util = zcl_abgagt_util=>get_instance( ).
      LOOP AT ls_params-files INTO lv_file.
        CLEAR: lv_obj_type, lv_obj_name.
        lo_util->zif_abgagt_util~parse_file_to_object(
          EXPORTING iv_file = lv_file
          IMPORTING ev_obj_type = lv_obj_type
                    ev_obj_name = lv_obj_name ).
        IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
          CLEAR ls_key.
          ls_key-obj_type = lv_obj_type.
          ls_key-obj_name = lv_obj_name.
          APPEND ls_key TO lt_keys.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Initialize result
    ls_result-success = abap_false.
    ls_result-test_count = 0.
    ls_result-passed_count = 0.
    ls_result-failed_count = 0.

    IF lt_keys IS INITIAL AND lv_package IS INITIAL.
      ls_result-message = 'No files or package provided'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Remove duplicates from keys
    IF lt_keys IS NOT INITIAL.
      SORT lt_keys BY obj_type obj_name.
      DELETE ADJACENT DUPLICATES FROM lt_keys COMPARING obj_type obj_name.
    ENDIF.

    " Run tests and get result directly from str_results
    ls_result = run_aunit_tests(
      iv_package = lv_package
      it_keys = lt_keys
      iv_coverage = ls_params-coverage ).

    IF ls_result-test_count = 0 AND ls_result-failed_count = 0.
      ls_result-message = 'No test results returned'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    IF ls_result-failed_count = 0.
      ls_result-success = abap_true.
      ls_result-message = |All { ls_result-test_count } tests passed|.
    ELSE.
      ls_result-message = |{ ls_result-failed_count } of { ls_result-test_count } tests failed|.
    ENDIF.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD run_aunit_tests.
    DATA: lo_runner TYPE REF TO cl_sut_aunit_runner.

    " Create runner using S_CREATE
    cl_sut_aunit_runner=>s_create(
      EXPORTING
        p_cov       = iv_coverage
        i_flg_api   = abap_true
      RECEIVING
        r_ref_runner = lo_runner ).

    " Configure runner for OBJECT selection mode
    lo_runner->p_disp = abap_false.   " No GUI display
    lo_runner->p_save = abap_true.     " Save results
    lo_runner->p_runmd = 'E'.         " Execute mode

    " Configure coverage scope if enabled
    IF iv_coverage = abap_true.
      lo_runner->p_cvrau = lo_runner->c_str_cov_scope-specified_range.

      " Add test classes to coverage program range
      IF it_keys IS NOT INITIAL.
        LOOP AT it_keys ASSIGNING FIELD-SYMBOL(<ls_key>).
          DATA(lv_mainprog) = lo_runner->get_mainprog(
            i_objtype = <ls_key>-obj_type
            i_objname = <ls_key>-obj_name ).
          IF lv_mainprog IS NOT INITIAL.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_mainprog ) TO lo_runner->so_cvprg.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " Set selection type to OBJECT
    lo_runner->p_seltyp = 'OBJECT'.
    lo_runner->p_selcl = abap_true.   " Enable class selection mode

    " Build SO_CLASS range for test class(es)
    DATA lt_so_class TYPE RANGE OF seoaliases-clsname.
    IF it_keys IS NOT INITIAL.
      LOOP AT it_keys ASSIGNING <ls_key>.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <ls_key>-obj_name ) TO lt_so_class.
      ENDLOOP.
    ENDIF.

    " Set the class range
    lo_runner->so_class[] = lt_so_class[].

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

    " Get results from str_results - use type from CL_SUT_AUNIT_RUNNER
    DATA: ls_str TYPE cl_sut_aunit_runner=>typ_str_results.
    ls_str = lo_runner->str_results.

    " Fill result directly from str_results counts
    rs_result-test_count = ls_str-cnt_testmethods.
    rs_result-passed_count = ls_str-cnt_ok_methods.
    rs_result-failed_count = ls_str-cnt_error_methods.

    IF rs_result-failed_count > 0 OR ls_str-pie_abortions > 0.
      rs_result-success = abap_false.
    ELSE.
      rs_result-success = abap_true.
    ENDIF.

    " Extract failed method details from tab_objects
    rs_result-errors = get_failed_methods( lo_runner->tab_objects ).

    " Get coverage results if requested
    IF iv_coverage = abap_true.
      rs_result-coverage_stats = get_coverage( io_runner = lo_runner ).
    ENDIF.
  ENDMETHOD.

  METHOD get_coverage.
    " Get coverage statistics from AUnit runner using stats method
    DATA: lv_cov_id TYPE sut_au_results-cov_id.
    lv_cov_id = io_runner->str_results-cov_id.

    IF lv_cov_id IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        " Try to get stats directly
        DATA(ls_cov_stats) = io_runner->get_coverage_result_stats(
          i_cov_id = lv_cov_id ).
        rs_coverage_stats-total_lines = ls_cov_stats-cov_lines_total.
        rs_coverage_stats-covered_lines = ls_cov_stats-cov_lines_covered.
        rs_coverage_stats-coverage_rate = ls_cov_stats-cov_lines_rate.
      CATCH cx_sut_error.
        " Coverage stats not available - try flat results
        TRY.
            DATA(lt_cov_flat) = io_runner->get_coverage_result_flat(
              i_cov_id = lv_cov_id ).
            " Check if we got any data
            IF lines( lt_cov_flat ) > 0.
              rs_coverage_stats-total_lines = lines( lt_cov_flat ).
            ENDIF.
          CATCH cx_sut_error.
            " No coverage data available
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD get_failed_methods.
    " Entry point for extracting failed methods from TAB_OBJECTS
    LOOP AT it_tab_objects ASSIGNING FIELD-SYMBOL(<ls_object>).
      " Extract errors from this object
      DATA(lt_errors) = extract_errors_from_object( <ls_object> ).
      APPEND LINES OF lt_errors TO rt_errors.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_errors_from_object.
    " Extract errors from TAB_TESTCLASSES within an object
    LOOP AT is_object-tab_testclasses ASSIGNING FIELD-SYMBOL(<ls_tcl>).
      " Extract errors from this testclass
      DATA(lt_errors) = extract_errors_from_testclass(
        is_testclass = <ls_tcl>
        iv_class_name = is_object-clsname ).
      APPEND LINES OF lt_errors TO rt_errors.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_errors_from_testclass.
    " Extract errors from TAB_METHODS within a testclass
    LOOP AT is_testclass-tab_methods ASSIGNING FIELD-SYMBOL(<ls_method>).
      " Extract error from this method
      DATA(ls_error) = extract_error_from_method(
        is_method = <ls_method>
        iv_method_name = <ls_method>-methodname ).

      " Only add if there's an error
      IF ls_error-error_text IS NOT INITIAL.
        ls_error-class_name = iv_class_name.
        APPEND ls_error TO rt_errors.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_error_from_method.
    " Extract STR_ERROR from method and build error structure
    " typ_str_error contains typ_str_error_core via INCLUDE as STR_ERROR_CORE

    " Check if there's an error - only process if error kind is set
    IF is_method-str_error IS INITIAL.
      RETURN.
    ENDIF.

    " Get error kind (e.g., 'ERROR', 'FAILURE')
    rs_error-error_kind = is_method-str_error-str_error_core-errorkind.

    " Only process if there's an actual error
    IF rs_error-error_kind IS INITIAL.
      RETURN.
    ENDIF.

    " Get error text
    rs_error-error_text = is_method-str_error-str_error_core-errortext.

    " If no error text, try tab_messages
    IF rs_error-error_text IS INITIAL.
      DATA lv_messages TYPE string.
      LOOP AT is_method-str_error-str_error_core-tab_messages ASSIGNING FIELD-SYMBOL(<lv_msg>).
        IF lv_messages IS NOT INITIAL.
          lv_messages = |{ lv_messages }\n|.
        ENDIF.
        lv_messages = |{ lv_messages }{ <lv_msg> }|.
      ENDLOOP.
      rs_error-error_text = lv_messages.
    ENDIF.

    rs_error-method_name = iv_method_name.
  ENDMETHOD.

ENDCLASS.
