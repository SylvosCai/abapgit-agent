*"*"use source
*"*"Local Interface:
*"**********************************************************************
" UNIT command implementation - runs AUnit tests directly
CLASS zcl_abgagt_command_unit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    " Type for str_results from CL_SUT_AUNIT_RUNNER
    TYPES: BEGIN OF ty_str_results,
             cnt_testmethods       TYPE i,
             cnt_ok_methods        TYPE i,
             cnt_error_methods     TYPE i,
             pie_failures          TYPE i,
             pie_abortions         TYPE i,
             flg_failure           TYPE c LENGTH 1,
             flg_abortion          TYPE c LENGTH 1,
           END OF ty_str_results.

    TYPES: BEGIN OF ty_test_result,
             object_type TYPE string,
             object_name TYPE string,
             test_method TYPE string,
             status TYPE string,
             message TYPE string,
             line TYPE string,
           END OF ty_test_result.

    TYPES ty_test_results TYPE STANDARD TABLE OF ty_test_result WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_unit_params,
             package TYPE string,
             files TYPE string_table,
           END OF ty_unit_params.

    TYPES: BEGIN OF ty_unit_result,
             success TYPE abap_bool,
             message TYPE string,
             test_count TYPE i,
             passed_count TYPE i,
             failed_count TYPE i,
             results TYPE ty_test_results,
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
      RETURNING
        VALUE(rt_results) TYPE ty_test_results.

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

    " Run tests
    ls_result-results = run_aunit_tests(
      iv_package = lv_package
      it_keys = lt_keys ).

    IF ls_result-results IS INITIAL.
      ls_result-message = 'No test results returned'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Count results
    ls_result-test_count = lines( ls_result-results ).

    LOOP AT ls_result-results ASSIGNING FIELD-SYMBOL(<ls_test>).
      DATA(lv_status) = <ls_test>-status.
      IF lv_status IS INITIAL.
        " If status is empty, count as passed (no error info)
        ls_result-passed_count = ls_result-passed_count + 1.
      ELSE.
        CASE lv_status.
          WHEN 'P' OR 'S' OR 'PASSED' OR 'passed'.
            ls_result-passed_count = ls_result-passed_count + 1.
          WHEN 'A' OR 'E' OR 'F' OR 'FAILED' OR 'failed' OR 'ERROR' OR 'error'.
            ls_result-failed_count = ls_result-failed_count + 1.
          WHEN OTHERS.
            " Unknown status - count as passed if no error in message
            IF <ls_test>-message IS INITIAL OR
               <ls_test>-message CN 'errorfail'.
              ls_result-passed_count = ls_result-passed_count + 1.
            ELSE.
              ls_result-failed_count = ls_result-failed_count + 1.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDLOOP.

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
        p_cov       = abap_false
        i_flg_api   = abap_true
      RECEIVING
        r_ref_runner = lo_runner ).

    " Configure runner for OBJECT selection mode
    lo_runner->p_disp = abap_false.   " No GUI display
    lo_runner->p_save = abap_true.     " Save results
    lo_runner->p_runmd = 'E'.         " Execute mode

    " Set selection type to OBJECT
    lo_runner->p_seltyp = 'OBJECT'.
    lo_runner->p_selcl = abap_true.   " Enable class selection mode

    " Build SO_CLASS range for test class(es)
    DATA lt_so_class TYPE RANGE OF seoaliases-clsname.
    IF it_keys IS NOT INITIAL.
      LOOP AT it_keys ASSIGNING FIELD-SYMBOL(<ls_key>).
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

    " Get results from str_results
    DATA: ls_str_results TYPE ty_str_results.

    " Cast str_results to local type using field symbols
    FIELD-SYMBOLS <fs_results> TYPE any.
    ASSIGN lo_runner->str_results TO <fs_results>.
    IF <fs_results> IS ASSIGNED.
      ls_str_results-cnt_testmethods = <fs_results>->cnt_testmethods.
      ls_str_results-cnt_ok_methods = <fs_results>->cnt_ok_methods.
      ls_str_results-cnt_error_methods = <fs_results>->cnt_error_methods.
      ls_str_results-pie_failures = <fs_results>->pie_failures.
      ls_str_results-pie_abortions = <fs_results>->pie_abortions.
      ls_str_results-flg_failure = <fs_results>->flg_failure.
      ls_str_results-flg_abortion = <fs_results>->flg_abortion.
    ENDIF.

    " Build result entry from str_results counts
    DATA(ls_result) = VALUE ty_test_result(
      object_type = 'CLAS'
      object_name = 'AUNIT'
      test_method = 'SUMMARY'
      status = COND #( WHEN ls_str_results-cnt_error_methods > 0 OR ls_str_results-pie_abortions > 0 THEN 'F' ELSE 'P' )
      message = |Tests: { ls_str_results-cnt_testmethods }, Passed: { ls_str_results-cnt_ok_methods }, Failed: { ls_str_results-cnt_error_methods }|
      line = ls_str_results-cnt_testmethods
    ).
    APPEND ls_result TO rt_results.
  ENDMETHOD.

ENDCLASS.
