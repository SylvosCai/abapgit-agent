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
           END OF ty_unit_params.

    TYPES: BEGIN OF ty_unit_result,
             success TYPE abap_bool,
             message TYPE string,
             test_count TYPE i,
             passed_count TYPE i,
             failed_count TYPE i,
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
        VALUE(rs_result) TYPE ty_unit_result.

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
      it_keys = lt_keys ).

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
  ENDMETHOD.

ENDCLASS.
