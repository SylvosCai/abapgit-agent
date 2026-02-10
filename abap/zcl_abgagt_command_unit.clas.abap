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

    " Error structure for failed test methods
    TYPES: BEGIN OF ty_error,
             class_name TYPE string,
             method_name TYPE string,
             error_kind TYPE string,
             error_text TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_unit_result,
             success TYPE abap_bool,
             message TYPE string,
             test_count TYPE i,
             passed_count TYPE i,
             failed_count TYPE i,
             errors TYPE ty_errors,
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

  PRIVATE SECTION.
    " Methods for extracting errors from nested structure
    METHODS get_failed_methods
      IMPORTING
        it_tab_objects TYPE any
      RETURNING
        VALUE(rt_errors) TYPE ty_errors.

    METHODS extract_errors_from_object
      IMPORTING
        is_object TYPE any
      RETURNING
        VALUE(rt_errors) TYPE ty_errors.

    METHODS extract_errors_from_testclass
      IMPORTING
        is_testclass TYPE any
        iv_class_name TYPE string
      RETURNING
        VALUE(rt_errors) TYPE ty_errors.

    METHODS extract_error_from_method
      IMPORTING
        is_method TYPE any
        iv_method_name TYPE string
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

    " Extract failed method details from tab_objects
    rs_result-errors = get_failed_methods( lo_runner->tab_objects ).
  ENDMETHOD.

  METHOD get_failed_methods.
    " Entry point for extracting failed methods from TAB_OBJECTS
    DATA lt_objects TYPE any.
    lt_objects = it_tab_objects.

    LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>).
      " Extract class name from object
      DATA lv_class_name TYPE string.
      ASSIGN COMPONENT 'OBJNAME' OF STRUCTURE <ls_object> TO FIELD-SYMBOL(<lv_objname>).
      IF sy-subrc = 0 AND <lv_objname> IS ASSIGNED.
        lv_class_name = <lv_objname>.
      ENDIF.

      " Extract errors from this object
      DATA(lt_errors) = extract_errors_from_object( <ls_object> ).
      APPEND LINES OF lt_errors TO rt_errors.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_errors_from_object.
    " Extract errors from TAB_TESTCLASSES within an object
    DATA lt_testclasses TYPE any.

    " Get TAB_TESTCLASSES component
    ASSIGN COMPONENT 'TAB_TESTCLASSES' OF STRUCTURE is_object TO FIELD-SYMBOL(<lt_tcl>).
    IF sy-subrc <> 0 OR <lt_tcl> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    lt_testclasses = <lt_tcl>.

    LOOP AT lt_testclasses ASSIGNING FIELD-SYMBOL(<ls_tcl>).
      " Get class name
      DATA lv_class_name TYPE string.
      ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE is_object TO FIELD-SYMBOL(<lv_clsname>).
      IF sy-subrc = 0 AND <lv_clsname> IS ASSIGNED.
        lv_class_name = <lv_clsname>.
      ENDIF.

      " Extract errors from this testclass
      DATA(lt_errors) = extract_errors_from_testclass(
        is_testclass = <ls_tcl>
        iv_class_name = lv_class_name ).
      APPEND LINES OF lt_errors TO rt_errors.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_errors_from_testclass.
    " Extract errors from TAB_METHODS within a testclass
    DATA lt_methods TYPE any.

    " Get TAB_METHODS component
    ASSIGN COMPONENT 'TAB_METHODS' OF STRUCTURE is_testclass TO FIELD-SYMBOL(<lt_methods>).
    IF sy-subrc <> 0 OR <lt_methods> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    lt_methods = <lt_methods>.

    LOOP AT lt_methods ASSIGNING FIELD-SYMBOL(<ls_method>).
      " Get method name
      DATA lv_method_name TYPE string.
      ASSIGN COMPONENT 'METHODNAME' OF STRUCTURE <ls_method> TO FIELD-SYMBOL(<lv_mname>).
      IF sy-subrc = 0 AND <lv_mname> IS ASSIGNED.
        lv_method_name = <lv_mname>.
      ENDIF.

      " Extract error from this method
      DATA(ls_error) = extract_error_from_method(
        is_method = <ls_method>
        iv_method_name = lv_method_name ).

      " Only add if there's an error
      IF ls_error-error_text IS NOT INITIAL.
        ls_error-class_name = iv_class_name.
        APPEND ls_error TO rt_errors.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_error_from_method.
    " Extract STR_ERROR from method and build error structure
    " Structure: STR_ERROR → STR_ERROR_CORE → errorkind, errortext, tab_messages

    " Get STR_ERROR component
    ASSIGN COMPONENT 'STR_ERROR' OF STRUCTURE is_method TO FIELD-SYMBOL(<ls_error>).
    IF sy-subrc <> 0 OR <ls_error> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    " Get STR_ERROR_CORE from STR_ERROR
    ASSIGN COMPONENT 'STR_ERROR_CORE' OF STRUCTURE <ls_error> TO FIELD-SYMBOL(<ls_error_core>).
    IF sy-subrc <> 0 OR <ls_error_core> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    " Get error kind (e.g., 'ERROR', 'FAILURE')
    ASSIGN COMPONENT 'ERRORKIND' OF STRUCTURE <ls_error_core> TO FIELD-SYMBOL(<lv_errorkind>).
    IF sy-subrc = 0 AND <lv_errorkind> IS ASSIGNED.
      rs_error-error_kind = <lv_errorkind>.
    ENDIF.

    " Only process if there's an actual error
    IF rs_error-error_kind IS INITIAL.
      RETURN.
    ENDIF.

    " Get error text
    ASSIGN COMPONENT 'ERRORTEXT' OF STRUCTURE <ls_error_core> TO FIELD-SYMBOL(<lv_errortext>).
    IF sy-subrc = 0 AND <lv_errortext> IS ASSIGNED.
      rs_error-error_text = <lv_errortext>.
    ENDIF.

    " If no error text, try tab_messages
    IF rs_error-error_text IS INITIAL.
      ASSIGN COMPONENT 'TAB_MESSAGES' OF STRUCTURE <ls_error_core> TO FIELD-SYMBOL(<lt_messages>).
      IF sy-subrc = 0 AND <lt_messages> IS ASSIGNED.
        DATA lv_messages TYPE string.
        LOOP AT <lt_messages> ASSIGNING FIELD-SYMBOL(<lv_msg>).
          IF lv_messages IS NOT INITIAL.
            lv_messages = |{ lv_messages }\n|.
          ENDIF.
          lv_messages = |{ lv_messages }{ <lv_msg> }|.
        ENDLOOP.
        rs_error-error_text = lv_messages.
      ENDIF.
    ENDIF.

    rs_error-method_name = iv_method_name.
  ENDMETHOD.

ENDCLASS.
