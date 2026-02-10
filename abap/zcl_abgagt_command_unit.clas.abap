*"*"use source
*"*"Local Interface:
*"**********************************************************************
" UNIT command implementation - runs unit tests using SAP AUnit
CLASS zcl_abgagt_command_unit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_object,
             object_type TYPE string,
             object_name TYPE string,
           END OF ty_object.

    TYPES ty_object_list TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

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

    TYPES: BEGIN OF ty_unit_params,
             package TYPE string,
             objects TYPE ty_object_list,
             files TYPE string_table,
           END OF ty_unit_params.

ENDCLASS.

CLASS zcl_abgagt_command_unit IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_unit.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_unit_params,
          lv_json TYPE string,
          lv_file TYPE string,
          lv_objtp TYPE string,
          lv_objnm TYPE string.

    IF lines( it_files ) = 1.
      READ TABLE it_files INDEX 1 INTO lv_json.
      IF lv_json CP '*{*' OR lv_json CP '*"*'.
        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_json
          CHANGING data = ls_params ).
      ENDIF.
    ELSEIF lines( it_files ) > 0.
      DATA(lo_util) = zcl_abgagt_util=>get_instance( ).
      LOOP AT it_files INTO lv_file.
        CLEAR: lv_objtp, lv_objnm.
        lo_util->zif_abgagt_util~parse_file_to_object(
          EXPORTING iv_file = lv_file
          IMPORTING ev_obj_type = lv_objtp
                    ev_obj_name = lv_objnm ).
        IF lv_objtp IS NOT INITIAL AND lv_objnm IS NOT INITIAL.
          APPEND INITIAL LINE TO ls_params-objects ASSIGNING FIELD-SYMBOL(<ls_param>).
          <ls_param>-object_type = lv_objtp.
          <ls_param>-object_name = lv_objnm.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DATA(lv_package) TYPE devclass.
    IF ls_params-package IS NOT INITIAL.
      lv_package = ls_params-package.
    ENDIF.
    DATA(ls_result) TYPE ty_unit_result.
    ls_result = run_unit_tests(
      iv_package = lv_package
      it_objects = ls_params-objects ).

    DATA: BEGIN OF ls_response,
            success TYPE string,
            message TYPE string,
            test_count TYPE i,
            passed_count TYPE i,
            failed_count TYPE i,
            results TYPE ty_test_results,
          END OF ls_response.

    ls_response-success = COND string( WHEN ls_result-success = abap_true THEN 'X' ELSE '' ).
    ls_response-message = ls_result-message.
    ls_response-test_count = ls_result-test_count.
    ls_response-passed_count = ls_result-passed_count.
    ls_response-failed_count = ls_result-failed_count.
    ls_response-results = ls_result-results.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.

  METHOD run_unit_tests.
    rs_result-success = abap_false.
    rs_result-test_count = 0.
    rs_result-passed_count = 0.
    rs_result-failed_count = 0.

    DATA(lt_test_classes) TYPE ty_object_list.
    lt_test_classes = get_test_classes(
      iv_package = iv_package
      it_objects = it_objects ).

    IF lt_test_classes IS INITIAL.
      rs_result-message = 'No test classes found'.
      RETURN.
    ENDIF.

    rs_result-message = |Found { lines( lt_test_classes ) } test class(es)|.
    rs_result-results = run_aunit_tests( lt_test_classes ).

    IF rs_result-results IS INITIAL.
      rs_result-message = |No test results - { rs_result-message }|.
      RETURN.
    ENDIF.

    count_results(
      EXPORTING it_results = rs_result-results
      IMPORTING e_test_count = rs_result-test_count
                e_passed_count = rs_result-passed_count
                e_failed_count = rs_result-failed_count ).

    rs_result-success = COND #( WHEN rs_result-failed_count = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD get_test_classes.
    DATA lt_tadir TYPE TABLE OF tadir.
    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF lt_tadir.

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

    IF it_objects IS NOT INITIAL.
      LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
        APPEND INITIAL LINE TO rt_classes ASSIGNING <ls_class>.
        <ls_class>-object_type = <ls_obj>-object_type.
        <ls_class>-object_name = <ls_obj>-object_name.
      ENDLOOP.
    ENDIF.

    SORT rt_classes BY object_name.
    DELETE ADJACENT DUPLICATES FROM rt_classes COMPARING object_name.
  ENDMETHOD.

  METHOD run_aunit_tests.
    DATA: lo_runner TYPE REF TO cl_sut_aunit_runner.

    cl_sut_aunit_runner=>s_create(
      EXPORTING
        p_cov       = abap_false
        i_flg_api   = abap_true
      RECEIVING
        r_ref_runner = lo_runner ).

    lo_runner->p_disp = abap_false.
    lo_runner->p_save = abap_true.
    lo_runner->p_runmd = 'E'.

    DATA lv_test_classes TYPE string.
    LOOP AT it_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
      IF lv_test_classes IS INITIAL.
        lv_test_classes = <ls_class>-object_name.
      ELSE.
        lv_test_classes = |{ lv_test_classes } { <ls_class>-object_name }|.
      ENDIF.
    ENDLOOP.

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

    DATA(lt_objects) = lo_runner->tab_objects.

    IF lt_objects IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>).
      DATA(lv_obj_name) TYPE string.
      lv_obj_name = <ls_object>-obj_name.

      LOOP AT <ls_object>-tab_testclasses ASSIGNING FIELD-SYMBOL(<ls_tcl>).
        DATA(lv_tcl_name) TYPE string.
        lv_tcl_name = <ls_tcl>-testclass.

        LOOP AT <ls_tcl>-tab_methods ASSIGNING FIELD-SYMBOL(<ls_method>).
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

          DATA(ls_row) TYPE ty_test_result.
          ls_row = VALUE #( object_type = 'CLAS'
            object_name = lv_obj_name
            test_method = lv_methodname
            status = lv_kind
            message = lv_desc
            line = lv_src ).
          APPEND ls_row TO rt_results.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD count_results.
    e_test_count = lines( it_results ).
    e_passed_count = 0.
    e_failed_count = 0.

    LOOP AT it_results ASSIGNING FIELD-SYMBOL(<ls_result>).
      CASE <ls_result>-status.
        WHEN 'P' OR 'S'.
          e_passed_count = e_passed_count + 1.
        WHEN 'A' OR 'E' OR 'F'.
          e_failed_count = e_failed_count + 1.
        WHEN OTHERS.
          IF <ls_result>-message CS 'Passed' OR <ls_result>-message CS 'passed'.
            e_passed_count = e_passed_count + 1.
          ELSE.
            e_failed_count = e_failed_count + 1.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
