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

    CONSTANTS gc_variant TYPE sci_chkv VALUE 'SWF_ABAP_UNIT'.

    METHODS run_tests
      IMPORTING
        iv_package TYPE devclass OPTIONAL
        it_objects TYPE ty_object_list OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ty_result.

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

    " Build object set for Code Inspector
    DATA(lt_objects) = build_object_set( lt_test_classes ).

    IF lt_objects IS INITIAL.
      rs_result-message = 'Could not build object set'.
      RETURN.
    ENDIF.

    " Run inspection with SWF_ABAP_UNIT variant
    DATA(lt_results) = run_inspection(
      it_objects = lt_objects
      iv_name    = |UNIT_{ sy-uname }_{ sy-datum }_{ sy-uzeit }| ).

    IF lt_results IS INITIAL.
      rs_result-message = |No test results - { rs_result-message }|.
      RETURN.
    ENDIF.

    " Convert and count results
    rs_result-results = convert_results( lt_results ).
    count_results( EXPORTING it_results = rs_result-results CHANGING rs_stats = rs_result ).

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

  METHOD build_object_set.
    " Build object set for Code Inspector
    DATA: ls_obj TYPE scir_objs.

    LOOP AT it_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
      ls_obj-objtype = <ls_class>-object_type.
      ls_obj-objname = <ls_class>-object_name.
      APPEND ls_obj TO rt_objects.
    ENDLOOP.

  ENDMETHOD.

  METHOD run_inspection.
    DATA: lo_objset TYPE REF TO cl_ci_objectset.
    DATA: lo_variant TYPE REF TO cl_ci_checkvariant.
    DATA: lo_inspection TYPE REF TO cl_ci_inspection.
    DATA: lt_list TYPE scit_alvlist.

    TRY.
        " Create object set
        lo_objset = cl_ci_objectset=>save_from_list(
          p_name    = iv_name
          p_objects = it_objects ).

        " Get unit test check variant
        lo_variant = cl_ci_checkvariant=>get_ref(
          p_user = ''
          p_name = gc_variant ).

        IF lo_variant IS NOT BOUND.
          " Variant not found - try default AUNIT variant
          lo_variant = cl_ci_checkvariant=>get_ref(
            p_user = ''
            p_name = 'DEFAULT' ).
        ENDIF.

        IF lo_variant IS NOT BOUND.
          " Cannot get variant - return empty
          RETURN.
        ENDIF.

        " Create inspection
        cl_ci_inspection=>create(
          EXPORTING
            p_user = sy-uname
            p_name = iv_name
          RECEIVING
            p_ref = lo_inspection ).

        " Set inspection with variant and object set
        lo_inspection->set(
          EXPORTING
            p_chkv = lo_variant
            p_objs = lo_objset ).

        " Run inspection
        lo_inspection->run(
          EXPORTING
            p_howtorun = 'D'
          EXCEPTIONS
            invalid_check_version = 1
            OTHERS = 2 ).

        IF sy-subrc <> 0.
          " Run failed - try without D option
          lo_inspection->run(
            EXPORTING
              p_howtorun = 'P'
            EXCEPTIONS
              invalid_check_version = 1
              OTHERS = 2 ).
        ENDIF.

        " Get results
        lo_inspection->plain_list( IMPORTING p_list = lt_list ).

        " Cleanup
        lo_inspection->delete( EXCEPTIONS locked = 1 OTHERS = 2 ).
        lo_objset->delete( EXCEPTIONS exists_in_insp = 1 locked = 2 OTHERS = 3 ).

      CATCH cx_root INTO DATA(lx_error).
        " Return empty on error
        RETURN.
    ENDTRY.

    rt_results = lt_list.

  ENDMETHOD.

  METHOD convert_results.
    " Convert Code Inspector results to our format
    DATA: ls_result LIKE LINE OF rt_results.

    LOOP AT it_alv ASSIGNING FIELD-SYMBOL(<ls_alv>).
      CLEAR ls_result.

      ls_result-object_type = <ls_alv>-objtype.
      ls_result-object_name = <ls_alv>-objname.
      ls_result-line = <ls_alv>-line.

      " Extract test method from text (format: "CLASS=>METHOD" or "CLASS METHOD")
      DATA(lv_text) = <ls_alv>-text.
      IF lv_text CP '*=>*'.
        SPLIT lv_text AT '=>' INTO DATA(lv_class) DATA(lv_method).
        ls_result-test_method = lv_method.
      ELSEIF lv_text CP '* *'.
        " Try space separation
        SPLIT lv_text AT ' ' INTO DATA(lv_class2) DATA(lv_method2).
        ls_result-test_method = lv_method2.
      ELSE.
        ls_result-test_method = lv_text.
      ENDIF.

      " Determine status based on kind
      CASE <ls_alv>-kind.
        WHEN 'S'.  " Success
          ls_result-status = 'PASSED'.
          ls_result-message = <ls_alv>-text.
        WHEN 'W'.  " Warning
          ls_result-status = 'PASSED'.
          ls_result-message = <ls_alv>-text.
        WHEN 'E' OR 'A'.  " Error or Abort
          ls_result-status = 'FAILED'.
          ls_result-message = <ls_alv>-text.
        WHEN OTHERS.
          ls_result-status = 'UNKNOWN'.
          ls_result-message = <ls_alv>-text.
      ENDCASE.

      APPEND ls_result TO rt_results.
    ENDLOOP.

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
