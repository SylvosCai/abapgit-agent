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

    TYPES: BEGIN OF ty_key,
             obj_name TYPE tadir-obj_name,
             obj_type TYPE tadir-object,
           END OF ty_key.

    TYPES ty_keys TYPE STANDARD TABLE OF ty_key WITH DEFAULT KEY.

    METHODS run_tests
      IMPORTING it_keys TYPE ty_keys
      RETURNING VALUE(rt_results) TYPE ty_test_results.

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

    IF lt_keys IS INITIAL.
      ls_result-message = 'No files or package provided'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Remove duplicates
    SORT lt_keys BY obj_type obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_keys COMPARING obj_type obj_name.

    " Run tests
    ls_result-results = run_tests( lt_keys ).

    IF ls_result-results IS INITIAL.
      ls_result-message = 'No test results returned'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Count results
    ls_result-test_count = lines( ls_result-results ).

    LOOP AT ls_result-results ASSIGNING FIELD-SYMBOL(<ls_test>).
      CASE <ls_test>-status.
        WHEN 'P' OR 'S' OR 'passed'.  " Passed or Success
          ls_result-passed_count = ls_result-passed_count + 1.
        WHEN 'A' OR 'E' OR 'F' OR 'failed' OR 'error'.  " Abort, Error, or Failed
          ls_result-failed_count = ls_result-failed_count + 1.
      ENDCASE.
    ENDLOOP.

    IF ls_result-failed_count = 0.
      ls_result-success = abap_true.
      ls_result-message = |All { ls_result-test_count } tests passed|.
    ELSE.
      ls_result-message = |{ ls_result-failed_count } of { ls_result-test_count } tests failed|.
    ENDIF.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD run_tests.
    DATA: lo_passport TYPE REF TO object,
          lo_runner TYPE REF TO object,
          li_result TYPE REF TO data.

    FIELD-SYMBOLS: <li_result> TYPE any,
                   <lt_task_data> TYPE any,
                   <lt_indices> TYPE ANY TABLE,
                   <lt_programs> TYPE ANY TABLE,
                   <ls_program> TYPE any,
                   <lv_any> TYPE any,
                   <lt_classes> TYPE ANY TABLE,
                   <ls_class> TYPE any,
                   <lt_methods> TYPE ANY TABLE,
                   <ls_method> TYPE any.

    TRY.
        " Get passport from SAPLSAUCV_GUI_RUNNER
        CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get
          RECEIVING
            result = lo_passport.

        " Create runner
        CALL METHOD ('CL_AUCV_TEST_RUNNER_STANDARD')=>create
          EXPORTING
            i_passport = lo_passport
          RECEIVING
            result     = lo_runner.
      CATCH cx_root.
        " Fallback: return error message
        RETURN.
    ENDTRY.

    CREATE DATA li_result TYPE REF TO ('IF_SAUNIT_INTERNAL_RESULT').
    ASSIGN li_result->* TO <li_result>.

    " Run tests for the program keys
    CALL METHOD lo_runner->('RUN_FOR_PROGRAM_KEYS')
      EXPORTING
        i_limit_on_duration_category = '36' " long
        i_limit_on_risk_level        = '33' " critical
        i_program_keys               = it_keys
      IMPORTING
        e_aunit_result               = <li_result>.

    " Parse results
    ASSIGN COMPONENT 'F_TASK_DATA' OF STRUCTURE <li_result> TO <lt_task_data>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT 'ALERTS_BY_INDICIES' OF STRUCTURE <lt_task_data> TO <lt_indices>.
    ASSIGN COMPONENT 'PROGRAMS' OF STRUCTURE <lt_task_data> TO <lt_programs>.

    IF <lt_programs> IS INITIAL.
      RETURN.
    ENDIF.

    " Loop through programs
    LOOP AT <lt_programs> ASSIGNING <ls_program>.
      DATA(lv_program_ndx) = sy-tabix.

      " Get object info
      DATA(lv_obj_type) = ''.
      DATA(lv_obj_name) = ''.

      " Try to get KEY field (exists in newer releases)
      ASSIGN COMPONENT 'INFO-KEY-OBJ_TYPE' OF STRUCTURE <ls_program> TO <lv_any>.
      IF sy-subrc = 0.
        lv_obj_type = <lv_any>.
        ASSIGN COMPONENT 'INFO-KEY-OBJ_NAME' OF STRUCTURE <ls_program> TO <lv_any>.
        IF sy-subrc = 0.
          lv_obj_name = <lv_any>.
        ENDIF.
      ELSE.
        " Fallback: use INFO-NAME
        ASSIGN COMPONENT 'INFO-NAME' OF STRUCTURE <ls_program> TO <lv_any>.
        IF sy-subrc = 0.
          lv_obj_name = <lv_any>.
        ENDIF.
      ENDIF.

      " Get classes
      ASSIGN COMPONENT 'CLASSES' OF STRUCTURE <ls_program> TO <lt_classes>.
      IF <lt_classes> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      " Loop through test classes
      LOOP AT <lt_classes> ASSIGNING <ls_class>.
        DATA(lv_class_ndx) = sy-tabix.

        " Get class name
        DATA(lv_class_name) = ''.
        ASSIGN COMPONENT 'INFO-NAME' OF STRUCTURE <ls_class> TO <lv_any>.
        IF sy-subrc = 0.
          lv_class_name = <lv_any>.
        ENDIF.

        " Get methods
        ASSIGN COMPONENT 'METHODS' OF STRUCTURE <ls_class> TO <lt_methods>.
        IF <lt_methods> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        " Loop through test methods
        LOOP AT <lt_methods> ASSIGNING <ls_method>.
          DATA(lv_method_name) = ''.
          DATA(lv_status) = ''.
          DATA(lv_message) = ''.
          DATA(lv_source) = ''.

          ASSIGN COMPONENT 'INFO-NAME' OF STRUCTURE <ls_method> TO <lv_any>.
          IF sy-subrc = 0.
            lv_method_name = <lv_any>.
          ENDIF.

          ASSIGN COMPONENT 'KIND' OF STRUCTURE <ls_method> TO <lv_any>.
          IF sy-subrc = 0.
            lv_status = <lv_any>.
          ENDIF.

          ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <ls_method> TO <lv_any>.
          IF sy-subrc = 0.
            lv_message = <lv_any>.
          ENDIF.

          ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <ls_method> TO <lv_any>.
          IF sy-subrc = 0.
            lv_source = <lv_any>.
          ENDIF.

          " Check if this method has alerts (failures)
          DATA(lv_has_error) = abap_false.

          LOOP AT <lt_indices> ASSIGNING FIELD-SYMBOL(<ls_index>).
            DATA: lt_alerts TYPE ANY TABLE.
            ASSIGN COMPONENT 'ALERTS' OF STRUCTURE <ls_index> TO <lt_alerts>.
            IF sy-subrc = 0 AND <lt_alerts> IS ASSIGNED.
              LOOP AT <lt_alerts> ASSIGNING FIELD-SYMBOL(<ls_alert>).
                DATA: lv_kind TYPE string.
                ASSIGN COMPONENT 'KIND' OF STRUCTURE <ls_alert> TO <lv_any>.
                IF sy-subrc = 0.
                  lv_kind = <lv_any>.
                ENDIF.
                IF lv_kind = 'F' OR lv_kind = 'E'.  " Failed or Error
                  " Get the error message
                  DATA: lt_params TYPE string_table,
                        lv_param TYPE string.
                  ASSIGN COMPONENT 'HEADER-PARAMS' OF STRUCTURE <ls_alert> TO <lt_params>.
                  IF sy-subrc = 0 AND <lt_params> IS ASSIGNED.
                    LOOP AT <lt_params> INTO lv_param.
                      IF lv_message IS INITIAL.
                        lv_message = lv_param.
                      ELSE.
                        lv_message = lv_message && | | && lv_param.
                      ENDIF.
                    ENDLOOP.
                  ENDIF.
                  lv_has_error = abap_true.
                  EXIT.
                ENDIF.
              ENDLOOP.
              IF lv_has_error = abap_true.
                EXIT.
              ENDIF.
            ENDIF.
          ENDLOOP.

          " Add result
          DATA(ls_result) = VALUE ty_test_result(
            object_type = lv_obj_type
            object_name = lv_obj_name
            test_method = lv_method_name
            status = lv_status
            message = lv_message
            line = lv_source
          ).
          APPEND ls_result TO rt_results.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
