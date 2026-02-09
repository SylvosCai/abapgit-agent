" TODO: Implement detailed syntax error parsing
" When a syntax error occurs, the log shows the affected object name
" but not the specific line/column. For better error reporting:
" - Parse the error message to extract object info
" - For syntax errors, query SEPSA or TRINT_OBJECT_LOG for details
" - Return structured error with line numbers and fix suggestions

CLASS zcl_abgagt_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_abgagt_agent.

    METHODS: get_version RETURNING VALUE(rv_version) TYPE string.

  PRIVATE SECTION.

    " Local type for item signature (matches abapGit structure)
    TYPES: BEGIN OF ty_item_signature,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE devclass,
           END OF ty_item_signature.

    DATA: mo_repo TYPE REF TO zif_abapgit_repo.

    METHODS configure_credentials
      IMPORTING
        iv_url TYPE string
        iv_username TYPE string
        iv_password TYPE string
      RAISING zcx_abapgit_exception.

    METHODS parse_file_to_object
      IMPORTING
        iv_file TYPE string
      EXPORTING
        ev_obj_type TYPE string
        ev_obj_name TYPE string.

    METHODS prepare_deserialize_checks
      IMPORTING
        it_files TYPE string_table OPTIONAL
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING zcx_abapgit_exception.

    METHODS check_log_for_errors
      RETURNING
        VALUE(rv_has_error) TYPE abap_bool.

    METHODS get_log_detail
      RETURNING
        VALUE(rv_detail) TYPE string.

    METHODS get_object_lists
      RETURNING
        VALUE(rs_result) TYPE zif_abgagt_agent=>ty_result.

    METHODS handle_exception
      IMPORTING
        ix_exception TYPE REF TO cx_root
      RETURNING
        VALUE(rs_result) TYPE zif_abgagt_agent=>ty_result.

    METHODS get_test_classes
      IMPORTING
        iv_package TYPE devclass OPTIONAL
        it_objects TYPE zif_abgagt_agent=>ty_object_keys OPTIONAL
      RETURNING
        VALUE(rt_classes) TYPE zif_abgagt_agent=>ty_object_keys.

    METHODS run_aunit_tests
      IMPORTING
        it_classes TYPE zif_abgagt_agent=>ty_object_keys
      RETURNING
        VALUE(rt_results) TYPE zif_abgagt_agent=>ty_test_results.

    METHODS count_results
      IMPORTING
        it_results TYPE zif_abgagt_agent=>ty_test_results
      CHANGING
        rs_stats TYPE zif_abgagt_agent=>ty_unit_result.

ENDCLASS.

CLASS zcl_abgagt_agent IMPLEMENTATION.

  METHOD zif_abgagt_agent~pull.
    DATA: lv_job_id TYPE string.
    lv_job_id = |{ sy-uname }{ sy-datum }{ sy-uzeit }|.
    rs_result-job_id = lv_job_id.
    rs_result-success = abap_false.
    GET TIME STAMP FIELD rs_result-started_at.

    IF iv_url IS INITIAL.
      rs_result-message = 'URL is required'.
      RETURN.
    ENDIF.

    TRY.
        IF iv_username IS NOT INITIAL AND iv_password IS NOT INITIAL.
          configure_credentials(
            iv_url      = iv_url
            iv_username = iv_username
            iv_password = iv_password ).
        ENDIF.

        DATA: li_repo TYPE REF TO zif_abapgit_repo.
        zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
          EXPORTING iv_url = iv_url
          IMPORTING ei_repo = li_repo ).
        mo_repo = li_repo.

        IF mo_repo IS BOUND.
          mo_repo->refresh( ).

          DATA(ls_checks) = prepare_deserialize_checks( it_files = it_files ).

          mo_repo->create_new_log( ).

          mo_repo->deserialize(
            is_checks = ls_checks
            ii_log   = mo_repo->get_log( ) ).

          " Check the abapGit log for errors and extract object lists
          DATA(lv_has_error) = check_log_for_errors( ).
          DATA(lv_error_detail) = get_log_detail( ).

          " Extract activated and failed objects from the log
          DATA(ls_obj_result) = get_object_lists( ).

          rs_result-log_messages = ls_obj_result-log_messages.
          rs_result-activated_objects = ls_obj_result-activated_objects.
          rs_result-failed_objects = ls_obj_result-failed_objects.

          " Count objects
          rs_result-activated_count = lines( rs_result-activated_objects ).
          rs_result-failed_count = lines( rs_result-failed_objects ).

          GET TIME STAMP FIELD rs_result-finished_at.

          IF lv_has_error = abap_true.
            rs_result-message = 'Pull completed with errors'.
            rs_result-error_detail = lv_error_detail.
          ELSE.
            rs_result-success = abap_true.
            rs_result-message = 'Pull completed successfully'.
          ENDIF.
        ELSE.
          rs_result-message = |Repository not found: { iv_url }|.
          GET TIME STAMP FIELD rs_result-finished_at.
        ENDIF.

      CATCH zcx_abapgit_exception INTO DATA(lx_git).
        rs_result = handle_exception( ix_exception = lx_git ).
        GET TIME STAMP FIELD rs_result-finished_at.
      CATCH cx_root INTO DATA(lx_error).
        rs_result = handle_exception( ix_exception = lx_error ).
        GET TIME STAMP FIELD rs_result-finished_at.
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abgagt_agent~get_repo_status.
    DATA: li_repo TYPE REF TO zif_abapgit_repo.
    TRY.
        zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
          EXPORTING iv_url = iv_url
          IMPORTING ei_repo = li_repo ).
      CATCH zcx_abapgit_exception.
        rv_status = 'Not found'.
        RETURN.
    ENDTRY.

    IF li_repo IS BOUND.
      rv_status = 'Found'.
    ELSE.
      rv_status = 'Not found'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_agent~inspect.
    DATA ls_error LIKE LINE OF rs_result-errors.

    rs_result-success = abap_true.
    rs_result-object_type = iv_object_type.
    rs_result-object_name = iv_object_name.

    TRY.
        " Check if object exists in TADIR
        DATA lv_devclass TYPE devclass.
        SELECT SINGLE devclass FROM tadir
          INTO lv_devclass
          WHERE pgmid = 'R3TR'
            AND object = iv_object_type
            AND obj_name = iv_object_name.

        IF lv_devclass IS INITIAL.
          rs_result-success = abap_false.
          rs_result-error_count = 1.
          ls_error-line = '1'.
          ls_error-column = '1'.
          ls_error-text = |Object { iv_object_type } { iv_object_name } does not exist|.
          ls_error-word = ''.
          APPEND ls_error TO rs_result-errors.
          RETURN.
        ENDIF.

        " Create object structure for the specific object
        DATA ls_obj TYPE scir_objs.
        ls_obj-objtype = iv_object_type.
        ls_obj-objname = iv_object_name.

        DATA lt_objects TYPE scit_objs.
        APPEND ls_obj TO lt_objects.

        " Create unique name for inspection
        DATA lv_name TYPE sci_objs.
        CONCATENATE 'SYNT_' sy-uname sy-datum sy-uzeit INTO lv_name.

        " Create object set
        DATA(lo_objset) = cl_ci_objectset=>save_from_list(
          p_name    = lv_name
          p_objects = lt_objects ).

        " Get check variant for syntax check
        DATA(lo_variant) = cl_ci_checkvariant=>get_ref(
          p_user = ''
          p_name = 'SYNTAX_CHECK' ).

        " Create inspection
        cl_ci_inspection=>create(
          EXPORTING
            p_user = sy-uname
            p_name = lv_name
          RECEIVING
            p_ref = DATA(lo_inspection) ).

        " Set inspection with object set and variant
        lo_inspection->set(
          EXPORTING
            p_chkv = lo_variant
            p_objs = lo_objset ).

        " Save inspection
        lo_inspection->save( ).

        " Run inspection directly
        lo_inspection->run(
          EXPORTING
            p_howtorun = 'D'
          EXCEPTIONS
            invalid_check_version = 1
            OTHERS = 2 ).

        " Get results
        DATA lt_list TYPE scit_alvlist.
        lo_inspection->plain_list( IMPORTING p_list = lt_list ).

        " Parse results
        LOOP AT lt_list INTO DATA(ls_list).
          " Only include errors for the requested object
          IF ls_list-objtype = iv_object_type AND ls_list-objname = iv_object_name.
            CLEAR ls_error.
            ls_error-line = ls_list-line.
            ls_error-column = ls_list-col.
            ls_error-text = ls_list-text.
            ls_error-word = ls_list-code.
            APPEND ls_error TO rs_result-errors.
          ENDIF.
        ENDLOOP.

        " Cleanup
        lo_inspection->delete(
          EXCEPTIONS
            locked = 1
            error_in_enqueue = 2
            not_authorized = 3
            exceptn_appl_exists = 4
            OTHERS = 5 ).

        lo_objset->delete(
          EXCEPTIONS
            exists_in_insp = 1
            locked = 2
            error_in_enqueue = 3
            not_authorized = 4
            exists_in_objs = 5
            OTHERS = 6 ).

        rs_result-error_count = lines( rs_result-errors ).
        IF rs_result-error_count > 0.
          rs_result-success = abap_false.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        ls_error-line = '1'.
        ls_error-column = '1'.
        ls_error-text = lx_error->get_text( ).
        ls_error-word = ''.
        APPEND ls_error TO rs_result-errors.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_abgagt_agent~run_tests.
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

  METHOD configure_credentials.
    zcl_abapgit_persist_factory=>get_user( )->set_repo_git_user_name(
      iv_url = iv_url iv_username = iv_username ).
    zcl_abapgit_persist_factory=>get_user( )->set_repo_login(
      iv_url = iv_url iv_login = iv_username ).
    zcl_abapgit_login_manager=>set_basic(
      iv_uri      = iv_url
      iv_username = iv_username
      iv_password = iv_password ).
  ENDMETHOD.

  METHOD parse_file_to_object.
    " Parse file path to extract obj_type and obj_name
    " Example: "zcl_my_class.clas.abap" -> CLAS, ZCL_MY_CLASS
    " Example: "src/zcl_my_class.clas.abap" -> CLAS, ZCL_MY_CLASS

    DATA lv_upper TYPE string.
    lv_upper = iv_file.
    TRANSLATE lv_upper TO UPPER CASE.

    " Split filename by '.' to get parts
    DATA lt_parts TYPE TABLE OF string.
    SPLIT lv_upper AT '.' INTO TABLE lt_parts.
    DATA lv_part_count TYPE i.
    lv_part_count = lines( lt_parts ).

    IF lv_part_count < 3.
      RETURN.
    ENDIF.

    " Last part should be 'ABAP' for verification
    READ TABLE lt_parts INDEX lv_part_count INTO DATA(lv_last).
    IF lv_last <> 'ABAP'.
      RETURN.
    ENDIF.

    " First part is obj_name (may contain path), second part is obj_type
    DATA lv_obj_name TYPE string.
    DATA lv_obj_type_raw TYPE string.
    READ TABLE lt_parts INDEX 1 INTO lv_obj_name.
    READ TABLE lt_parts INDEX 2 INTO lv_obj_type_raw.

    " Convert file extension to object type
    IF lv_obj_type_raw = 'CLASS'.
      ev_obj_type = 'CLAS'.
    ELSE.
      ev_obj_type = lv_obj_type_raw.
    ENDIF.

    " Extract file name from obj_name (remove path prefix)
    DATA lv_len TYPE i.
    lv_len = strlen( lv_obj_name ).
    DATA lv_offs TYPE i.
    lv_offs = find( val = reverse( lv_obj_name ) sub = '/' ).
    IF lv_offs > 0.
      lv_offs = lv_len - lv_offs - 1.
      lv_obj_name = lv_obj_name+lv_offs.
    ENDIF.

    " Remove leading '/' if present
    IF lv_obj_name(1) = '/'.
      lv_obj_name = lv_obj_name+1.
    ENDIF.

    ev_obj_name = lv_obj_name.
  ENDMETHOD.

  METHOD prepare_deserialize_checks.
    rs_checks = mo_repo->deserialize_checks( ).

    " If specific files requested, build lookup table of (obj_type, obj_name)
    DATA lt_valid_files TYPE HASHED TABLE OF zif_abapgit_definitions=>ty_item_signature
                            WITH UNIQUE KEY obj_type obj_name.
    IF it_files IS SUPPLIED AND lines( it_files ) > 0.
      LOOP AT it_files INTO DATA(lv_file).
        DATA lv_obj_type TYPE string.
        DATA lv_obj_name TYPE string.
        parse_file_to_object(
          EXPORTING iv_file = lv_file
          IMPORTING ev_obj_type = lv_obj_type
                    ev_obj_name = lv_obj_name ).

        IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
          DATA ls_sig TYPE zif_abapgit_definitions=>ty_item_signature.
          ls_sig-obj_type = lv_obj_type.
          ls_sig-obj_name = lv_obj_name.
          INSERT ls_sig INTO TABLE lt_valid_files.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Set decision for each file
    LOOP AT rs_checks-overwrite INTO DATA(ls_overwrite).
      IF it_files IS SUPPLIED AND lines( it_files ) > 0.
        " Check if file is in valid files list
        READ TABLE lt_valid_files WITH TABLE KEY obj_type = ls_overwrite-obj_type
                                             obj_name = ls_overwrite-obj_name
                                     TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          ls_overwrite-decision = zif_abapgit_definitions=>c_yes.
        ELSE.
          ls_overwrite-decision = zif_abapgit_definitions=>c_no.
        ENDIF.
      ELSE.
        " No files specified - deserialize all
        ls_overwrite-decision = zif_abapgit_definitions=>c_yes.
      ENDIF.
      MODIFY rs_checks-overwrite FROM ls_overwrite.
    ENDLOOP.

    DATA: lo_settings TYPE REF TO zcl_abapgit_settings.
    lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    lo_settings->set_activate_wo_popup( abap_true ).
  ENDMETHOD.

  METHOD check_log_for_errors.
    DATA: lo_log TYPE REF TO zif_abapgit_log.

    rv_has_error = abap_false.

    lo_log = mo_repo->get_log( ).
    IF lo_log IS BOUND.
      DATA(lv_status) = lo_log->get_status( ).
      IF lv_status = zif_abapgit_log=>c_status-error.
        rv_has_error = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_log_detail.
    " Extract detailed log messages including type, id, number, text, obj_type, obj_name, exception
    DATA: lo_log TYPE REF TO zif_abapgit_log.

    rv_detail = ''.

    lo_log = mo_repo->get_log( ).
    IF lo_log IS BOUND.
      DATA: lt_messages TYPE zif_abapgit_log=>ty_log_outs.
      DATA: ls_msg TYPE zif_abapgit_log=>ty_log_out.
      lt_messages = lo_log->get_messages( ).

      DATA lv_first TYPE abap_bool VALUE abap_false.

      LOOP AT lt_messages INTO ls_msg.
        IF ls_msg-type = 'E' OR ls_msg-type = 'A' OR ls_msg-type = 'W'.
          DATA: lv_msg TYPE string.
          IF ls_msg-obj_type IS NOT INITIAL AND ls_msg-obj_name IS NOT INITIAL.
            lv_msg = |{ ls_msg-obj_type } { ls_msg-obj_name }: { ls_msg-text }|.
          ELSE.
            lv_msg = ls_msg-text.
          ENDIF.
          " Add exception text if available
          IF ls_msg-exception IS BOUND.
            lv_msg = |{ lv_msg }\nException: { ls_msg-exception->get_text( ) }|.
          ENDIF.
          IF lv_first = abap_false.
            rv_detail = lv_msg.
            lv_first = abap_true.
          ELSE.
            rv_detail = |{ rv_detail }&&&{ lv_msg }|.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " Replace marker with newline for display
      IF rv_detail IS NOT INITIAL.
        rv_detail = replace( val = rv_detail sub = '&&&' with = cl_abap_char_utilities=>newline ).
        rv_detail = |Error Details:{ cl_abap_char_utilities=>newline }{ rv_detail }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_object_lists.
    " Extract activated and failed objects from the log with full details
    DATA: lo_log TYPE REF TO zif_abapgit_log.
    DATA: lv_key TYPE string.

    CLEAR: rs_result-log_messages, rs_result-activated_objects, rs_result-failed_objects.

    lo_log = mo_repo->get_log( ).
    IF lo_log IS BOUND.
      DATA: lt_messages TYPE zif_abapgit_log=>ty_log_outs.
      DATA: ls_msg TYPE zif_abapgit_log=>ty_log_out.
      lt_messages = lo_log->get_messages( ).

      LOOP AT lt_messages INTO ls_msg.
        DATA: ls_object TYPE zif_abgagt_agent=>ty_object.
        ls_object-type = ls_msg-type.
        ls_object-id = ls_msg-id.
        ls_object-number = ls_msg-number.
        ls_object-text = ls_msg-text.
        ls_object-obj_type = ls_msg-obj_type.
        ls_object-obj_name = ls_msg-obj_name.

        " Exception is a REF, need to convert to string
        " Also append exception text to the message for better error reporting
        IF ls_msg-exception IS BOUND.
          DATA: lv_exc_text TYPE string.
          lv_exc_text = ls_msg-exception->get_text( ).
          ls_object-exception = lv_exc_text.
          " Append exception text to the main text if it's not already there
          IF lv_exc_text IS NOT INITIAL AND ls_msg-text NA lv_exc_text.
            ls_object-text = |{ ls_msg-text }\nException: { lv_exc_text }|.
          ENDIF.
        ENDIF.

        " Add all messages to log_messages table
        APPEND ls_object TO rs_result-log_messages.

        " Success messages (type 'S') - add to activated objects if unique
        IF ls_msg-type = 'S' AND ls_msg-obj_type IS NOT INITIAL AND ls_msg-obj_name IS NOT INITIAL.
          " Check for duplicates
          lv_key = |{ ls_msg-obj_type }{ ls_msg-obj_name }|.
          READ TABLE rs_result-activated_objects WITH KEY obj_type = ls_msg-obj_type
                                                        obj_name = ls_msg-obj_name
                                                  TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_object TO rs_result-activated_objects.
          ENDIF.
        ENDIF.

        " Error/Abort/Warning messages - add to failed objects
        IF ls_msg-type = 'E' OR ls_msg-type = 'A' OR ls_msg-type = 'W'.
          APPEND ls_object TO rs_result-failed_objects.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD handle_exception.
    rs_result-success = abap_false.
    rs_result-message = ix_exception->get_text( ).

    DATA: lx_prev TYPE REF TO cx_root.
    lx_prev = ix_exception->previous.
    WHILE lx_prev IS BOUND.
      DATA: lv_msg TYPE string.
      lv_msg = lx_prev->get_text( ).
      IF lv_msg IS NOT INITIAL.
        rs_result-error_detail = rs_result-error_detail && '\n  -> ' && lv_msg.
      ENDIF.
      lx_prev = lx_prev->previous.
    ENDWHILE.
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
    lo_runner->p_save = abap_true.      " Save values
    lo_runner->p_runmd = 'E'.           " Execute only (not plan)

    " Set test classes
    DATA lv_test_classes TYPE string.
    LOOP AT it_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
      IF lv_test_classes IS INITIAL.
        lv_test_classes = <ls_class>-object_name.
      ELSE.
        lv_test_classes = |{ lv_test_classes } { <ls_class>-object_name }|.
      ENDIF.
    ENDLOOP.

    " Pass test class names via RFC enablement
    " The runner will execute the tests

    " Run tests
    lo_runner->run(
      EXPORTING
        i_flg_select_only = abap_false
      EXCEPTIONS
        OTHERS = 1 ).

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

          DATA(ls_result) = VALUE zif_abgagt_agent=>ty_test_result(
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

  METHOD get_version.
    rv_version = '1.0.0'.
  ENDMETHOD.

ENDCLASS.
