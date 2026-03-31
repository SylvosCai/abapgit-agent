" TODO: Implement detailed syntax error parsing
" When a syntax error occurs, the log shows the affected object name
" but not the specific line/column. For better error reporting:
" - Parse the error message to extract object info
" - For syntax errors, query SEPSA or TRINT_OBJECT_LOG for details
" - Return structured error with line numbers and fix suggestions

CLASS zcl_abgagt_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_abgagt_agent.

    METHODS constructor
      IMPORTING
        io_repo              TYPE REF TO zif_abapgit_repo             OPTIONAL
        io_conflict_detector TYPE REF TO zif_abgagt_conflict_detector OPTIONAL
        io_rtti              TYPE REF TO zif_abgagt_rtti_provider      OPTIONAL.

    METHODS: get_version RETURNING VALUE(rv_version) TYPE string.

  PROTECTED SECTION.

    " Local type for item signature (matches abapGit structure)
    TYPES: BEGIN OF ty_item_signature,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE devclass,
           END OF ty_item_signature.

    DATA: mo_repo              TYPE REF TO zif_abapgit_repo.
    DATA: mo_conflict_detector TYPE REF TO zif_abgagt_conflict_detector.
    DATA: mo_obj_filter        TYPE REF TO zif_abapgit_object_filter.
    DATA: mo_rtti              TYPE REF TO zif_abgagt_rtti_provider.
    " Remote files fetched pre-pull in build_file_entries_from_remote.
    " Reused in get_local_xml_files — get_files_remote() after the pull may
    " re-fetch from GitHub (invalidated cache) causing a slow second round-trip.
    " Pre-pull remote content is exactly what we need: the git file as committed.
    DATA: mt_remote_files      TYPE zif_abapgit_git_definitions=>ty_files_tt.
    " Error text from get_files_remote() failure, stored for diagnostics.
    DATA: mv_remote_fetch_error TYPE string.

    METHODS configure_credentials
      IMPORTING
        iv_url TYPE string
        iv_username TYPE string
        iv_password TYPE string
      RAISING zcx_abapgit_exception.

    METHODS prepare_deserialize_checks
      IMPORTING
        it_files             TYPE string_table OPTIONAL
        iv_transport_request TYPE string OPTIONAL
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING zcx_abapgit_exception.

    METHODS check_log_for_errors
      RETURNING
        VALUE(rv_has_error) TYPE abap_bool.

    METHODS build_file_entries_from_remote
      IMPORTING
        it_files               TYPE string_table OPTIONAL
      RETURNING
        VALUE(rt_entries) TYPE zif_abgagt_conflict_detector=>ty_file_entries
      RAISING
        zcx_abapgit_exception.

    METHODS build_obj_filter
      IMPORTING
        it_files TYPE string_table OPTIONAL.

    METHODS get_log_detail
      RETURNING
        VALUE(rv_detail) TYPE string.

    METHODS get_object_lists
      RETURNING
        VALUE(rs_result) TYPE zif_abgagt_agent=>ty_result.

    METHODS filter_param_available
      RETURNING
        VALUE(rv_available) TYPE abap_bool.

    METHODS handle_exception
      IMPORTING
        ix_exception TYPE REF TO cx_root
      RETURNING
        VALUE(rs_result) TYPE zif_abgagt_agent=>ty_result.

    METHODS paths_param_available
      RETURNING
        VALUE(rv_available) TYPE abap_bool.

    METHODS resolve_repo
      IMPORTING
        iv_url        TYPE string
      RETURNING
        VALUE(rv_found) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS select_branch
      IMPORTING
        iv_branch TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS run_conflict_check
      IMPORTING
        it_files           TYPE string_table OPTIONAL
        iv_branch          TYPE string       OPTIONAL
        iv_conflict_mode   TYPE string       OPTIONAL
      EXPORTING
        et_file_entries    TYPE zif_abgagt_conflict_detector=>ty_file_entries
        ev_conflict_count  TYPE i
        ev_conflict_report TYPE string
      RETURNING
        VALUE(rv_aborted)  TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS run_deserialize
      IMPORTING
        is_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_exception.

    METHODS build_pull_result
      IMPORTING
        it_file_entries TYPE zif_abgagt_conflict_detector=>ty_file_entries OPTIONAL
        iv_branch       TYPE string OPTIONAL
      CHANGING
        cs_result       TYPE zif_abgagt_agent=>ty_result.

    METHODS get_local_xml_files
      RETURNING
        VALUE(rt_xml_files) TYPE zif_abgagt_agent=>ty_xml_files.

ENDCLASS.

CLASS zcl_abgagt_agent IMPLEMENTATION.

  METHOD constructor.
    IF io_repo IS BOUND.
      mo_repo = io_repo.
    ENDIF.
    IF io_conflict_detector IS BOUND.
      mo_conflict_detector = io_conflict_detector.
    ELSE.
      mo_conflict_detector = zcl_abgagt_conflict_detector=>get_instance( ).
    ENDIF.
    mo_rtti = COND #( WHEN io_rtti IS BOUND THEN io_rtti
                      ELSE NEW lcl_rtti_provider( ) ).
  ENDMETHOD.

  METHOD zif_abgagt_agent~pull.
    rs_result-job_id            = |{ sy-uname }{ sy-datum }{ sy-uzeit }|.
    rs_result-success           = abap_false.
    rs_result-transport_request = iv_transport_request.
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

        IF resolve_repo( iv_url ) = abap_false.
          rs_result-message = |Repository not found: { iv_url }|.
          GET TIME STAMP FIELD rs_result-finished_at.
          RETURN.
        ENDIF.

        select_branch( iv_branch ).
        build_obj_filter( it_files ).

        DATA lt_file_entries TYPE zif_abgagt_conflict_detector=>ty_file_entries.
        DATA lv_conflict_count TYPE i.
        DATA lv_conflict_report TYPE string.

        IF run_conflict_check(
               EXPORTING
                 it_files           = it_files
                 iv_branch          = iv_branch
                 iv_conflict_mode   = iv_conflict_mode
               IMPORTING
                 et_file_entries    = lt_file_entries
                 ev_conflict_count  = lv_conflict_count
                 ev_conflict_report = lv_conflict_report ) = abap_true.
            rs_result-conflict_count  = lv_conflict_count.
            rs_result-conflict_report = lv_conflict_report.
            rs_result-message = |Pull aborted — { lv_conflict_count } conflict(s) detected|.
            GET TIME STAMP FIELD rs_result-finished_at.
            RETURN.
          ENDIF.

        DATA(ls_checks) = prepare_deserialize_checks(
          it_files             = it_files
          iv_transport_request = iv_transport_request ).

        DATA lv_activation_cancelled TYPE abap_bool VALUE abap_false.
        TRY.
            run_deserialize( ls_checks ).
          CATCH zcx_abapgit_exception INTO DATA(lx_deser).
            " "Activation cancelled" means nothing needed to be re-activated.
            " Treat it as a non-error so XML sync can still check for file drift.
            IF lx_deser->get_text( ) CS 'Activation cancelled' OR
               lx_deser->get_text( ) CS 'activation cancelled'.
              lv_activation_cancelled = abap_true.
            ELSE.
              RAISE EXCEPTION lx_deser.
            ENDIF.
          CATCH cx_root.
            " CX_SY_NO_HANDLER (wrapping CX_SY_RANGE_OUT_OF_BOUNDS) can escape
            " abapGit's deserialize_step when an individual object's deserialize()
            " raises a non-zcx_abapgit_exception inside a RAISING zcx_abapgit_exception
            " method boundary. Proceed to build_pull_result so the pull returns
            " "Pull completed" rather than failing with an unhandled exception.
            " (Objects processed before the crash are activated; skipped ones are not.)
        ENDTRY.

        " Populate mt_remote_files after deserialize — abapGit's internal remote
        " cache (mt_remote) is warm at this point (populated by find_remote_dot_abapgit
        " inside deserialize). get_files_remote() here reads from memory only, no
        " network call. Pre-pull fetch in build_file_entries_from_remote may fail
        " with transient git errors (e.g. "Unexpected pack header") so we refresh
        " here as a reliable fallback.
        IF mt_remote_files IS INITIAL.
          TRY.
              IF mo_obj_filter IS BOUND.
                mt_remote_files = mo_repo->get_files_remote( mo_obj_filter ).
              ELSE.
                mt_remote_files = mo_repo->get_files_remote( ).
              ENDIF.
            CATCH zcx_abapgit_exception cx_root.
              " Still unavailable — XML sync will be skipped
          ENDTRY.
        ENDIF.

        IF lv_activation_cancelled = abap_true.
          " Nothing was re-activated — skip log checks, but still run XML sync
          " so git files that differ from the serializer are reported as drift.
          rs_result-message        = 'Activation cancelled. Check the inactive objects.'.
          rs_result-local_xml_files = get_local_xml_files( ).
          GET TIME STAMP FIELD rs_result-finished_at.
        ELSE.
          build_pull_result(
            EXPORTING
              it_file_entries = lt_file_entries
              iv_branch       = iv_branch
            CHANGING
              cs_result       = rs_result ).
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
    IF mo_repo IS BOUND.
      li_repo = mo_repo.
    ELSE.
      TRY.
          zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
            EXPORTING iv_url = iv_url
            IMPORTING ei_repo = li_repo ).
        CATCH zcx_abapgit_exception.
          rv_status = 'Not found'.
          RETURN.
      ENDTRY.
    ENDIF.
    rv_status = COND #( WHEN li_repo IS BOUND THEN 'Found' ELSE 'Not found' ).
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

  METHOD prepare_deserialize_checks.

    " Build lookup table for DECISION loop from the files list
    DATA lt_valid_files TYPE HASHED TABLE OF zif_abapgit_definitions=>ty_item_signature
                            WITH UNIQUE KEY obj_type obj_name.

    IF it_files IS SUPPLIED AND lines( it_files ) > 0.
      LOOP AT it_files INTO DATA(lv_file).
        DATA lv_obj_type TYPE string.
        DATA lv_obj_name TYPE string.
        zcl_abgagt_util=>get_instance( )->parse_file_to_object(
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

    " Build object filter for partial download when --files is used.
    " Stored as instance variable so pull() can reuse it for deserialize().
    build_obj_filter( it_files ).

    " Call deserialize_checks — pass ii_obj_filter dynamically if the installed
    " abapGit version supports it; fall back to plain call for older versions.
    " Wrap in cx_root catch: get_files_local() inside abapGit can raise
    " CX_SY_RANGE_OUT_OF_BOUNDS (e.g. broken serializer for an object in the
    " package) which crosses a RAISING zcx_abapgit_exception boundary and
    " becomes CX_SY_NO_HANDLER. If that happens, skip checks and let the
    " subsequent deserialize step handle per-object failures.
    TRY.
        IF filter_param_available( ) = abap_true AND mo_obj_filter IS BOUND.
          CALL METHOD mo_repo->('DESERIALIZE_CHECKS')
            EXPORTING
              ii_obj_filter = mo_obj_filter
            RECEIVING
              rs_checks     = rs_checks.
        ELSE.
          rs_checks = mo_repo->deserialize_checks( ).
        ENDIF.
      CATCH cx_root.
        " Serializer error in get_files_local — skip checks, pull proceeds
        RETURN.
    ENDTRY.

    " Set transport request if provided
    IF iv_transport_request IS NOT INITIAL.
      rs_checks-transport-transport = iv_transport_request.
    ENDIF.

    " Set decision for each object in the overwrite table
    LOOP AT rs_checks-overwrite INTO DATA(ls_overwrite).
      IF lt_valid_files IS NOT INITIAL.
        READ TABLE lt_valid_files WITH TABLE KEY obj_type = ls_overwrite-obj_type
                                                 obj_name = ls_overwrite-obj_name
                                         TRANSPORTING NO FIELDS.
        ls_overwrite-decision = COND #( WHEN sy-subrc = 0
                                        THEN zif_abapgit_definitions=>c_yes
                                        ELSE zif_abapgit_definitions=>c_no ).
      ELSE.
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

        " Error/Abort messages - add to failed objects (warnings are not failures)
        IF ls_msg-type = 'E' OR ls_msg-type = 'A'.
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

  METHOD get_version.
    rv_version = '1.0.0'.
  ENDMETHOD.

  METHOD filter_param_available.
    " Check at runtime whether the installed abapGit version exposes
    " II_OBJ_FILTER on ZIF_ABAPGIT_REPO~DESERIALIZE.
    " Returns abap_true only when the parameter exists — safe for older installs.
    DATA lo_descr  TYPE REF TO cl_abap_objectdescr.
    DATA ls_method LIKE LINE OF lo_descr->methods.

    rv_available = abap_false.
    TRY.
        lo_descr = mo_rtti->describe_object( mo_repo ).
        READ TABLE lo_descr->methods INTO ls_method
          WITH KEY name = 'ZIF_ABAPGIT_REPO~DESERIALIZE'.
        IF sy-subrc = 0.
          READ TABLE ls_method-parameters WITH KEY name = 'II_OBJ_FILTER'
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            rv_available = abap_true.
          ELSE.
            rv_available = abap_false.
          ENDIF.
        ENDIF.
      CATCH cx_root.
        rv_available = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD paths_param_available.
    " Check at runtime whether the installed abapGit version supports the
    " IT_PATHS parameter on ZCL_ABAPGIT_OBJECT_FILTER_OBJ constructor.
    " Returns abap_true only when the parameter exists — safe for older installs.
    DATA lo_descr  TYPE REF TO cl_abap_classdescr.
    DATA ls_method LIKE LINE OF lo_descr->methods.

    rv_available = abap_false.
    TRY.
        lo_descr = mo_rtti->describe_class( 'ZCL_ABAPGIT_OBJECT_FILTER_OBJ' ).
        READ TABLE lo_descr->methods INTO ls_method
          WITH KEY name = 'CONSTRUCTOR'.
        IF sy-subrc = 0.
          READ TABLE ls_method-parameters WITH KEY name = 'IT_PATHS'
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            rv_available = abap_true.
          ELSE.
            rv_available = abap_false.
          ENDIF.
        ENDIF.
      CATCH cx_root.
        rv_available = abap_false.
    ENDTRY.
  ENDMETHOD.

  " ---------------------------------------------------------------------------
  " build_obj_filter
  " Builds mo_obj_filter from a list of file paths.  Extracted so the filter
  " can be constructed before build_file_entries_from_remote, allowing the
  " gitv2 two-phase fetch to be used during the conflict-detection fetch.
  " ---------------------------------------------------------------------------
  METHOD build_obj_filter.
    CLEAR mo_obj_filter.
    IF it_files IS SUPPLIED AND lines( it_files ) > 0.
      DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
      DATA lt_paths TYPE string_table.
      LOOP AT it_files INTO DATA(lv_file).
        DATA lv_obj_type TYPE string.
        DATA lv_obj_name TYPE string.
        zcl_abgagt_util=>get_instance( )->parse_file_to_object(
          EXPORTING iv_file     = lv_file
          IMPORTING ev_obj_type = lv_obj_type
                    ev_obj_name = lv_obj_name ).
        IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
          DATA ls_tadir LIKE LINE OF lt_tadir.
          ls_tadir-pgmid    = 'R3TR'.
          ls_tadir-object   = lv_obj_type.
          ls_tadir-obj_name = lv_obj_name.
          APPEND ls_tadir TO lt_tadir.
        ENDIF.

        " Extract directory path for gitv2 tree pruning
        " e.g. 'src/git/foo.clas.abap' -> '/src/git/'
        DATA lv_slash_pos TYPE i.
        DATA lv_path      TYPE string.
        DATA lv_len       TYPE i.
        lv_slash_pos = find( val = lv_file sub = '/' occ = -1 ).
        IF lv_slash_pos > 0.
          lv_len  = lv_slash_pos + 1.
          lv_path = '/' && lv_file(lv_len).
        ELSE.
          lv_path = '/'.
        ENDIF.
        READ TABLE lt_paths WITH KEY table_line = lv_path TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND lv_path TO lt_paths.
        ENDIF.
      ENDLOOP.
      IF lt_tadir IS NOT INITIAL.
        IF paths_param_available( ) = abap_true.
          DATA lt_param TYPE abap_parmbind_tab.
          DATA ls_param LIKE LINE OF lt_param.
          ls_param-name  = 'IT_FILTER'.
          ls_param-kind  = cl_abap_objectdescr=>exporting.
          GET REFERENCE OF lt_tadir INTO ls_param-value.
          INSERT ls_param INTO TABLE lt_param.
          ls_param-name  = 'IT_PATHS'.
          GET REFERENCE OF lt_paths INTO ls_param-value.
          INSERT ls_param INTO TABLE lt_param.
          CREATE OBJECT mo_obj_filter TYPE ('ZCL_ABAPGIT_OBJECT_FILTER_OBJ')
            PARAMETER-TABLE lt_param.
        ELSE.
          CREATE OBJECT mo_obj_filter TYPE zcl_abapgit_object_filter_obj
            EXPORTING it_filter = lt_tadir.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  " ---------------------------------------------------------------------------
  " build_file_entries_from_remote
  " Fetches remote git files and builds ty_file_entries for conflict detection.
  " Only .abap/.asddls files are included; filtered to it_files if provided.
  " ---------------------------------------------------------------------------
  METHOD resolve_repo.
    " Look up the repository by URL and assign it to mo_repo.
    " Returns abap_true if found, abap_false if not registered.
    rv_found = abap_false.
    IF mo_repo IS BOUND.
      rv_found = abap_true.
      RETURN.
    ENDIF.
    DATA li_repo TYPE REF TO zif_abapgit_repo.
    zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
      EXPORTING iv_url = iv_url
      IMPORTING ei_repo = li_repo ).
    IF li_repo IS BOUND.
      mo_repo  = li_repo.
      rv_found = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD select_branch.
    " Switch the repository to the requested branch (empty = keep current).
    " abapGit expects full git ref format: refs/heads/<name> or refs/tags/<name>.
    IF iv_branch IS INITIAL.
      RETURN.
    ENDIF.

    DATA lv_git_ref TYPE string.
    IF iv_branch CS 'refs/'.
      lv_git_ref = iv_branch.
    ELSEIF iv_branch(1) = 'v' AND iv_branch CN ' '.
      lv_git_ref = |refs/tags/{ iv_branch }|.
    ELSE.
      lv_git_ref = |refs/heads/{ iv_branch }|.
    ENDIF.

    TRY.
        DATA li_repo_online TYPE REF TO zif_abapgit_repo_online.
        li_repo_online ?= mo_repo.
        li_repo_online->select_branch( lv_git_ref ).
      CATCH cx_sy_move_cast_error.
        " Not an online repository — branch selection not applicable.
    ENDTRY.
  ENDMETHOD.

  METHOD run_conflict_check.
    " Run conflict detection and return results.
    " rv_aborted = abap_true  → caller must abort the pull.
    rv_aborted        = abap_false.
    ev_conflict_count = 0.

    et_file_entries = build_file_entries_from_remote( it_files ).

    IF iv_conflict_mode = 'ignore' OR et_file_entries IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_conflicts) = mo_conflict_detector->check_conflicts(
      it_files  = et_file_entries
      iv_branch = iv_branch ).

    ev_conflict_count  = lines( lt_conflicts ).
    ev_conflict_report = mo_conflict_detector->get_conflict_report( lt_conflicts ).

    IF ev_conflict_count > 0.
      rv_aborted = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD run_deserialize.
    " Execute abapGit deserialization with the prepared checks.
    mo_repo->create_new_log( ).
    IF filter_param_available( ) = abap_true AND mo_obj_filter IS BOUND.
      CALL METHOD mo_repo->('DESERIALIZE')
        EXPORTING
          is_checks     = is_checks
          ii_log        = mo_repo->get_log( )
          ii_obj_filter = mo_obj_filter.
    ELSE.
      mo_repo->deserialize(
        is_checks = is_checks
        ii_log    = mo_repo->get_log( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD build_pull_result.
    " Populate cs_result after a successful deserialization.
    DATA(ls_objects) = get_object_lists( ).

    cs_result-activated_objects = ls_objects-activated_objects.
    cs_result-failed_objects    = ls_objects-failed_objects.
    cs_result-log_messages      = ls_objects-log_messages.
    cs_result-activated_count   = lines( cs_result-activated_objects ).
    cs_result-failed_count      = lines( cs_result-failed_objects ).

    GET TIME STAMP FIELD cs_result-finished_at.

    IF check_log_for_errors( ) = abap_true.
      cs_result-message      = 'Pull completed with errors'.
      cs_result-error_detail = get_log_detail( ).
    ELSE.
      cs_result-success = abap_true.
      cs_result-message = 'Pull completed successfully'.

      cs_result-local_xml_files = get_local_xml_files( ).

      " Store baseline metadata for future conflict detection.
      IF it_file_entries IS NOT INITIAL.
        mo_conflict_detector->store_pull_metadata(
          it_files  = it_file_entries
          iv_branch = iv_branch ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD build_file_entries_from_remote.
    CLEAR mt_remote_files.

    TRY.
        " Pass the object filter when available so fetch_remote uses the gitv2
        " two-phase fetch (fast) instead of downloading the full pack.
        IF mo_obj_filter IS BOUND.
          mt_remote_files = mo_repo->get_files_remote( mo_obj_filter ).
        ELSE.
          mt_remote_files = mo_repo->get_files_remote( ).
        ENDIF.
      CATCH zcx_abapgit_exception INTO DATA(lx_remote).
        " Cannot read remote files — skip conflict check (e.g. auth error pre-pull)
        mv_remote_fetch_error = lx_remote->get_text( ).
        RETURN.
      CATCH cx_root INTO DATA(lx_remote_root).
        mv_remote_fetch_error = lx_remote_root->get_text( ).
        RETURN.
    ENDTRY.

    " Build lookup of requested files (filename only, uppercased)
    DATA lt_filter TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    IF it_files IS SUPPLIED AND lines( it_files ) > 0.
      LOOP AT it_files INTO DATA(lv_req_file).
        DATA lv_fn TYPE string.
        " Extract just the filename (strip path)
        DATA(lv_pos) = find( val = reverse( lv_req_file ) sub = '/' ).
        IF lv_pos > 0.
          DATA(lv_offs) = strlen( lv_req_file ) - lv_pos.
          lv_fn = lv_req_file+lv_offs.
        ELSE.
          lv_fn = lv_req_file.
        ENDIF.
        TRANSLATE lv_fn TO UPPER CASE.
        INSERT lv_fn INTO TABLE lt_filter.
      ENDLOOP.
    ENDIF.

    LOOP AT mt_remote_files INTO DATA(ls_remote_file).
      DATA(lv_filename) = ls_remote_file-filename.

      " Only process ABAP source files — check true suffix (last segment after '.')
      " Use CS check for speed, then verify no false positive from .abapgit.xml-type names
      DATA(lv_ext) = to_upper( lv_filename ).
      DATA(lv_ext_len) = strlen( lv_ext ).
      DATA(lv_last_dot) = find( val = reverse( lv_ext ) sub = '.' ).
      IF lv_last_dot <= 0.
        CONTINUE.
      ENDIF.
      DATA lv_suffix_off TYPE i.
      lv_suffix_off = lv_ext_len - lv_last_dot - 1.
      DATA(lv_true_ext) = lv_ext+lv_suffix_off.
      IF lv_true_ext <> '.ABAP' AND lv_true_ext <> '.ASDDLS'.
        CONTINUE.
      ENDIF.

      " Skip test class files — not standalone objects
      IF lv_ext CS '.TESTCLASSES.' OR lv_ext CS '.LOCALS_DEF.' OR lv_ext CS '.LOCALS_IMP.'.
        CONTINUE.
      ENDIF.

      " Filter to requested files if specified
      IF lines( lt_filter ) > 0.
        DATA(lv_upper_fn) = to_upper( lv_filename ).
        READ TABLE lt_filter WITH TABLE KEY table_line = lv_upper_fn TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      " Parse filename → obj_type + obj_name
      DATA lv_obj_type TYPE string.
      DATA lv_obj_name TYPE string.
      zcl_abgagt_util=>get_instance( )->parse_file_to_object(
        EXPORTING iv_file = lv_filename
        IMPORTING ev_obj_type = lv_obj_type
                  ev_obj_name = lv_obj_name ).

      IF lv_obj_type IS INITIAL OR lv_obj_name IS INITIAL.
        CONTINUE.
      ENDIF.

      " Convert xstring content to string (UTF-8)
      DATA lv_content TYPE string.
      TRY.
          lv_content = cl_abap_codepage=>convert_from(
            source   = ls_remote_file-data
            codepage = 'UTF-8' ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      DATA ls_entry TYPE zif_abgagt_conflict_detector=>ty_file_entry.
      ls_entry-obj_type = lv_obj_type.
      ls_entry-obj_name = lv_obj_name.
      ls_entry-content  = lv_content.
      APPEND ls_entry TO rt_entries.
    ENDLOOP.

    " Read local ABAP system files to enable content-based change detection.
    " A failure here must NOT wipe rt_entries — remote file data is already
    " collected and store_pull_metadata depends on it being returned.
    " On failure we simply skip populating local_content (no LOCAL_EDIT detection).
    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA lv_local_read_ok TYPE abap_bool VALUE abap_true.
    TRY.
        IF paths_param_available( ) = abap_true AND mo_obj_filter IS BOUND.
          lt_local = mo_repo->get_files_local_filtered( ii_obj_filter = mo_obj_filter ).
        ELSE.
          lt_local = mo_repo->get_files_local( ).
        ENDIF.
      CATCH zcx_abapgit_exception cx_root.
        " Cannot read local files — proceed without local_content (no LOCAL_EDIT detection).
        " rt_entries is already populated with remote data and must be returned as-is so
        " store_pull_metadata runs and ZABGAGT_OBJ_META (including DEVCLASS) stays up to date.
        lv_local_read_ok = abap_false.
    ENDTRY.

    CHECK lv_local_read_ok = abap_true.

    " Build lookup: obj_type + obj_name → local file content (main source only)
    DATA lt_local_idx TYPE HASHED TABLE OF zif_abgagt_conflict_detector=>ty_file_entry
                      WITH UNIQUE KEY obj_type obj_name.

    LOOP AT lt_local INTO DATA(ls_local_item).
      DATA(lv_local_fn) = to_upper( ls_local_item-file-filename ).
      DATA(lv_local_len) = strlen( lv_local_fn ).
      DATA(lv_local_dot) = find( val = reverse( lv_local_fn ) sub = '.' ).
      IF lv_local_dot <= 0.
        CONTINUE.
      ENDIF.
      DATA lv_local_off TYPE i.
      lv_local_off = lv_local_len - lv_local_dot - 1.
      DATA(lv_local_ext) = lv_local_fn+lv_local_off.
      IF lv_local_ext <> '.ABAP' AND lv_local_ext <> '.ASDDLS'.
        CONTINUE.
      ENDIF.
      IF lv_local_fn CS '.TESTCLASSES.' OR lv_local_fn CS '.LOCALS_DEF.' OR lv_local_fn CS '.LOCALS_IMP.'.
        CONTINUE.
      ENDIF.

      DATA lv_local_content TYPE string.
      TRY.
          lv_local_content = cl_abap_codepage=>convert_from(
            source   = ls_local_item-file-data
            codepage = 'UTF-8' ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      DATA ls_local_idx TYPE zif_abgagt_conflict_detector=>ty_file_entry.
      ls_local_idx-obj_type      = ls_local_item-item-obj_type.
      ls_local_idx-obj_name      = ls_local_item-item-obj_name.
      ls_local_idx-local_content = lv_local_content.
      INSERT ls_local_idx INTO TABLE lt_local_idx.
    ENDLOOP.

    " Populate local_content in rt_entries from lookup
    LOOP AT rt_entries ASSIGNING FIELD-SYMBOL(<ls_entry>).
      READ TABLE lt_local_idx WITH TABLE KEY
        obj_type = <ls_entry>-obj_type
        obj_name = <ls_entry>-obj_name
      INTO DATA(ls_idx).
      IF sy-subrc = 0.
        <ls_entry>-local_content = ls_idx-local_content.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_local_xml_files.
    " Compare post-pull serializer output against pre-pull git remote files.
    " mt_remote_files was fetched in build_file_entries_from_remote (before the
    " pull) — this is exactly the git content we want to compare against.
    " Calling get_files_remote() again after the pull would re-fetch from GitHub
    " (abapGit invalidates its mt_remote cache during deserialize), causing a
    " slow second network round-trip and often an auth exception.

    " Step 1: Build hashed lookup of pre-pull remote XML files.
    DATA lt_remote_idx TYPE HASHED TABLE OF zif_abapgit_git_definitions=>ty_file
      WITH UNIQUE KEY path filename.
    LOOP AT mt_remote_files INTO DATA(ls_remote).
      IF to_upper( ls_remote-filename ) NS '.XML'.
        CONTINUE.
      ENDIF.
      INSERT ls_remote INTO TABLE lt_remote_idx.
    ENDLOOP.

    IF lt_remote_idx IS INITIAL.
      RETURN.
    ENDIF.

    " Step 2: Fetch post-pull serializer output — filtered to pulled objects only.
    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_item_tt.
    TRY.
        IF paths_param_available( ) = abap_true AND mo_obj_filter IS BOUND.
          lt_local = mo_repo->get_files_local_filtered( ii_obj_filter = mo_obj_filter ).
        ELSE.
          lt_local = mo_repo->get_files_local( ).
        ENDIF.
      CATCH zcx_abapgit_exception cx_root.
        RETURN.
    ENDTRY.

    " Step 3: Compare — emit only files where bytes differ.
    LOOP AT lt_local INTO DATA(ls_item).
      IF to_upper( ls_item-file-filename ) NS '.XML'.
        CONTINUE.
      ENDIF.

      READ TABLE lt_remote_idx WITH TABLE KEY
        path     = ls_item-file-path
        filename = ls_item-file-filename
      INTO DATA(ls_remote_file).
      IF sy-subrc = 0 AND ls_item-file-data = ls_remote_file-data.
        " Bytes identical and file exists in git — nothing to sync.
        CONTINUE.
      ENDIF.

      DATA ls_xml TYPE zif_abgagt_agent=>ty_xml_file.
      ls_xml-filename = ls_item-file-filename.
      ls_xml-path     = ls_item-file-path.
      ls_xml-data     = cl_http_utility=>encode_x_base64( unencoded = ls_item-file-data ).
      APPEND ls_xml TO rt_xml_files.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
