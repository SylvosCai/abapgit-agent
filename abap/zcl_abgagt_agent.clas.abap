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
        io_conflict_detector TYPE REF TO zif_abgagt_conflict_detector OPTIONAL.

    METHODS: get_version RETURNING VALUE(rv_version) TYPE string.
    METHODS: parse_file_to_object
      IMPORTING
        iv_file TYPE string
      EXPORTING
        ev_obj_type TYPE string
        ev_obj_name TYPE string.

  PROTECTED SECTION.

    " Local type for item signature (matches abapGit structure)
    TYPES: BEGIN OF ty_item_signature,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE devclass,
           END OF ty_item_signature.

    DATA: mo_repo              TYPE REF TO zif_abapgit_repo.
    DATA: mo_conflict_detector TYPE REF TO zif_abgagt_conflict_detector.

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
        ii_obj_filter        TYPE REF TO zif_abapgit_object_filter OPTIONAL
        io_repo_desc         TYPE REF TO cl_abap_objectdescr OPTIONAL
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING zcx_abapgit_exception.

    METHODS check_log_for_errors
      RETURNING
        VALUE(rv_has_error) TYPE abap_bool.

    METHODS build_object_filter
      IMPORTING
        it_files          TYPE string_table OPTIONAL
      RETURNING
        VALUE(ro_filter)  TYPE REF TO zif_abapgit_object_filter
      RAISING
        zcx_abapgit_exception.

    METHODS build_file_entries_from_remote
      IMPORTING
        it_files               TYPE string_table OPTIONAL
        ii_obj_filter          TYPE REF TO zif_abapgit_object_filter OPTIONAL
        iv_has_local_filtered  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_entries) TYPE zif_abgagt_conflict_detector=>ty_file_entries
      RAISING
        zcx_abapgit_exception.

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
  ENDMETHOD.

  METHOD zif_abgagt_agent~pull.
    DATA: lv_job_id TYPE string.
    lv_job_id = |{ sy-uname }{ sy-datum }{ sy-uzeit }|.
    rs_result-job_id = lv_job_id.
    rs_result-success = abap_false.
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

        DATA: li_repo TYPE REF TO zif_abapgit_repo.
        IF mo_repo IS NOT BOUND.
          zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
            EXPORTING iv_url = iv_url
            IMPORTING ei_repo = li_repo ).
          mo_repo = li_repo.
        ENDIF.

        IF mo_repo IS BOUND.

          " Switch branch/tag if specified and it's an online repository
          IF iv_branch IS NOT INITIAL.
            DATA: li_repo_online TYPE REF TO zif_abapgit_repo_online.
            DATA: lv_git_ref TYPE string.

            " Convert short branch/tag name to full git ref format
            " abapGit expects: refs/heads/master or refs/tags/v1.0.0
            IF iv_branch CS 'refs/'.
              " Already a full ref
              lv_git_ref = iv_branch.
            ELSEIF iv_branch(1) = 'v' AND iv_branch CN ' '.
              " Looks like a tag (starts with 'v' and no spaces)
              lv_git_ref = |refs/tags/{ iv_branch }|.
            ELSE.
              " Assume it's a branch
              lv_git_ref = |refs/heads/{ iv_branch }|.
            ENDIF.

            TRY.
                li_repo_online ?= mo_repo.
                li_repo_online->select_branch( lv_git_ref ).
              CATCH cx_sy_move_cast_error.
                " Not an online repository, skip branch selection
            ENDTRY.
          ENDIF.

          " Build filter once — used for both remote fetch and deserialize
          DATA lo_obj_filter TYPE REF TO zif_abapgit_object_filter.
          lo_obj_filter = build_object_filter( it_files ).

          " Check once (RTTI) whether the repo supports get_files_local_filtered.
          " Doing this here avoids a second expensive RTTI lookup inside build_file_entries_from_remote.
          DATA(lo_repo_desc1)       = CAST cl_abap_objectdescr(
                                        cl_abap_typedescr=>describe_by_object_ref( mo_repo ) ).
          DATA(lv_has_local_filtered) = xsdbool(
                                          line_exists( lo_repo_desc1->methods[
                                            name = 'GET_FILES_LOCAL_FILTERED' ] )
                                          AND lo_obj_filter IS BOUND ).

          " --- Conflict detection (before deserialize) ---
          " Build remote file entries and run conflict check before any activation.
          " On success, the same entries are reused for store_pull_metadata after deserialize.
          DATA(lt_file_entries) = build_file_entries_from_remote(
            it_files              = it_files
            ii_obj_filter         = lo_obj_filter
            iv_has_local_filtered = lv_has_local_filtered ).

          IF iv_conflict_mode <> 'ignore' AND lt_file_entries IS NOT INITIAL.
            DATA(lt_conflicts) = mo_conflict_detector->check_conflicts(
              it_files  = lt_file_entries
              iv_branch = iv_branch ).

            IF lt_conflicts IS NOT INITIAL.
              rs_result-conflict_count  = lines( lt_conflicts ).
              rs_result-conflict_report = mo_conflict_detector->get_conflict_report( lt_conflicts ).
              rs_result-message = |Pull aborted — { rs_result-conflict_count } conflict(s) detected|.
              GET TIME STAMP FIELD rs_result-finished_at.
              RETURN.
            ENDIF.
          ENDIF.

          DATA(ls_checks) = prepare_deserialize_checks(
            it_files             = it_files
            iv_transport_request = iv_transport_request
            ii_obj_filter        = lo_obj_filter
            io_repo_desc         = lo_repo_desc1 ).

          mo_repo->create_new_log( ).

          DATA(lo_repo_desc2) = lo_repo_desc1.
          " RTTI: check on the interface-prefixed name, not the alias.
          " Aliases (e.g. DESERIALIZE for zif_abapgit_repo~deserialize) appear in the
          " methods table but their -parameters sub-table is empty in RTTI — only the
          " interface-prefixed entry (ZIF_ABAPGIT_REPO~DESERIALIZE) carries the full
          " parameter list.  Checking the alias name always yields lv_deser_has_filter
          " = false, causing the unfiltered fallback to activate the entire repository.
          DATA(lv_deser_has_filter) = xsdbool(
                                        line_exists( lo_repo_desc2->methods[
                                          name = 'ZIF_ABAPGIT_REPO~DESERIALIZE' ] )
                                        AND line_exists(
                                          lo_repo_desc2->methods[
                                            name = 'ZIF_ABAPGIT_REPO~DESERIALIZE' ]-parameters[
                                            name = 'II_OBJ_FILTER' ] ) ).

          IF lv_deser_has_filter = abap_true.
            DATA(lo_log_ref) = mo_repo->get_log( ).
            DATA(lt_ptab_deser) = VALUE abap_parmbind_tab(
              ( name  = 'IS_CHECKS'
                kind  = cl_abap_objectdescr=>exporting
                value = REF #( ls_checks ) )
              ( name  = 'II_LOG'
                kind  = cl_abap_objectdescr=>exporting
                value = REF #( lo_log_ref ) )
              ( name  = 'II_OBJ_FILTER'
                kind  = cl_abap_objectdescr=>exporting
                value = REF #( lo_obj_filter ) ) ).
            CALL METHOD mo_repo->('DESERIALIZE') PARAMETER-TABLE lt_ptab_deser.
          ELSE.
            mo_repo->deserialize(
              is_checks = ls_checks
              ii_log    = mo_repo->get_log( ) ).
          ENDIF.

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

            " --- Post-pull conflict metadata (lt_file_entries built before deserialize) ---
            mo_conflict_detector->store_pull_metadata(
              it_files  = lt_file_entries
              iv_branch = iv_branch ).
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

  METHOD parse_file_to_object.
    " Parse file path to extract obj_type and obj_name
    " Example: "zcl_my_class.clas.abap" -> CLAS, ZCL_MY_CLASS
    " Example: "src/zcl_my_class.clas.abap" -> CLAS, ZCL_MY_CLASS
    " Example: "zc_my_view.ddls.asddls" -> DDLS, ZC_MY_VIEW

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

    " Check file extension - support both .abap and .asddls
    READ TABLE lt_parts INDEX lv_part_count INTO DATA(lv_last).
    IF lv_last <> 'ABAP' AND lv_last <> 'ASDDLS'.
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

  METHOD build_object_filter.

    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

    IF it_files IS SUPPLIED AND lines( it_files ) > 0.
      LOOP AT it_files INTO DATA(lv_file).
        DATA lv_obj_type TYPE string.
        DATA lv_obj_name TYPE string.
        parse_file_to_object(
          EXPORTING iv_file = lv_file
          IMPORTING ev_obj_type = lv_obj_type
                    ev_obj_name = lv_obj_name ).
        IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
          DATA ls_tadir TYPE zif_abapgit_definitions=>ty_tadir.
          ls_tadir-object   = lv_obj_type.
          ls_tadir-obj_name = lv_obj_name.
          APPEND ls_tadir TO lt_tadir.
        ENDIF.
      ENDLOOP.

      IF lt_tadir IS NOT INITIAL.
        CREATE OBJECT ro_filter TYPE zcl_abapgit_object_filter_obj
          EXPORTING it_filter = lt_tadir.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD prepare_deserialize_checks.

    " Build lookup table for DECISION loop from the filter
    DATA lt_valid_files TYPE HASHED TABLE OF zif_abapgit_definitions=>ty_item_signature
                            WITH UNIQUE KEY obj_type obj_name.

    IF ii_obj_filter IS BOUND.
      LOOP AT ii_obj_filter->get_filter( ) INTO DATA(ls_tadir).
        DATA ls_sig TYPE zif_abapgit_definitions=>ty_item_signature.
        ls_sig-obj_type = ls_tadir-object.
        ls_sig-obj_name = ls_tadir-obj_name.
        INSERT ls_sig INTO TABLE lt_valid_files.
      ENDLOOP.
    ENDIF.

    " Use filtered call when filter is set — only processes requested objects.
    " Older abapGit versions lack ii_obj_filter; detect at runtime via RTTI.
    DATA(lo_obj_desc) = COND #(
                          WHEN io_repo_desc IS BOUND THEN io_repo_desc
                          ELSE CAST cl_abap_objectdescr(
                                 cl_abap_typedescr=>describe_by_object_ref( mo_repo ) ) ).
    DATA(lt_methods)  = lo_obj_desc->methods.
    " Same RTTI caveat as for DESERIALIZE: check the interface-prefixed name.
    DATA(lv_has_filter) = xsdbool(
                            line_exists( lt_methods[
                              name = 'ZIF_ABAPGIT_REPO~DESERIALIZE_CHECKS' ] )
                            AND line_exists(
                              lt_methods[
                                name = 'ZIF_ABAPGIT_REPO~DESERIALIZE_CHECKS' ]-parameters[
                                name = 'II_OBJ_FILTER' ] ) ).

    IF lv_has_filter = abap_true.
      DATA ls_checks_ret TYPE zif_abapgit_definitions=>ty_deserialize_checks.
      DATA(lt_ptab_checks) = VALUE abap_parmbind_tab(
        ( name  = 'II_OBJ_FILTER'
          kind  = cl_abap_objectdescr=>exporting
          value = REF #( ii_obj_filter ) )
        ( name  = 'RS_CHECKS'
          kind  = cl_abap_objectdescr=>returning
          value = REF #( ls_checks_ret ) ) ).
      CALL METHOD mo_repo->('DESERIALIZE_CHECKS') PARAMETER-TABLE lt_ptab_checks.
      rs_checks = ls_checks_ret.
    ELSE.
      rs_checks = mo_repo->deserialize_checks( ).
    ENDIF.

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

  " ---------------------------------------------------------------------------
  " build_file_entries_from_remote
  " Fetches remote git files and builds ty_file_entries for conflict detection.
  " Only .abap/.asddls files are included; filtered to it_files if provided.
  " ---------------------------------------------------------------------------
  METHOD build_file_entries_from_remote.
    DATA lt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt.

    TRY.
        lt_remote = mo_repo->get_files_remote( ii_obj_filter = ii_obj_filter ).
      CATCH zcx_abapgit_exception.
        " Cannot read remote files — skip conflict check
        RETURN.
    ENDTRY.

    " Build lookup of requested files (obj_type + obj_name)
    DATA lt_filter TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    IF it_files IS SUPPLIED AND lines( it_files ) > 0.
      LOOP AT it_files INTO DATA(lv_req_file).
        DATA lv_fn TYPE string.
        DATA lv_ot TYPE string.
        DATA lv_on TYPE string.
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

    LOOP AT lt_remote INTO DATA(ls_remote_file).
      DATA(lv_filename) = ls_remote_file-filename.

      " Only process ABAP source files
      DATA(lv_ext) = to_upper( lv_filename ).
      IF NOT ( lv_ext CS '.ABAP' OR lv_ext CS '.ASDDLS' ).
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
      parse_file_to_object(
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
    " This replaces VRSD timestamp approach (which does not work for local packages).
    " Use filtered variant when the caller confirmed it exists and a filter is provided.
    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_item_tt.
    TRY.
        IF iv_has_local_filtered = abap_true.
          DATA(lt_ptab_local) = VALUE abap_parmbind_tab(
            ( name  = 'II_OBJ_FILTER'
              kind  = cl_abap_objectdescr=>exporting
              value = REF #( ii_obj_filter ) )
            ( name  = 'RT_FILES'
              kind  = cl_abap_objectdescr=>returning
              value = REF #( lt_local ) ) ).
          CALL METHOD mo_repo->('GET_FILES_LOCAL_FILTERED') PARAMETER-TABLE lt_ptab_local.
        ELSE.
          lt_local = mo_repo->get_files_local( ).
        ENDIF.
      CATCH zcx_abapgit_exception.
        " Cannot read local files — proceed without local_content (no LOCAL_EDIT detection)
        RETURN.
    ENDTRY.

    " Build lookup: obj_type + obj_name → local file content (main source only)
    DATA lt_local_idx TYPE HASHED TABLE OF zif_abgagt_conflict_detector=>ty_file_entry
                      WITH UNIQUE KEY obj_type obj_name.

    LOOP AT lt_local INTO DATA(ls_local_item).
      DATA(lv_local_fn) = to_upper( ls_local_item-file-filename ).
      IF NOT ( lv_local_fn CS '.ABAP' OR lv_local_fn CS '.ASDDLS' ).
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
      ls_local_idx-obj_type     = ls_local_item-item-obj_type.
      ls_local_idx-obj_name     = ls_local_item-item-obj_name.
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

ENDCLASS.
