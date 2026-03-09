CLASS zcl_abgagt_conflict_detector DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_conflict_detector.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zif_abgagt_conflict_detector.

ENDCLASS.

CLASS zcl_abgagt_conflict_detector IMPLEMENTATION.

  METHOD get_instance.
    DATA lo_instance TYPE REF TO zcl_abgagt_conflict_detector.
    CREATE OBJECT lo_instance.
    ro_instance = lo_instance.
  ENDMETHOD.

  " ---------------------------------------------------------------------------
  " check_conflicts
  " ---------------------------------------------------------------------------
  METHOD zif_abgagt_conflict_detector~check_conflicts.
    LOOP AT it_files INTO DATA(ls_file).

      " Load baseline from ZABGAGT_OBJ_META
      SELECT SINGLE last_git_sha, last_branch, last_pulled_at, last_pulled_by,
                    sys_changed_at, sys_changed_by
        FROM zabgagt_obj_meta
        WHERE obj_type = @ls_file-obj_type
          AND obj_name = @ls_file-obj_name
        INTO @DATA(ls_baseline).

      IF sy-subrc <> 0.
        " No baseline → first pull, skip conflict check
        CONTINUE.
      ENDIF.

      " Calculate current git SHA from remote file content
      DATA(lv_current_sha) = zif_abgagt_conflict_detector~calculate_sha( ls_file-content ).

      " Determine whether the local ABAP system has been modified since last pull.
      " Compare local content SHA against the stored baseline git SHA.
      " If local_content is not provided (old callers), default to unchanged (safe).
      DATA lv_local_sha TYPE string.
      IF ls_file-local_content IS NOT INITIAL.
        lv_local_sha = zif_abgagt_conflict_detector~calculate_sha( ls_file-local_content ).
      ELSE.
        lv_local_sha = ls_baseline-last_git_sha.
      ENDIF.

      " Determine what changed
      DATA(lv_git_changed)     = xsdbool( lv_current_sha <> ls_baseline-last_git_sha ).
      DATA(lv_sys_changed)     = xsdbool( lv_local_sha <> ls_baseline-last_git_sha ).
      DATA(lv_branch_switched) = xsdbool( iv_branch <> ls_baseline-last_branch ).

      " Type 1: git changed AND system changed → SYSTEM_EDIT
      IF lv_git_changed = abap_true AND lv_sys_changed = abap_true.
        DATA ls_conflict TYPE zif_abgagt_conflict_detector=>ty_conflict.
        GET TIME STAMP FIELD ls_conflict-sys_changed_at.
        ls_conflict-obj_type       = ls_file-obj_type.
        ls_conflict-obj_name       = ls_file-obj_name.
        ls_conflict-conflict_type  = 'SYSTEM_EDIT'.
        ls_conflict-git_sha_old    = ls_baseline-last_git_sha.
        ls_conflict-git_sha_new    = lv_current_sha.
        ls_conflict-sys_changed_by = ls_baseline-sys_changed_by.
        ls_conflict-last_pulled_at = ls_baseline-last_pulled_at.
        ls_conflict-last_pulled_by = ls_baseline-last_pulled_by.
        APPEND ls_conflict TO rt_conflicts.
        CONTINUE.
      ENDIF.

      " Type 2: git changed AND branch switched → BRANCH_SWITCH
      IF lv_git_changed = abap_true AND lv_branch_switched = abap_true.
        CLEAR ls_conflict.
        ls_conflict-obj_type       = ls_file-obj_type.
        ls_conflict-obj_name       = ls_file-obj_name.
        ls_conflict-conflict_type  = 'BRANCH_SWITCH'.
        ls_conflict-git_sha_old    = ls_baseline-last_git_sha.
        ls_conflict-git_sha_new    = lv_current_sha.
        ls_conflict-branch_old     = ls_baseline-last_branch.
        ls_conflict-branch_new     = iv_branch.
        ls_conflict-last_pulled_at = ls_baseline-last_pulled_at.
        ls_conflict-last_pulled_by = ls_baseline-last_pulled_by.
        APPEND ls_conflict TO rt_conflicts.
        CONTINUE.
      ENDIF.

      " Type 3: system changed, git unchanged → LOCAL_EDIT
      " Pull would silently overwrite local ADT edits — must abort.
      IF lv_git_changed = abap_false AND lv_sys_changed = abap_true.
        CLEAR ls_conflict.
        GET TIME STAMP FIELD ls_conflict-sys_changed_at.
        ls_conflict-obj_type       = ls_file-obj_type.
        ls_conflict-obj_name       = ls_file-obj_name.
        ls_conflict-conflict_type  = 'LOCAL_EDIT'.
        ls_conflict-git_sha_old    = ls_baseline-last_git_sha.
        ls_conflict-sys_changed_by = ls_baseline-sys_changed_by.
        ls_conflict-last_pulled_at = ls_baseline-last_pulled_at.
        ls_conflict-last_pulled_by = ls_baseline-last_pulled_by.
        APPEND ls_conflict TO rt_conflicts.
        CONTINUE.
      ENDIF.

      " All other combinations are safe (fast-forward, no changes, etc.)
    ENDLOOP.
  ENDMETHOD.

  " ---------------------------------------------------------------------------
  " store_pull_metadata  (writes to ZABGAGT_OBJ_META)
  " ---------------------------------------------------------------------------
  METHOD zif_abgagt_conflict_detector~store_pull_metadata.
    DATA lv_now TYPE timestampl.
    GET TIME STAMP FIELD lv_now.

    " Bulk-fetch DEVCLASS from TADIR for all files in one round-trip.
    " DEVCLASS is not part of the primary key, so every MODIFY will naturally
    " overwrite it with the current TADIR value (handles package moves).
    " PGMID = 'R3TR' is required: TADIR primary key is PGMID+OBJECT+OBJ_NAME,
    " omitting it causes the DB optimizer to match unexpected rows.
    TYPES: BEGIN OF ty_tadir_entry,
             object   TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE tadir-devclass,
           END OF ty_tadir_entry.
    DATA lt_tadir TYPE HASHED TABLE OF ty_tadir_entry
                  WITH UNIQUE KEY object obj_name.
    IF it_files IS NOT INITIAL.
      SELECT object, obj_name, devclass
        FROM tadir
        FOR ALL ENTRIES IN @it_files
        WHERE pgmid    = 'R3TR'
          AND object   = @it_files-obj_type
          AND obj_name = @it_files-obj_name
        INTO CORRESPONDING FIELDS OF TABLE @lt_tadir.
    ENDIF.

    LOOP AT it_files INTO DATA(ls_file).
      DATA(lv_sha) = zif_abgagt_conflict_detector~calculate_sha( ls_file-content ).

      DATA ls_row TYPE zabgagt_obj_meta.
      ls_row-obj_type       = ls_file-obj_type.
      ls_row-obj_name       = ls_file-obj_name.
      ls_row-last_git_sha   = lv_sha.
      ls_row-last_branch    = iv_branch.
      ls_row-last_pulled_at = lv_now.
      ls_row-last_pulled_by = sy-uname.
      ls_row-sys_changed_at = lv_now.
      ls_row-sys_changed_by = sy-uname.

      " Populate DEVCLASS from TADIR lookup (O(1) via hashed table)
      DATA(ls_tadir) = VALUE #( lt_tadir[ object   = ls_file-obj_type
                                          obj_name = ls_file-obj_name ] OPTIONAL ).
      ls_row-devclass = ls_tadir-devclass.

      MODIFY zabgagt_obj_meta FROM @ls_row.
    ENDLOOP.
  ENDMETHOD.

  " ---------------------------------------------------------------------------
  " get_conflict_report
  " ---------------------------------------------------------------------------
  METHOD zif_abgagt_conflict_detector~get_conflict_report.
    CHECK it_conflicts IS NOT INITIAL.

    DATA(lv_count) = lines( it_conflicts ).
    rv_report = |Pull aborted — { lv_count } conflict(s) detected\n\n|.

    LOOP AT it_conflicts INTO DATA(ls_conflict).
      rv_report = rv_report &&
        |Object:        { ls_conflict-obj_type } { ls_conflict-obj_name }\n| &&
        |Conflict type: { ls_conflict-conflict_type }\n|.

      CASE ls_conflict-conflict_type.
        WHEN 'SYSTEM_EDIT'.
          rv_report = rv_report &&
            |Git changed:   { ls_conflict-git_sha_old } → { ls_conflict-git_sha_new }\n| &&
            |System changed by { ls_conflict-sys_changed_by }| &&
            | (after last pull by { ls_conflict-last_pulled_by })\n|.

        WHEN 'BRANCH_SWITCH'.
          rv_report = rv_report &&
            |Branch:        { ls_conflict-branch_old } → { ls_conflict-branch_new }\n| &&
            |Git changed:   { ls_conflict-git_sha_old } → { ls_conflict-git_sha_new }\n| &&
            |Last pull by { ls_conflict-last_pulled_by }\n|.

        WHEN 'LOCAL_EDIT'.
          rv_report = rv_report &&
            |System modified after last pull by { ls_conflict-last_pulled_by }\n| &&
            |Git content unchanged — pull would overwrite local edits\n|.
      ENDCASE.

      rv_report = rv_report && |\n|.
    ENDLOOP.

    rv_report = rv_report &&
      |To override: abapgit-agent pull --conflict-mode ignore\n|.
  ENDMETHOD.

  " ---------------------------------------------------------------------------
  " calculate_sha  (SHA-1 via cl_abap_hmac)
  " ---------------------------------------------------------------------------
  METHOD zif_abgagt_conflict_detector~calculate_sha.
    DATA lv_hash TYPE string.

    TRY.
        DATA lv_key TYPE xstring.
        lv_key = cl_abap_codepage=>convert_to( source   = 'abapgit-agent'
                                               codepage = '4110' ).
        cl_abap_hmac=>calculate_hmac_for_char(
          EXPORTING
            if_algorithm  = 'SHA1'
            if_key        = lv_key
            if_data       = iv_content
          IMPORTING
            ef_hmacstring = lv_hash ).
        rv_sha = lv_hash.
      CATCH cx_root.
        " Fallback: simple length+content hash for environments without SHA1
        rv_sha = |sha_{ strlen( iv_content ) }_{ iv_content }|.
        IF strlen( rv_sha ) > 40.
          rv_sha = rv_sha(40).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
