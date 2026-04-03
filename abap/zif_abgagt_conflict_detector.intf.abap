"! <p class="shorttext synchronized">Conflict Detector Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_conflict_detector PUBLIC.

  TYPES: BEGIN OF ty_file_entry,
    obj_type      TYPE tadir-object,
    obj_name      TYPE tadir-obj_name,
    content       TYPE string,        " remote content (from git)
    local_content TYPE string,        " local content (from ABAP system)
  END OF ty_file_entry.

  TYPES ty_file_entries TYPE STANDARD TABLE OF ty_file_entry WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_conflict,
    obj_type       TYPE tadir-object,
    obj_name       TYPE tadir-obj_name,
    conflict_type  TYPE string,        " 'SYSTEM_EDIT', 'BRANCH_SWITCH', or 'LOCAL_EDIT'
    git_sha_old    TYPE string,
    git_sha_new    TYPE string,
    branch_old     TYPE string,
    branch_new     TYPE string,
    sys_changed_at TYPE timestampl,
    sys_changed_by TYPE syuname,
    last_pulled_at TYPE timestampl,
    last_pulled_by TYPE syuname,
  END OF ty_conflict.

  TYPES ty_conflicts TYPE STANDARD TABLE OF ty_conflict WITH NON-UNIQUE DEFAULT KEY.

  "! Detect conflicts before a pull
  "! Reads baseline from ZABGAGT_OBJ_META (doubled in tests via cl_osql_test_environment).
  "! @parameter it_files | Files to check against stored baseline
  "! @parameter iv_branch | Target branch name
  "! @parameter rt_conflicts | Conflicting objects; empty = safe to proceed
  METHODS check_conflicts
    IMPORTING
      it_files  TYPE ty_file_entries
      iv_branch TYPE string
    RETURNING
      VALUE(rt_conflicts) TYPE ty_conflicts.

  "! Store pull metadata baseline after a successful pull
  "! Writes to ZABGAGT_OBJ_META (doubled in tests via cl_osql_test_environment).
  "! @parameter it_files | Files that were pulled
  "! @parameter iv_branch | Branch that was pulled
  METHODS store_pull_metadata
    IMPORTING
      it_files  TYPE ty_file_entries
      iv_branch TYPE string.

  "! Build a human-readable conflict report string
  "! @parameter it_conflicts | List of detected conflicts
  "! @parameter rv_report | Formatted report text; initial when list is empty
  METHODS get_conflict_report
    IMPORTING
      it_conflicts TYPE ty_conflicts
    RETURNING
      VALUE(rv_report) TYPE string.

  "! Calculate a stable SHA-1 fingerprint for file content
  "! Public so tests can compute expected SHAs without hard-coding strings.
  "! @parameter iv_content | File content to hash
  "! @parameter rv_sha | SHA-1 hex string
  METHODS calculate_sha
    IMPORTING
      iv_content TYPE string
    RETURNING
      VALUE(rv_sha) TYPE string.

ENDINTERFACE.
