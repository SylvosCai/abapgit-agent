"! <p class="shorttext synchronized">Agent Interface for abapGit pull operations</p>
INTERFACE zif_abgagt_agent PUBLIC.

  TYPES: BEGIN OF ty_object,
    type TYPE string,
    id TYPE string,
    number TYPE string,
    text TYPE string,
    obj_type TYPE string,
    obj_name TYPE string,
    exception TYPE string,
  END OF ty_object.

  TYPES: ty_object_list TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_xml_file,
           filename TYPE string,
           path     TYPE string,
           data     TYPE string,
         END OF ty_xml_file.
  TYPES: ty_xml_files TYPE STANDARD TABLE OF ty_xml_file WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_result,
    success TYPE abap_bool,
    job_id TYPE string,
    message TYPE string,
    error_detail TYPE string,
    transport_request TYPE string,
    activated_count TYPE i,
    failed_count TYPE i,
    log_messages TYPE ty_object_list,
    activated_objects TYPE ty_object_list,
    failed_objects TYPE ty_object_list,
    started_at TYPE timestampl,
    finished_at TYPE timestampl,
    conflict_report TYPE string,
    conflict_count TYPE i,
    local_xml_files TYPE ty_xml_files,
  END OF ty_result.

  TYPES: BEGIN OF ty_drop_result,
    success  TYPE abap_bool,
    obj_type TYPE string,
    obj_name TYPE string,
    message  TYPE string,
    error    TYPE string,
  END OF ty_drop_result.

  "! Pull objects from remote git repository and activate them
  "! @parameter iv_url | Git repository URL
  "! @parameter iv_branch | Branch to pull (default: main)
  "! @parameter iv_username | Git credentials username (optional)
  "! @parameter iv_password | Git credentials password or token (optional)
  "! @parameter iv_transport_request | Transport request number (optional)
  "! @parameter it_files | Restrict to these file paths (optional; all objects if empty)
  "! @parameter iv_conflict_mode | Conflict handling: 'abort' or 'ignore'
  "! @parameter rs_result | Pull result with activation counts and log
  "! @raising zcx_abapgit_exception | On pull or activation failure
  METHODS pull
    IMPORTING
      iv_url TYPE string
      iv_branch TYPE string DEFAULT 'main'
      iv_username TYPE string OPTIONAL
      iv_password TYPE string OPTIONAL
      iv_transport_request TYPE string OPTIONAL
      it_files TYPE string_table OPTIONAL
      iv_conflict_mode TYPE string DEFAULT 'abort'
    RETURNING
      VALUE(rs_result) TYPE ty_result
    RAISING
      zcx_abapgit_exception.

  "! Get the current status of a registered repository
  "! @parameter iv_url | Git repository URL
  "! @parameter rv_status | Status string (e.g., 'ACTIVE', 'NOT_FOUND')
  METHODS get_repo_status
    IMPORTING
      iv_url TYPE string
    RETURNING
      VALUE(rv_status) TYPE string.

  "! Drop (physically delete) a single ABAP object from the system
  "! @parameter iv_file               | File path (e.g., 'src/zcl_foo.clas.abap')
  "! @parameter iv_transport_request  | Transport request (optional)
  "! @parameter rs_result             | Drop result with success flag and details
  METHODS drop
    IMPORTING
      iv_file              TYPE string
      iv_transport_request TYPE string OPTIONAL
    RETURNING
      VALUE(rs_result) TYPE ty_drop_result.

ENDINTERFACE.
