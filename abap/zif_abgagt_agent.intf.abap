" TODO: Implement detailed syntax error parsing
" When a syntax error occurs, the log shows the affected object name
" but not the specific line/column. For better error reporting:
" - Parse the error message to extract object info
" - For syntax errors, query SEPSA or TRINT_OBJECT_LOG for details
" - Return structured error with line numbers and fix suggestions

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

  TYPES: BEGIN OF ty_result,
    success TYPE abap_bool,
    job_id TYPE string,
    message TYPE string,
    error_detail TYPE string,
    activated_count TYPE i,
    failed_count TYPE i,
    log_messages TYPE ty_object_list,
    activated_objects TYPE ty_object_list,
    failed_objects TYPE ty_object_list,
    started_at TYPE timestampl,
    finished_at TYPE timestampl,
  END OF ty_result.

  TYPES: BEGIN OF ty_pull_params,
    url TYPE string,
    branch TYPE string,
    username TYPE string,
    password TYPE string,
    package TYPE devclass,
    folder_logic TYPE string,
    create_new TYPE abap_bool,
  END OF ty_pull_params.

  " Types for inspect (syntax check)
  TYPES: BEGIN OF ty_error,
           line TYPE string,
           column TYPE string,
           text TYPE string,
           word TYPE string,
         END OF ty_error.

  TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_inspect_result,
           success TYPE abap_bool,
           object_type TYPE string,
           object_name TYPE string,
           error_count TYPE i,
           errors TYPE ty_errors,
         END OF ty_inspect_result.

  " Types for unit tests
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

  TYPES: BEGIN OF ty_object_key,
           object_type TYPE string,
           object_name TYPE string,
         END OF ty_object_key.

  TYPES ty_object_keys TYPE STANDARD TABLE OF ty_object_key WITH NON-UNIQUE DEFAULT KEY.

  TYPES: ty_log_table TYPE TABLE OF string.
  TYPES: ty_job_status TYPE string.

  CONSTANTS:
    gc_status_pending TYPE ty_job_status VALUE 'PENDING',
    gc_status_running TYPE ty_job_status VALUE 'RUNNING',
    gc_status_completed TYPE ty_job_status VALUE 'COMPLETED',
    gc_status_failed TYPE ty_job_status VALUE 'FAILED'.

  METHODS pull
    IMPORTING
      iv_url TYPE string
      iv_branch TYPE string DEFAULT 'main'
      iv_username TYPE string OPTIONAL
      iv_password TYPE string OPTIONAL
      it_files TYPE string_table OPTIONAL
    RETURNING
      VALUE(rs_result) TYPE ty_result
    RAISING
      zcx_abapgit_exception.

  METHODS get_repo_status
    IMPORTING
      iv_url TYPE string
    RETURNING
      VALUE(rv_status) TYPE string.

  METHODS inspect
    IMPORTING
      iv_object_type TYPE string
      iv_object_name TYPE string
    RETURNING
      VALUE(rs_result) TYPE ty_inspect_result.

  METHODS run_tests
    IMPORTING
      iv_package TYPE devclass OPTIONAL
      it_objects TYPE ty_object_keys OPTIONAL
    RETURNING
      VALUE(rs_result) TYPE ty_unit_result.

  METHODS parse_file_to_object
    IMPORTING
      iv_file TYPE string
    EXPORTING
      ev_obj_type TYPE string
      ev_obj_name TYPE string.

ENDINTERFACE.
