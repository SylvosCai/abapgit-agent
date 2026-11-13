INTERFACE zif_abgagt_source_file_policy PUBLIC.
  METHODS accepts_source_extension
    IMPORTING iv_extension TYPE string
    RETURNING VALUE(rv_accepted) TYPE abap_bool.
  METHODS validate_remote
    IMPORTING it_files TYPE zif_abapgit_git_definitions=>ty_files_tt
    RAISING zcx_abapgit_exception.
  METHODS validate_local
    IMPORTING
      it_files   TYPE zif_abapgit_definitions=>ty_files_item_tt
      it_objects TYPE zif_abgagt_conflict_detector=>ty_file_entries
    RAISING zcx_abapgit_exception.
  METHODS handle_local_read_failure
    IMPORTING it_remote TYPE zif_abapgit_git_definitions=>ty_files_tt
    RAISING zcx_abapgit_exception.
  METHODS handle_decode_failure
    IMPORTING iv_type TYPE string iv_filename TYPE string ix_previous TYPE REF TO cx_root
    RAISING zcx_abapgit_exception.
ENDINTERFACE.
