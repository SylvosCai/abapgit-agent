CLASS zcl_abgagt_source_file_policy DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_source_file_policy.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_source_count,
             obj_type TYPE string,
             obj_name TYPE string,
             sources  TYPE i,
           END OF ty_source_count,
           ty_source_counts TYPE HASHED TABLE OF ty_source_count
             WITH UNIQUE KEY obj_type obj_name.
    METHODS validate_names
      IMPORTING it_names TYPE string_table iv_side TYPE string
      RAISING zcx_abapgit_exception.
    METHODS is_candidate
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rv_candidate) TYPE abap_bool.
    METHODS parse_strict_name
      IMPORTING iv_name TYPE string
      EXPORTING ev_type TYPE string ev_name TYPE string ev_native TYPE abap_bool
      RAISING zcx_abapgit_exception.
    METHODS is_strict_type
      IMPORTING iv_type TYPE string
      RETURNING VALUE(rv_strict) TYPE abap_bool.
    METHODS has_strict_remote
      IMPORTING it_files TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING VALUE(rv_has_strict) TYPE abap_bool.
ENDCLASS.

CLASS zcl_abgagt_source_file_policy IMPLEMENTATION.
  METHOD zif_abgagt_source_file_policy~accepts_source_extension.
    rv_accepted = xsdbool(
      iv_extension = '.ABAP' OR iv_extension = '.ASDDLS' OR iv_extension = '.XML'
      OR iv_extension = '.ASBDEF' OR iv_extension = '.SRVDSRV'
      OR iv_extension = '.ASSRVD' OR iv_extension = '.ASDDLXS' ).
  ENDMETHOD.

  METHOD zif_abgagt_source_file_policy~validate_remote.
    validate_names(
      it_names = VALUE #( FOR ls_file IN it_files ( ls_file-filename ) )
      iv_side  = 'Remote' ).
  ENDMETHOD.

  METHOD zif_abgagt_source_file_policy~validate_local.
    DATA lt_names TYPE string_table.
    LOOP AT it_files INTO DATA(ls_file).
      IF line_exists( it_objects[
           obj_type = ls_file-item-obj_type obj_name = ls_file-item-obj_name ] ).
        APPEND ls_file-file-filename TO lt_names.
      ENDIF.
    ENDLOOP.
    validate_names( it_names = lt_names iv_side = 'Local' ).
  ENDMETHOD.

  METHOD zif_abgagt_source_file_policy~handle_local_read_failure.
    IF has_strict_remote( it_remote ) = abap_true.
      zcx_abapgit_exception=>raise(
        iv_text = 'Cannot read required local sources; pull stopped before deserialization' ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_source_file_policy~handle_decode_failure.
    IF is_strict_type( iv_type ) = abap_true.
      zcx_abapgit_exception=>raise(
        iv_text = |Cannot decode required source { iv_filename } as UTF-8|
        ix_previous = ix_previous ).
    ENDIF.
  ENDMETHOD.

  METHOD validate_names.
    DATA lt_counts TYPE ty_source_counts.

    LOOP AT it_names INTO DATA(lv_name).
      DATA lv_type TYPE string.
      DATA lv_object TYPE string.
      DATA lv_native TYPE abap_bool.
      parse_strict_name(
        EXPORTING iv_name = lv_name
        IMPORTING ev_type = lv_type ev_name = lv_object ev_native = lv_native ).
      IF lv_type IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE lt_counts ASSIGNING FIELD-SYMBOL(<ls_count>)
        WITH TABLE KEY obj_type = lv_type obj_name = lv_object.
      IF sy-subrc <> 0.
        INSERT VALUE #( obj_type = lv_type obj_name = lv_object )
          INTO TABLE lt_counts ASSIGNING <ls_count>.
      ENDIF.
      IF lv_native = abap_true.
        <ls_count>-sources += 1.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_counts INTO DATA(ls_count) WHERE sources <> 1.
      zcx_abapgit_exception=>raise(
        iv_text = |{ iv_side } { ls_count-obj_type } { ls_count-obj_name } requires exactly one native source; found { ls_count-sources }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD has_strict_remote.
    LOOP AT it_files INTO DATA(ls_file).
      IF is_candidate( ls_file-filename ) = abap_true.
        rv_has_strict = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_candidate.
    DATA(lv_upper) = to_upper( iv_name ).
    rv_candidate = xsdbool(
      lv_upper CS '.BDEF.' OR lv_upper CS '.SRVD.' OR lv_upper CS '.DDLX.'
      OR lv_upper CS '.ASBDEF' OR lv_upper CS '.SRVDSRV'
      OR lv_upper CS '.ASSRVD' OR lv_upper CS '.ASDDLXS' ).
  ENDMETHOD.

  METHOD parse_strict_name.
    CLEAR: ev_type, ev_name, ev_native.
    CHECK is_candidate( iv_name ) = abap_true.

    DATA(lv_upper) = to_upper( iv_name ).
    REPLACE ALL OCCURRENCES OF '\' IN lv_upper WITH '/'.
    SPLIT lv_upper AT '/' INTO TABLE DATA(lt_path).
    DATA(lv_base) = VALUE string( lt_path[ lines( lt_path ) ] OPTIONAL ).
    SPLIT lv_base AT '.' INTO TABLE DATA(lt_parts).
    IF lines( lt_parts ) <> 3.
      zcx_abapgit_exception=>raise( iv_text = |Malformed source file { iv_name }| ).
    ENDIF.

    ev_name = lt_parts[ 1 ].
    ev_type = lt_parts[ 2 ].
    DATA(lv_extension) = lt_parts[ 3 ].
    CASE ev_type.
      WHEN 'BDEF'.
        ev_native = xsdbool( lv_extension = 'ASBDEF' ).
        IF ev_native = abap_false AND lv_extension <> 'XML'.
          CLEAR ev_type.
        ENDIF.
      WHEN 'SRVD'.
        ev_native = xsdbool( lv_extension = 'SRVDSRV' OR lv_extension = 'ASSRVD' ).
        IF ev_native = abap_false AND lv_extension <> 'XML'.
          CLEAR ev_type.
        ENDIF.
      WHEN 'DDLX'.
        ev_native = xsdbool( lv_extension = 'ASDDLXS' ).
        IF ev_native = abap_false AND lv_extension <> 'XML'.
          CLEAR ev_type.
        ENDIF.
      WHEN OTHERS.
        CLEAR ev_type.
    ENDCASE.

    IF ev_type IS INITIAL OR ev_name IS INITIAL
       OR ev_name CN 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_#'.
      zcx_abapgit_exception=>raise( iv_text = |Malformed source file { iv_name }| ).
    ENDIF.
    IF ev_name CS '#'.
      SPLIT ev_name AT '#' INTO TABLE DATA(lt_name_parts).
      IF lines( lt_name_parts ) <> 3 OR lt_name_parts[ 1 ] IS NOT INITIAL
         OR lt_name_parts[ 2 ] IS INITIAL OR lt_name_parts[ 3 ] IS INITIAL.
        zcx_abapgit_exception=>raise( iv_text = |Malformed source file { iv_name }| ).
      ENDIF.
      REPLACE ALL OCCURRENCES OF '#' IN ev_name WITH '/'.
    ENDIF.
    IF strlen( ev_name ) > 30.
      zcx_abapgit_exception=>raise( iv_text = |Malformed source file { iv_name }| ).
    ENDIF.
  ENDMETHOD.

  METHOD is_strict_type.
    rv_strict = xsdbool( iv_type = 'BDEF' OR iv_type = 'SRVD' OR iv_type = 'DDLX' ).
  ENDMETHOD.
ENDCLASS.
