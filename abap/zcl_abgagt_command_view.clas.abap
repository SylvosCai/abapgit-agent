*"*"use source
*"*"Local Interface:
*"**********************************************************************
" VIEW command implementation - view ABAP object definitions
CLASS zcl_abgagt_command_view DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_view_params,
             objects TYPE string_table,
             type TYPE string,
           END OF ty_view_params.

    TYPES: BEGIN OF ty_component,
             field TYPE string,
             key TYPE abap_bool,
             type TYPE string,
             length TYPE int4,
             dataelement TYPE string,
             description TYPE string,
           END OF ty_component.

    TYPES: BEGIN OF ty_view_object,
             name TYPE string,
             type TYPE string,
             type_text TYPE string,
             description TYPE string,
             devclass TYPE tadir-devclass,
             domain TYPE string,
             domain_type TYPE string,
             domain_length TYPE int4,
             domain_decimals TYPE int4,
             source TYPE string,
             definition TYPE string,
             not_found TYPE abap_bool,
             components TYPE STANDARD TABLE OF ty_component WITH DEFAULT KEY,
           END OF ty_view_object.

    TYPES ty_view_objects TYPE STANDARD TABLE OF ty_view_object WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_summary,
             total TYPE i,
             by_type TYPE string_table,
           END OF ty_summary.

    TYPES: BEGIN OF ty_view_result,
             success TYPE abap_bool,
             command TYPE string,
             message TYPE string,
             objects TYPE ty_view_objects,
             summary TYPE ty_summary,
             error TYPE string,
           END OF ty_view_result.

    METHODS detect_object_type
      IMPORTING iv_name TYPE string
      EXPORTING ev_type_text TYPE string
                ev_devclass TYPE tadir-devclass
      RETURNING VALUE(rv_type) TYPE string.

    METHODS get_object_info
      IMPORTING iv_name TYPE string
                iv_type TYPE string
      RETURNING VALUE(rs_object) TYPE ty_view_object.

    METHODS build_summary
      IMPORTING it_objects TYPE ty_view_objects
      RETURNING VALUE(rs_summary) TYPE ty_summary.

ENDCLASS.

CLASS zcl_abgagt_command_view IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_view.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_view_params,
          ls_result TYPE ty_view_result,
          lt_objects TYPE ty_view_objects,
          lv_object TYPE string,
          lo_factory TYPE REF TO zcl_abgagt_viewer_factory,
          lo_viewer TYPE REF TO zif_abgagt_viewer,
          ls_info TYPE ty_view_object.

    ls_result-command = zif_abgagt_command=>gc_view.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-objects IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error = 'Objects parameter is required'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    lo_factory = zcl_abgagt_viewer_factory=>get_instance( ).

    LOOP AT ls_params-objects INTO lv_object.
      ls_info-name = lv_object.

      DATA(lv_type) = ls_params-type.
      DATA lv_type_text TYPE string.
      DATA lv_devclass TYPE tadir-devclass.
      lv_type_text = ''.
      IF lv_type IS INITIAL.
        lv_type = detect_object_type(
          EXPORTING
            iv_name = lv_object
          IMPORTING
            ev_type_text = lv_type_text
            ev_devclass = lv_devclass ).
      ENDIF.

      " Set type from detection result
      ls_info-type = lv_type.
      ls_info-type_text = lv_type_text.
      ls_info-devclass = lv_devclass.

      " Get viewer and retrieve info
      TRY.
          lo_viewer = lo_factory->get_viewer( lv_type ).
          IF lo_viewer IS BOUND.
            ls_info = lo_viewer->get_info( lv_object ).
          ELSE.
            ls_info = get_object_info( iv_name = lv_object iv_type = lv_type ).
          ENDIF.
        CATCH cx_sy_create_object_error cx_dd_ddl_read.
          " Fallback for unknown types
          ls_info = get_object_info( iv_name = lv_object iv_type = lv_type ).
      ENDTRY.

      " Preserve devclass from detection (viewer may not have it for source includes)
      IF ls_info-devclass IS INITIAL AND lv_devclass IS NOT INITIAL.
        ls_info-devclass = lv_devclass.
      ENDIF.

      APPEND ls_info TO lt_objects.
    ENDLOOP.

    ls_result-success = abap_true.
    ls_result-message = 'Retrieved object(s)'.
    ls_result-objects = lt_objects.
    ls_result-summary = build_summary( lt_objects ).

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD detect_object_type.
    " Use utility to detect source include info
    DATA: lo_util TYPE REF TO zif_abgagt_util,
          ls_include_info TYPE zif_abgagt_util=>ty_include_info,
          ls_obj_info TYPE zif_abgagt_util=>ty_object_info,
          lv_obj_name TYPE tadir-obj_name.

    lo_util = zcl_abgagt_util=>get_instance( ).
    ls_include_info = lo_util->detect_include_info( iv_name ).

    " If it's a source include, use the detected info
    IF ls_include_info-is_source_include = abap_true AND ls_include_info-obj_type IS NOT INITIAL.
      rv_type = ls_include_info-obj_type.
      ev_type_text = ls_include_info-type_text.
      " Get devclass from TADIR for source include using unpadded name
      " Remove trailing = from class/interface name
      lv_obj_name = ls_include_info-obj_name.
      FIND REGEX '^([A-Z0-9_]+)=+' IN lv_obj_name MATCH OFFSET DATA(lv_offset).
      IF lv_offset > 0.
        lv_obj_name = lv_obj_name(lv_offset).
      ENDIF.
      ls_obj_info = lo_util->get_object_info_from_tadir( lv_obj_name ).
      ev_devclass = ls_obj_info-devclass.
      RETURN.
    ENDIF.

    " Query TADIR to find actual object type using utility method
    lv_obj_name = iv_name.
    ls_obj_info = lo_util->get_object_info_from_tadir( lv_obj_name ).
    rv_type = ls_obj_info-obj_type.
    ev_type_text = ls_obj_info-type_text.
    ev_devclass = ls_obj_info-devclass.
  ENDMETHOD.

  METHOD get_object_info.
    " Note: type, type_text, and devclass are already set by detect_object_type
    " Only need to build the description
    rs_object-name = iv_name.
    rs_object-type = iv_type.

    " Build description using already-set type_text and devclass
    IF rs_object-devclass IS NOT INITIAL.
      rs_object-description = |{ rs_object-type_text } { iv_name } in { rs_object-devclass }|.
    ELSE.
      rs_object-description = |{ rs_object-type_text } { iv_name }|.
    ENDIF.
  ENDMETHOD.

  METHOD build_summary.
    rs_summary-total = lines( it_objects ).

    DATA lv_type TYPE string.
    DATA lv_found.
    DATA ls_type TYPE string.

    LOOP AT it_objects INTO DATA(ls_obj).
      lv_type = ls_obj-type.
      lv_found = abap_false.
      LOOP AT rs_summary-by_type INTO ls_type.
        IF ls_type = lv_type.
          lv_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_found = abap_false.
        APPEND lv_type TO rs_summary-by_type.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
