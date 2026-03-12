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
             full TYPE abap_bool,
           END OF ty_view_params.

    TYPES: BEGIN OF ty_section,
             suffix      TYPE string,
             description TYPE string,
             method_name TYPE string,
             file        TYPE string,
             lines       TYPE string_table,
           END OF ty_section.
    TYPES ty_sections TYPE STANDARD TABLE OF ty_section WITH DEFAULT KEY.

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
             sections TYPE ty_sections,
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
      RETURNING VALUE(rv_type) TYPE string.

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
      " Auto-detect type if not provided
      DATA(lv_type) = ls_params-type.
      IF lv_type IS INITIAL.
        lv_type = detect_object_type( iv_name = lv_object ).
      ENDIF.

      " Get viewer and retrieve info
      TRY.
          lo_viewer = lo_factory->get_viewer( lv_type ).
          ls_info = lo_viewer->get_info(
            iv_name = lv_object
            iv_full = ls_params-full ).
        CATCH cx_sy_create_object_error.
          " Viewer class not found - set not found flag
          ls_info-name = lv_object.
          ls_info-type = lv_type.
          ls_info-not_found = abap_true.
        CATCH cx_root.
          " Any other error (e.g. cx_dd_ddl_read) - set not found flag
          ls_info-name = lv_object.
          ls_info-type = lv_type.
          ls_info-not_found = abap_true.
      ENDTRY.

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
      RETURN.
    ENDIF.

    " Query TADIR to find actual object type using utility method
    lv_obj_name = iv_name.
    ls_obj_info = lo_util->get_object_info_from_tadir( lv_obj_name ).
    rv_type = ls_obj_info-obj_type.
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
