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
      IF lv_type IS INITIAL.
        lv_type = detect_object_type( lv_object ).
      ENDIF.

      " Check if object was not found in TADIR
      IF lv_type IS INITIAL.
        ls_info-not_found = abap_true.
        ls_info-type_text = 'Unknown'.
      ELSE.
        ls_info-type = lv_type.

        " Set type text
        CASE lv_type.
          WHEN 'CLAS'. ls_info-type_text = 'Class'.
          WHEN 'INTF'. ls_info-type_text = 'Interface'.
          WHEN 'TABL'. ls_info-type_text = 'Table'.
          WHEN 'STRU'. ls_info-type_text = 'Structure'.
          WHEN 'DTEL'. ls_info-type_text = 'Data Element'.
          WHEN 'TTYP'. ls_info-type_text = 'Table Type'.
          WHEN 'DDLS'. ls_info-type_text = 'CDS View'.
          WHEN OTHERS. ls_info-type_text = lv_type.
        ENDCASE.

        " Get viewer and retrieve info only if object was found
        TRY.
            lo_viewer = lo_factory->get_viewer( lv_type ).
            IF lo_viewer IS BOUND.
              ls_info = lo_viewer->get_info( lv_object ).
            ELSE.
              ls_info = get_object_info( iv_name = lv_object iv_type = lv_type ).
            ENDIF.
          CATCH cx_sy_create_object_error.
            " Fallback for unknown types
            ls_info = get_object_info( iv_name = lv_object iv_type = lv_type ).
        ENDTRY.
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
    " Query TADIR to find actual object type
    SELECT SINGLE object FROM tadir
      INTO rv_type
      WHERE obj_name = iv_name
        AND object IN ('CLAS', 'INTF', 'TABL', 'DTEL', 'STRU', 'TTYP', 'DDLS').
  ENDMETHOD.

  METHOD get_object_info.
    DATA lv_obj_name TYPE string.
    DATA lv_devclass TYPE tadir-devclass.

    rs_object-name = iv_name.
    rs_object-type = iv_type.

    CASE iv_type.
      WHEN 'CLAS' OR 'INTF'.
        IF iv_type = 'CLAS'.
          rs_object-type_text = 'Class'.
        ELSE.
          rs_object-type_text = 'Interface'.
        ENDIF.

        SELECT SINGLE obj_name devclass FROM tadir
          INTO (lv_obj_name, lv_devclass)
          WHERE obj_name = iv_name
            AND object = iv_type.
        IF sy-subrc = 0.
          rs_object-description = |{ rs_object-type_text } { iv_name } in { lv_devclass }|.
        ENDIF.

      WHEN 'TABL'.
        rs_object-type_text = 'Table'.
        SELECT SINGLE obj_name devclass FROM tadir
          INTO (lv_obj_name, lv_devclass)
          WHERE obj_name = iv_name
            AND object = 'TABL'.
        IF sy-subrc = 0.
          rs_object-description = |Table { iv_name } in { lv_devclass }|.
        ENDIF.

      WHEN 'STRU'.
        rs_object-type_text = 'Structure'.
        SELECT SINGLE obj_name devclass FROM tadir
          INTO (lv_obj_name, lv_devclass)
          WHERE obj_name = iv_name
            AND object = 'TABL'.
        IF sy-subrc = 0.
          rs_object-description = |Structure { iv_name } in { lv_devclass }|.
        ENDIF.

      WHEN 'DTEL'.
        rs_object-type_text = 'Data Element'.
        SELECT SINGLE obj_name devclass FROM tadir
          INTO (lv_obj_name, lv_devclass)
          WHERE obj_name = iv_name
            AND object = 'DTEL'.
        IF sy-subrc = 0.
          rs_object-description = |Data Element { iv_name } in { lv_devclass }|.
        ENDIF.

      WHEN OTHERS.
        rs_object-type_text = iv_type.
    ENDCASE.
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
