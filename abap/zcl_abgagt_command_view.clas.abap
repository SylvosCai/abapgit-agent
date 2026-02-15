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
             include_methods TYPE abap_bool,
             include_docs TYPE abap_bool,
           END OF ty_view_params.

    TYPES: BEGIN OF ty_view_object,
             name TYPE string,
             type TYPE string,
             type_text TYPE string,
             description TYPE string,
           END OF ty_view_object.

    TYPES ty_view_objects TYPE TABLE OF ty_view_object WITH NON-UNIQUE DEFAULT KEY.

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
          lv_object TYPE string.

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

    LOOP AT ls_params-objects INTO lv_object.
      DATA(ls_object) = VALUE ty_view_object( name = lv_object ).

      IF ls_params-type IS NOT INITIAL.
        ls_object-type = ls_params-type.
      ELSE.
        ls_object-type = detect_object_type( lv_object ).
      ENDIF.

      CASE ls_object-type.
        WHEN 'CLAS'. ls_object-type_text = 'Class'.
        WHEN 'INTF'. ls_object-type_text = 'Interface'.
        WHEN 'TABL'. ls_object-type_text = 'Table'.
        WHEN 'STRU'. ls_object-type_text = 'Structure'.
        WHEN 'DTEL'. ls_object-type_text = 'Data Element'.
        WHEN 'FUGR'. ls_object-type_text = 'Function Group'.
        WHEN 'PROG'. ls_object-type_text = 'Program'.
        WHEN OTHERS. ls_object-type_text = ls_object-type.
      ENDCASE.

      ls_object = get_object_info( iv_name = lv_object iv_type = ls_object-type ).

      APPEND ls_object TO lt_objects.
    ENDLOOP.

    ls_result-success = abap_true.
    ls_result-message = 'Retrieved object(s)'.
    ls_result-objects = lt_objects.
    ls_result-summary = build_summary( lt_objects ).

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD detect_object_type.
    DATA lv_prefix TYPE string.
    DATA lv_tab TYPE dd02l-tabname.
    DATA lv_dtel TYPE dd04l-rollname.

    lv_prefix = iv_name(4).

    CASE lv_prefix.
      WHEN 'ZCL_'. rv_type = 'CLAS'.
      WHEN 'ZIF_'. rv_type = 'INTF'.
      WHEN 'ZTY_'. rv_type = 'DTEL'.
      WHEN 'ZS__'. rv_type = 'DTEL'.
      WHEN OTHERS.
        SELECT SINGLE tabname FROM dd02l INTO lv_tab
          WHERE tabname = iv_name.
        IF sy-subrc = 0.
          rv_type = 'TABL'.
        ELSE.
          SELECT SINGLE rollname FROM dd04l INTO lv_dtel
            WHERE rollname = iv_name.
          IF sy-subrc = 0.
            rv_type = 'DTEL'.
          ELSE.
            rv_type = 'CLAS'.
          ENDIF.
        ENDIF.
    ENDCASE.

    IF iv_name CP 'ZCL_*' OR iv_name CP 'ZCL*'.
      rv_type = 'CLAS'.
    ELSEIF iv_name CP 'ZIF_*' OR iv_name CP 'ZIF*'.
      rv_type = 'INTF'.
    ENDIF.
  ENDMETHOD.

  METHOD get_object_info.
    DATA lv_descr TYPE string.
    DATA lv_ddtext TYPE string.

    rs_object-name = iv_name.
    rs_object-type = iv_type.

    CASE iv_type.
      WHEN 'CLAS'.
        rs_object-type_text = 'Class'.
        SELECT SINGLE seoclass~descr FROM seoclass INTO lv_descr
          WHERE clsname = iv_name.
        rs_object-description = lv_descr.

      WHEN 'INTF'.
        rs_object-type_text = 'Interface'.
        SELECT SINGLE seoclass~descr FROM seoclass INTO lv_descr
          WHERE clsname = iv_name.
        rs_object-description = lv_descr.

      WHEN 'TABL'.
        rs_object-type_text = 'Table'.
        SELECT SINGLE dd02l~ddtext FROM dd02l INTO lv_ddtext
          WHERE tabname = iv_name.
        rs_object-description = lv_ddtext.

      WHEN 'STRU'.
        rs_object-type_text = 'Structure'.
        SELECT SINGLE dd02l~ddtext FROM dd02l INTO lv_ddtext
          WHERE tabname = iv_name.
        rs_object-description = lv_ddtext.

      WHEN 'DTEL'.
        rs_object-type_text = 'Data Element'.
        SELECT SINGLE dd04l~ddtext FROM dd04l INTO lv_ddtext
          WHERE rollname = iv_name.
        rs_object-description = lv_ddtext.

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
