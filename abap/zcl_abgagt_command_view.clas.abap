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

    TYPES: BEGIN OF ty_method,
             name TYPE string,
             visibility TYPE string,
             descript TYPE string,
           END OF ty_method.

    TYPES ty_methods TYPE TABLE OF ty_method WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_component,
             fieldname TYPE string,
             keyflag TYPE abap_bool,
             datatype TYPE string,
             leng TYPE string,
             description TYPE string,
           END OF ty_component.

    TYPES ty_components TYPE TABLE OF ty_component WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_view_object,
             name TYPE string,
             type TYPE string,
             type_text TYPE string,
             description TYPE string,
             methods TYPE ty_methods,
             components TYPE ty_components,
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

    METHODS get_class_info
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rs_object) TYPE ty_view_object.

    METHODS get_interface_info
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rs_object) TYPE ty_view_object.

    METHODS get_table_info
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rs_object) TYPE ty_view_object.

    METHODS get_structure_info
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rs_object) TYPE ty_view_object.

    METHODS get_data_element_info
      IMPORTING iv_name TYPE string
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

      CASE ls_object-type.
        WHEN 'CLAS'.
          ls_object = get_class_info( lv_object ).
        WHEN 'INTF'.
          ls_object = get_interface_info( lv_object ).
        WHEN 'TABL'.
          ls_object = get_table_info( lv_object ).
        WHEN 'STRU'.
          ls_object = get_structure_info( lv_object ).
        WHEN 'DTEL'.
          ls_object = get_data_element_info( lv_object ).
        WHEN OTHERS.
          ls_object-description = 'Unsupported object type'.
      ENDCASE.

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
    lv_prefix = iv_name(4).

    CASE lv_prefix.
      WHEN 'ZCL_'. rv_type = 'CLAS'.
      WHEN 'ZIF_'. rv_type = 'INTF'.
      WHEN 'ZTY_'. rv_type = 'DTEL'.
      WHEN 'ZS__'. rv_type = 'DTEL'.
      WHEN OTHERS.
        SELECT SINGLE tabname FROM dd02l INTO @DATA(lv_tab)
          WHERE tabname = @iv_name.
        IF sy-subrc = 0.
          rv_type = 'TABL'.
        ELSE.
          SELECT SINGLE rollname FROM dd04l INTO @DATA(lv_dtel)
            WHERE rollname = @iv_name.
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

  METHOD get_class_info.
    DATA: lv_name TYPE string,
          lv_descr TYPE string,
          lv_exposure TYPE seocompodf-exposure,
          lv_cmpname TYPE seocompodf-cmpname,
          lv_cmpdescript TYPE seocompodf-descript.

    DATA lt_methods TYPE ty_methods.
    DATA ls_method TYPE ty_method.

    SELECT clsname FROM seocompodf
      INTO lv_name
      WHERE clsname = iv_name
        AND exposure IN ('0','1','2')
        AND type = '0'.
      CLEAR ls_method.
      ls_method-name = lv_cmpname.
      ls_method-descript = lv_cmpdescript.
      CASE lv_exposure.
        WHEN '0'. ls_method-visibility = 'PUBLIC'.
        WHEN '1'. ls_method-visibility = 'PROTECTED'.
        WHEN '2'. ls_method-visibility = 'PRIVATE'.
      ENDCASE.
      APPEND ls_method TO lt_methods.
    ENDSELECT.

    SELECT descr FROM seoclass INTO lv_descr
      WHERE clsname = iv_name.

    rs_object-name = iv_name.
    rs_object-type = 'CLAS'.
    rs_object-type_text = 'Class'.
    rs_object-description = lv_descr.
    rs_object-methods = lt_methods.
  ENDMETHOD.

  METHOD get_interface_info.
    DATA: lv_name TYPE string,
          lv_descr TYPE string,
          lv_cmpname TYPE seocompodf-cmpname,
          lv_cmpdescript TYPE seocompodf-descript.

    DATA lt_methods TYPE ty_methods.
    DATA ls_method TYPE ty_method.

    SELECT clsname FROM seocompodf
      INTO lv_name
      WHERE clsname = iv_name.
      CLEAR ls_method.
      ls_method-name = lv_cmpname.
      ls_method-descript = lv_cmpdescript.
      ls_method-visibility = 'PUBLIC'.
      APPEND ls_method TO lt_methods.
    ENDSELECT.

    SELECT descr FROM seoclass INTO lv_descr
      WHERE clsname = iv_name.

    rs_object-name = iv_name.
    rs_object-type = 'INTF'.
    rs_object-type_text = 'Interface'.
    rs_object-description = lv_descr.
    rs_object-methods = lt_methods.
  ENDMETHOD.

  METHOD get_table_info.
    DATA: lv_ddtext TYPE string,
          lv_fieldname TYPE dd03l-fieldname,
          lv_keyflag TYPE dd03l-keyflag,
          lv_datatype TYPE dd03l-datatype,
          lv_leng TYPE dd03l-leng.

    DATA lt_components TYPE ty_components.
    DATA ls_component TYPE ty_component.

    SELECT ddtext FROM dd02l INTO lv_ddtext
      WHERE tabname = iv_name.

    SELECT fieldname FROM dd03l
      INTO lv_fieldname
      WHERE tabname = iv_name
        AND as4local = 'A'.
      CLEAR ls_component.
      ls_component-fieldname = lv_fieldname.
      APPEND ls_component TO lt_components.
    ENDSELECT.

    rs_object-name = iv_name.
    rs_object-type = 'TABL'.
    rs_object-type_text = 'Table'.
    rs_object-description = lv_ddtext.
    rs_object-components = lt_components.
  ENDMETHOD.

  METHOD get_structure_info.
    DATA lv_ddtext TYPE string.

    SELECT ddtext FROM dd02l INTO lv_ddtext
      WHERE tabname = iv_name.

    DATA lt_components TYPE ty_components.
    DATA ls_component TYPE ty_component.

    SELECT fieldname FROM dd03l
      INTO DATA(lv_fieldname)
      WHERE tabname = iv_name
        AND as4local = 'A'.
      CLEAR ls_component.
      ls_component-fieldname = lv_fieldname.
      APPEND ls_component TO lt_components.
    ENDSELECT.

    rs_object-name = iv_name.
    rs_object-type = 'STRU'.
    rs_object-type_text = 'Structure'.
    rs_object-description = lv_ddtext.
    rs_object-components = lt_components.
  ENDMETHOD.

  METHOD get_data_element_info.
    DATA: lv_ddtext TYPE dd04l-ddtext,
          lv_datatype TYPE dd04l-datatype,
          lv_leng TYPE dd04l-leng.

    SELECT ddtext FROM dd04l INTO lv_ddtext
      WHERE rollname = iv_name.

    DATA ls_component TYPE ty_component.
    ls_component-fieldname = iv_name.
    ls_component-datatype = lv_datatype.
    ls_component-leng = lv_leng.
    ls_component-description = lv_ddtext.

    DATA lv_desc TYPE string.
    CONCATENATE lv_ddtext '(Type:' lv_datatype 'Length:' lv_leng ')' INTO lv_desc SEPARATED BY SPACE.

    rs_object-name = iv_name.
    rs_object-type = 'DTEL'.
    rs_object-type_text = 'Data Element'.
    rs_object-description = lv_desc.
    APPEND ls_component TO rs_object-components.
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
