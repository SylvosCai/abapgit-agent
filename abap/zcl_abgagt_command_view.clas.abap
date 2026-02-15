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
          lt_objects TYPE ty_view_objects.

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

    " Process each object
    LOOP AT ls_params-objects INTO DATA(lv_object).
      DATA(ls_object) = VALUE ty_view_object( name = lv_object ).

      " Detect or use specified type
      IF ls_params-type IS NOT INITIAL.
        ls_object-type = ls_params-type.
      ELSE.
        ls_object-type = detect_object_type( lv_object ).
      ENDIF.

      " Set type text
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

      " Get object info based on type
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
    " Auto-detect object type from name prefix
    DATA(lv_prefix) = iv_name(4).

    CASE lv_prefix.
      WHEN 'ZCL_'. rv_type = 'CLAS'.
      WHEN 'ZIF_'. rv_type = 'INTF'.
      WHEN 'ZTY_'. rv_type = 'DTEL'.
      WHEN 'ZS__'. rv_type = 'DTEL'.
      WHEN OTHERS.
        " Check for table/structure patterns
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
            rv_type = 'CLAS'. " Default to CLAS
          ENDIF.
        ENDIF.
    ENDCASE.

    " Override with type parameter if specified
    IF iv_name CP 'ZCL_*' OR iv_name CP 'ZCL*'.
      rv_type = 'CLAS'.
    ELSEIF iv_name CP 'ZIF_*' OR iv_name CP 'ZIF*'.
      rv_type = 'INTF'.
    ENDIF.
  ENDMETHOD.

  METHOD get_class_info.
    rs_object-name = iv_name.
    rs_object-type = 'CLAS'.
    rs_object-type_text = 'Class'.

    " Get class description
    SELECT SINGLE descript FROM seoclass INTO @DATA(lv_descript)
      WHERE clsname = @iv_name.
    IF sy-subrc = 0.
      rs_object-description = lv_descript.
    ENDIF.

    " Get method definitions
    SELECT cmpname descript expos FROM seocompodf
      INTO TABLE @DATA(lt_methods)
      WHERE clsname = @iv_name
        AND expos IN ('0','1','2')
        AND type = '0'.
    IF sy-subrc = 0.
      LOOP AT lt_methods INTO DATA(ls_method).
        DATA(ls_method_out) = VALUE ty_method(
          name = ls_method-cmpname
          descript = ls_method-descript
        ).
        CASE ls_method-expos.
          WHEN '0'. ls_method_out-visibility = 'PUBLIC'.
          WHEN '1'. ls_method_out-visibility = 'PROTECTED'.
          WHEN '2'. ls_method_out-visibility = 'PRIVATE'.
        ENDCASE.
        APPEND ls_method_out TO rs_object-methods.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_interface_info.
    rs_object-name = iv_name.
    rs_object-type = 'INTF'.
    rs_object-type_text = 'Interface'.

    " Get interface description
    SELECT SINGLE descript FROM seoclass INTO @DATA(lv_descript)
      WHERE clsname = @iv_name.
    IF sy-subrc = 0.
      rs_object-description = lv_descript.
    ENDIF.

    " Get interface methods
    SELECT cmpname descript FROM seocompodf
      INTO TABLE @DATA(lt_methods)
      WHERE clsname = @iv_name.
    IF sy-subrc = 0.
      LOOP AT lt_methods INTO DATA(ls_method).
        APPEND VALUE ty_method(
          name = ls_method-cmpname
          descript = ls_method-descript
          visibility = 'PUBLIC'
        ) TO rs_object-methods.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_table_info.
    rs_object-name = iv_name.
    rs_object-type = 'TABL'.
    rs_object-type_text = 'Table'.

    " Get table description
    SELECT SINGLE ddtext FROM dd02l INTO @DATA(lv_ddtext)
      WHERE tabname = @iv_name.
    IF sy-subrc = 0.
      rs_object-description = lv_ddtext.
    ENDIF.

    " Get table fields
    SELECT fieldname keyflag datatype leng FROM dd03l
      INTO TABLE @DATA(lt_fields)
      WHERE tabname = @iv_name
        AND as4local = 'A'
      ORDER BY position.
    IF sy-subrc = 0.
      LOOP AT lt_fields INTO DATA(ls_field).
        DATA(lv_desc) = ''.
        " Get field description from DD04L
        SELECT SINGLE ddtext FROM dd04l INTO @lv_desc
          WHERE rollname = @ls_field-datatype.
        APPEND VALUE ty_component(
          fieldname = ls_field-fieldname
          keyflag = ls_field-keyflag
          datatype = ls_field-datatype
          leng = ls_field-leng
          description = lv_desc
        ) TO rs_object-components.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_structure_info.
    rs_object-name = iv_name.
    rs_object-type = 'STRU'.
    rs_object-type_text = 'Structure'.

    " Get structure description
    SELECT SINGLE ddtext FROM dd02l INTO @DATA(lv_ddtext)
      WHERE tabname = @iv_name.
    IF sy-subrc = 0.
      rs_object-description = lv_ddtext.
    ENDIF.

    " Get structure fields (same as tables)
    SELECT fieldname keyflag datatype leng FROM dd03l
      INTO TABLE @DATA(lt_fields)
      WHERE tabname = @iv_name
        AND as4local = 'A'
      ORDER BY position.
    IF sy-subrc = 0.
      LOOP AT lt_fields INTO DATA(ls_field).
        DATA(lv_desc) = ''.
        SELECT SINGLE ddtext FROM dd04l INTO @lv_desc
          WHERE rollname = @ls_field-datatype.
        APPEND VALUE ty_component(
          fieldname = ls_field-fieldname
          keyflag = ls_field-keyflag
          datatype = ls_field-datatype
          leng = ls_field-leng
          description = lv_desc
        ) TO rs_object-components.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_data_element_info.
    rs_object-name = iv_name.
    rs_object-type = 'DTEL'.
    rs_object-type_text = 'Data Element'.

    " Get data element info
    SELECT SINGLE ddtext datatype leng outputlen FROM dd04l
      INTO @DATA(ls_dtel)
      WHERE rollname = @iv_name.
    IF sy-subrc = 0.
      DATA lv_desc TYPE string.
      CONCATENATE ls_dtel-ddtext '(Type:' ls_dtel-datatype 'Length:' ls_dtel-leng ')' INTO lv_desc SEPARATED BY SPACE.
      rs_object-description = lv_desc.
    ENDIF.

    " Add as a component for consistent output
    APPEND VALUE ty_component(
      fieldname = iv_name
      datatype = ls_dtel-datatype
      leng = ls_dtel-leng
      description = ls_dtel-ddtext
    ) TO rs_object-components.
  ENDMETHOD.

  METHOD build_summary.
    rs_summary-total = lines( it_objects ).

    " Count by type
    LOOP AT it_objects INTO DATA(ls_obj).
      DATA(lv_type) = ls_obj-type.
      DATA(lv_found) = abap_false.
      LOOP AT rs_summary-by_type INTO DATA(ls_type).
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
