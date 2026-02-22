*"*"use source
*"*"Local Interface:
*"**********************************************************************
" WHERE command implementation - where-used list for ABAP objects
CLASS zcl_abgagt_command_where DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_where_params,
             objects TYPE string_table,
             type TYPE string,
             limit TYPE i,
           END OF ty_where_params.

    TYPES: BEGIN OF ty_reference,
             object TYPE string,
             object_type TYPE string,
             include_name TYPE string,
             method_name TYPE string,
             include_type TYPE string,
             package TYPE string,
           END OF ty_reference.

    TYPES ty_references TYPE STANDARD TABLE OF ty_reference WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_where_object,
             name TYPE string,
             type TYPE string,
             error TYPE string,
             references TYPE ty_references,
             count TYPE i,
           END OF ty_where_object.

    TYPES ty_where_objects TYPE STANDARD TABLE OF ty_where_object WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_summary,
             total_objects TYPE i,
             total_references TYPE i,
           END OF ty_summary.

    TYPES: BEGIN OF ty_where_result,
             success TYPE abap_bool,
             command TYPE string,
             message TYPE string,
             objects TYPE ty_where_objects,
             summary TYPE ty_summary,
             error TYPE string,
           END OF ty_where_result.

    METHODS detect_object_type
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rv_type) TYPE string.

    METHODS get_where_used_list
      IMPORTING iv_obj_type TYPE trobjtype
                iv_obj_name TYPE sobj_name
      RETURNING VALUE(rt_references) TYPE ty_references.

  PRIVATE SECTION.
    DATA mo_util TYPE REF TO zif_abgagt_util.

ENDCLASS.

CLASS zcl_abgagt_command_where IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_where.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    " Initialize utility instance
    mo_util = zcl_abgagt_util=>get_instance( ).

    DATA: ls_params TYPE ty_where_params,
          ls_result TYPE ty_where_result,
          lt_objects TYPE ty_where_objects,
          lv_object TYPE string,
          ls_where_obj TYPE ty_where_object,
          lt_references TYPE ty_references.

    ls_result-command = zif_abgagt_command=>gc_where.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-objects IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error = 'Objects parameter is required'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Default limit
    IF ls_params-limit IS INITIAL.
      ls_params-limit = 100.
    ENDIF.

    LOOP AT ls_params-objects INTO lv_object.
      ls_where_obj-name = lv_object.

      DATA(lv_type) = ls_params-type.
      IF lv_type IS INITIAL.
        lv_type = detect_object_type( lv_object ).
      ENDIF.

      " Default to CLAS if not detected
      IF lv_type IS INITIAL.
        lv_type = 'CLAS'.
      ENDIF.

      ls_where_obj-type = lv_type.

      " Check if object exists in TADIR before calling where-used list
      DATA lv_obj_type TYPE trobjtype.
      lv_obj_type = lv_type.
      DATA lv_obj_name TYPE sobj_name.
      lv_obj_name = lv_object.
      DATA lv_exists TYPE tadir-object.

      SELECT SINGLE object FROM tadir
        INTO lv_exists
        WHERE obj_name = lv_obj_name
          AND object = lv_obj_type.

      IF sy-subrc <> 0.
        " Object not found - add error to result and skip
        ls_where_obj-error = |Object not found: { lv_object }|.
        APPEND ls_where_obj TO lt_objects.
        CLEAR ls_where_obj.
        CONTINUE.
      ENDIF.

      " Get where-used list
      lt_references = get_where_used_list(
        iv_obj_type = lv_obj_type
        iv_obj_name = lv_obj_name
      ).

      " Apply limit
      DATA lt_limited_refs TYPE ty_references.
      DATA ls_ref TYPE ty_reference.
      DATA lv_count TYPE i.
      IF ls_params-limit > 0.
        LOOP AT lt_references INTO ls_ref.
          APPEND ls_ref TO lt_limited_refs.
          lv_count = lv_count + 1.
          IF lv_count >= ls_params-limit.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSE.
        lt_limited_refs = lt_references.
      ENDIF.

      ls_where_obj-references = lt_limited_refs.
      ls_where_obj-count = lines( lt_limited_refs ).

      APPEND ls_where_obj TO lt_objects.
      CLEAR ls_where_obj.
    ENDLOOP.

    " Build summary
    ls_result-summary-total_objects = lines( lt_objects ).
    LOOP AT lt_objects INTO ls_where_obj.
      ls_result-summary-total_references = ls_result-summary-total_references + ls_where_obj-count.
    ENDLOOP.

    ls_result-success = abap_true.
    IF ls_result-summary-total_references = 1.
      ls_result-message = |Found { ls_result-summary-total_references } reference|.
    ELSE.
      ls_result-message = |Found { ls_result-summary-total_references } references|.
    ENDIF.

    ls_result-objects = lt_objects.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD detect_object_type.
    " Auto-detect object type from name pattern
    IF iv_name CP 'ZCL_*' OR iv_name CP 'CL_*'.
      rv_type = 'CLAS'.
    ELSEIF iv_name CP 'ZIF_*' OR iv_name CP 'IF_*'.
      rv_type = 'INTF'.
    ELSEIF iv_name CP 'SAPL*'.
      rv_type = 'PROG'.
    ELSE.
      " Check TADIR for actual type
      SELECT SINGLE object FROM tadir
        INTO rv_type
        WHERE obj_name = iv_name
          AND object IN ('CLAS', 'INTF', 'PROG').
      IF sy-subrc <> 0.
        rv_type = 'CLAS'. " Default fallback
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_where_used_list.
    DATA: lt_ref TYPE akb_except_type,
          ls_ref_out TYPE ty_reference.

    " Call the function module
    CALL FUNCTION 'AKB_WHERE_USED_LIST'
      EXPORTING
        obj_type = iv_obj_type
        obj_name = iv_obj_name
      IMPORTING
        references = lt_ref.

    " Map to output structure - convert to string fields for JSON serialization
    " Filter: only include CLAS and PROG types
    LOOP AT lt_ref ASSIGNING FIELD-SYMBOL(<ls_ref>).
      IF <ls_ref>-obj_type <> 'CLAS' AND <ls_ref>-obj_type <> 'PROG'.
        CONTINUE.
      ENDIF.
      ls_ref_out-object = <ls_ref>-obj_name.
      ls_ref_out-object_type = <ls_ref>-obj_type.
      ls_ref_out-include_name = <ls_ref>-sub_name.
      CLEAR ls_ref_out-method_name.

      " Get human-readable include type description
      ls_ref_out-include_type = mo_util->get_include_description(
        CONV string( <ls_ref>-sub_name ) ).

      " Get method name if it's a method include
      DATA lv_include TYPE string.
      DATA lv_include_len TYPE i.
      lv_include_len = strlen( <ls_ref>-sub_name ).
      IF lv_include_len >= 35.
        lv_include = <ls_ref>-sub_name+30(5).
      ELSEIF lv_include_len >= 34.
        lv_include = <ls_ref>-sub_name+30(4).
      ELSEIF lv_include_len >= 32.
        lv_include = <ls_ref>-sub_name+30(2).
      ELSE.
        lv_include = <ls_ref>-sub_name.
      ENDIF.

      " Check if it's a method include (CM###)
      IF strlen( lv_include ) >= 2 AND lv_include(2) = 'CM'.
        " Convert method index from base-36
        DATA(lv_method_index) = mo_util->convert_method_index( lv_include ).
        " Get method name from TMDIR
        ls_ref_out-method_name = mo_util->get_method_name(
          iv_classname = CONV string( <ls_ref>-obj_name )
          iv_method_index = lv_method_index ).
      ENDIF.

      ls_ref_out-package = <ls_ref>-appl_packet.
      APPEND ls_ref_out TO rt_references.
    ENDLOOP.

    " Sort by object name
    SORT rt_references BY object.
  ENDMETHOD.

ENDCLASS.
