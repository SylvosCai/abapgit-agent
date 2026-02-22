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

    METHODS get_method_name
      IMPORTING iv_classname    TYPE string
                iv_include_name TYPE string
      RETURNING VALUE(rv_method_name) TYPE string.

    METHODS convert_base36_to_int
      IMPORTING iv_base36      TYPE string
      RETURNING VALUE(rv_result) TYPE i.

    METHODS get_include_type
      IMPORTING iv_include_name TYPE string
      RETURNING VALUE(rv_include_type) TYPE string.

ENDCLASS.

CLASS zcl_abgagt_command_where IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_where.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
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

      " Get where-used list
      DATA lv_obj_type TYPE trobjtype.
      lv_obj_type = lv_type.
      DATA lv_obj_name TYPE sobj_name.
      lv_obj_name = lv_object.
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
      " Get method name for method includes
      ls_ref_out-method_name = get_method_name(
        iv_classname = CONV #( <ls_ref>-obj_name )
        iv_include_name = CONV string( <ls_ref>-sub_name ) ).
      " Get human-readable include type
      ls_ref_out-include_type = get_include_type( CONV string( <ls_ref>-sub_name ) ).
      ls_ref_out-package = <ls_ref>-appl_packet.
      APPEND ls_ref_out TO rt_references.
    ENDLOOP.

    " Sort by object name
    SORT rt_references BY object.
  ENDMETHOD.

  METHOD get_method_name.
    " Extract method name from include name
    rv_method_name = ''.

    " Split include name by '=' to get class name and include type
    DATA lt_parts TYPE TABLE OF string.
    SPLIT iv_include_name AT '=' INTO TABLE lt_parts.

    DATA lv_include TYPE string.
    DATA lv_include_len TYPE i.

    " Check if include name contains '=' (padded case)
    IF iv_include_name CS '='.
      " With padding: split by '=' and get last part
      LOOP AT lt_parts INTO DATA(lv_part) WHERE table_line IS NOT INITIAL.
        lv_include = lv_part.
      ENDLOOP.
    ELSE.
      " No padding: extract include type by length
      " Total length 32: last 2 chars (CU, CI, IU)
      " Total length 34: last 4 chars (CCAU, CCIMP, CCDEF)
      " Total length 35: last 5 chars (CM001-CM099, CM00A-CM99Z)
      lv_include_len = strlen( iv_include_name ).
      IF lv_include_len >= 32.
        lv_include = iv_include_name+30(2).
      ELSEIF lv_include_len >= 34.
        lv_include = iv_include_name+30(4).
      ELSEIF lv_include_len >= 35.
        lv_include = iv_include_name+30(5).
      ELSE.
        " String too short, try to get whatever is left
        lv_include = iv_include_name.
      ENDIF.
    ENDIF.

    " Check include type
    CASE lv_include.
      WHEN 'CCAU'.
        rv_method_name = 'UNIT TEST'.
      WHEN 'CCDEF'.
        rv_method_name = 'LOCAL DEFINITIONS'.
      WHEN 'CCIMP'.
        rv_method_name = 'LOCAL IMPLEMENTATIONS'.
      WHEN 'CI'.
        rv_method_name = 'LOCAL INTERFACES'.
      WHEN 'CT'.
        rv_method_name = 'MACROS'.
      WHEN OTHERS.
        " Check if it's a method include (CM###)
        IF strlen( lv_include ) >= 2 AND lv_include(2) = 'CM'.
          " Convert CM003 to 3 (remove CM prefix)
          " Extended methods like CM00A, CM00B need base-36 conversion
          DATA(lv_num_str) = substring( val = lv_include off = 2 ).
          " Use base-36 conversion (0-9 = 0-9, A-Z = 10-35)
          DATA(lv_include_num) = convert_base36_to_int( lv_num_str ).
          " Get method name from TMDIR
          SELECT SINGLE methodname
            FROM tmdir
            INTO rv_method_name
            WHERE classname = iv_classname
              AND methodindx = lv_include_num.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD convert_base36_to_int.
    " Convert base-36 string to integer
    " 0-9 = 0-9, A-Z = 10-35
    DATA lv_len TYPE i.
    DATA lv_idx TYPE i.
    DATA lv_char TYPE c.
    DATA lv_val TYPE i.
    DATA lv_digit TYPE c.

    rv_result = 0.
    lv_len = strlen( iv_base36 ).

    IF lv_len = 0.
      RETURN.
    ENDIF.

    DO lv_len TIMES.
      lv_idx = sy-index - 1.
      lv_digit = iv_base36+lv_idx(1).
      lv_digit = to_upper( lv_digit ).

      IF lv_digit CO '0123456789'.
        lv_val = CONV i( lv_digit ).
      ELSEIF lv_digit CO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
        " Convert A-Z to 10-35 using CASE
        CASE lv_digit.
          WHEN 'A'. lv_val = 10.
          WHEN 'B'. lv_val = 11.
          WHEN 'C'. lv_val = 12.
          WHEN 'D'. lv_val = 13.
          WHEN 'E'. lv_val = 14.
          WHEN 'F'. lv_val = 15.
          WHEN 'G'. lv_val = 16.
          WHEN 'H'. lv_val = 17.
          WHEN 'I'. lv_val = 18.
          WHEN 'J'. lv_val = 19.
          WHEN 'K'. lv_val = 20.
          WHEN 'L'. lv_val = 21.
          WHEN 'M'. lv_val = 22.
          WHEN 'N'. lv_val = 23.
          WHEN 'O'. lv_val = 24.
          WHEN 'P'. lv_val = 25.
          WHEN 'Q'. lv_val = 26.
          WHEN 'R'. lv_val = 27.
          WHEN 'S'. lv_val = 28.
          WHEN 'T'. lv_val = 29.
          WHEN 'U'. lv_val = 30.
          WHEN 'V'. lv_val = 31.
          WHEN 'W'. lv_val = 32.
          WHEN 'X'. lv_val = 33.
          WHEN 'Y'. lv_val = 34.
          WHEN 'Z'. lv_val = 35.
        ENDCASE.
      ELSE.
        CONTINUE.
      ENDIF.

      rv_result = rv_result * 36 + lv_val.
    ENDDO.
  ENDMETHOD.

  METHOD get_include_type.
    " Extract include type from include name and return human-readable description
    DATA lv_include_len TYPE i.
    DATA lv_include TYPE string.

    rv_include_type = 'Unknown'.

    lv_include_len = strlen( iv_include_name ).
    " Check from longest to shortest (35, 34, 32)
    IF lv_include_len >= 35.
      lv_include = iv_include_name+30(5).
    ELSEIF lv_include_len >= 34.
      lv_include = iv_include_name+30(4).
    ELSEIF lv_include_len >= 32.
      lv_include = iv_include_name+30(2).
    ELSE.
      lv_include = iv_include_name.
    ENDIF.

    CASE lv_include.
      WHEN 'CU'.
        rv_include_type = 'Public Section'.
      WHEN 'CO'.
        rv_include_type = 'Protected Section'.
      WHEN 'CP'.
        rv_include_type = 'Private Section'.
      WHEN 'CCAU'.
        rv_include_type = 'Unit Test'.
      WHEN 'CCIMP'.
        rv_include_type = 'Local Implementations'.
      WHEN 'CCDEF'.
        rv_include_type = 'Local Definitions'.
      WHEN 'CI'.
        rv_include_type = 'Local Interfaces'.
      WHEN 'CT'.
        rv_include_type = 'Macros'.
      WHEN 'IU'.
        rv_include_type = 'Interface Section'.
      WHEN OTHERS.
        " Check if it's a method include (CM###)
        IF strlen( lv_include ) >= 2 AND lv_include(2) = 'CM'.
          rv_include_type = 'Class Method'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
