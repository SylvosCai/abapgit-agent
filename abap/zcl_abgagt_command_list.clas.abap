*"*"use source
*"*"Local Interface:
*"**********************************************************************
" LIST command implementation - list objects in a package
CLASS zcl_abgagt_command_list DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_list_params,
             package TYPE tdevc-devclass,
             type TYPE string,
             name TYPE string,
             limit TYPE i,
             offset TYPE i,
           END OF ty_list_params.

    TYPES: BEGIN OF ty_object,
             type TYPE string,
             name TYPE string,
           END OF ty_object.

    TYPES ty_objects TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_type_count,
             type TYPE string,
             count TYPE i,
           END OF ty_type_count.

    TYPES ty_type_counts TYPE STANDARD TABLE OF ty_type_count WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_list_result,
             success TYPE abap_bool,
             command TYPE string,
             package TYPE tdevc-devclass,
             total TYPE i,
             limit TYPE i,
             offset TYPE i,
             objects TYPE ty_objects,
             by_type TYPE ty_type_counts,
             error TYPE string,
           END OF ty_list_result.

    METHODS execute_list
      IMPORTING is_params TYPE ty_list_params
      RETURNING VALUE(rs_result) TYPE ty_list_result.

    METHODS validate_package
      IMPORTING iv_package TYPE tdevc-devclass
      RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS validate_type
      IMPORTING iv_type TYPE string
      RETURNING VALUE(rv_valid) TYPE abap_bool.

ENDCLASS.

CLASS zcl_abgagt_command_list IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_list.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_list_params,
          ls_result TYPE ty_list_result.

    ls_result-command = zif_abgagt_command=>gc_list.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Validate package
    IF ls_params-package IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error = 'Package is required'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Validate package exists
    IF validate_package( ls_params-package ) = abap_false.
      ls_result-success = abap_false.
      ls_result-package = ls_params-package.
      ls_result-error = |Package { ls_params-package } does not exist|.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Set default limit
    IF ls_params-limit IS INITIAL OR ls_params-limit <= 0.
      ls_params-limit = 100.
    ENDIF.

    " Max limit
    IF ls_params-limit > 1000.
      ls_params-limit = 1000.
    ENDIF.

    " Default offset
    IF ls_params-offset IS INITIAL.
      ls_params-offset = 0.
    ENDIF.

    " Execute list
    ls_result = execute_list( ls_params ).
    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD validate_package.
    DATA lv_package TYPE tdevc-devclass.
    SELECT SINGLE devclass FROM tdevc
      INTO lv_package
      WHERE devclass = iv_package.
    rv_valid = boolc( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD validate_type.
    " Simplified validation - always true for now
    rv_valid = abap_true.
  ENDMETHOD.

  METHOD execute_list.
    DATA: lt_objects TYPE ty_objects,
          lt_counts TYPE ty_type_counts,
          ls_object TYPE ty_object,
          ls_count TYPE ty_type_count,
          lv_package TYPE tdevc-devclass,
          lv_limit TYPE i,
          lv_offset TYPE i,
          lv_name_pattern TYPE tadir-obj_name,
          lt_type_range TYPE RANGE OF tadir-object,
          ls_type LIKE LINE OF lt_type_range.

    lv_package = is_params-package.
    lv_limit = is_params-limit.
    lv_offset = is_params-offset.
    lv_name_pattern = is_params-name.

    " Build type range table
    IF is_params-type IS NOT INITIAL.
      DATA lt_type_strings TYPE STANDARD TABLE OF string.
      SPLIT to_upper( is_params-type ) AT ',' INTO TABLE lt_type_strings.
      LOOP AT lt_type_strings INTO DATA(lv_type_str).
        CLEAR ls_type.
        ls_type-sign = 'I'.
        ls_type-option = 'EQ'.
        ls_type-low = lv_type_str.
        APPEND ls_type TO lt_type_range.
      ENDLOOP.
    ENDIF.

    " Convert name pattern - replace * with % for SQL LIKE
    IF lv_name_pattern IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN lv_name_pattern WITH '%'.
    ENDIF.

    " Get objects with filters in SELECT
    IF lt_type_range IS NOT INITIAL AND lv_name_pattern IS NOT INITIAL.
      SELECT object, obj_name FROM tadir
        WHERE devclass = @lv_package
          AND object IN @lt_type_range
          AND obj_name LIKE @lv_name_pattern
        ORDER BY object, obj_name
        INTO TABLE @lt_objects
        UP TO @lv_limit ROWS
        OFFSET @lv_offset.
    ELSEIF lt_type_range IS NOT INITIAL.
      SELECT object, obj_name FROM tadir
        WHERE devclass = @lv_package
          AND object IN @lt_type_range
        ORDER BY object, obj_name
        INTO TABLE @lt_objects
        UP TO @lv_limit ROWS
        OFFSET @lv_offset.
    ELSEIF lv_name_pattern IS NOT INITIAL.
      SELECT object, obj_name FROM tadir
        WHERE devclass = @lv_package
          AND obj_name LIKE @lv_name_pattern
        ORDER BY object, obj_name
        INTO TABLE @lt_objects
        UP TO @lv_limit ROWS
        OFFSET @lv_offset.
    ELSE.
      SELECT object, obj_name FROM tadir
        WHERE devclass = @lv_package
        ORDER BY object, obj_name
        INTO TABLE @lt_objects
        UP TO @lv_limit ROWS
        OFFSET @lv_offset.
    ENDIF.

    " Get total count (without limit/offset)
    IF lt_type_range IS NOT INITIAL AND lv_name_pattern IS NOT INITIAL.
      SELECT COUNT( * ) FROM tadir
        INTO @rs_result-total
        WHERE devclass = @lv_package
          AND object IN @lt_type_range
          AND obj_name LIKE @lv_name_pattern.
    ELSEIF lt_type_range IS NOT INITIAL.
      SELECT COUNT( * ) FROM tadir
        INTO @rs_result-total
        WHERE devclass = @lv_package
          AND object IN @lt_type_range.
    ELSEIF lv_name_pattern IS NOT INITIAL.
      SELECT COUNT( * ) FROM tadir
        INTO @rs_result-total
        WHERE devclass = @lv_package
          AND obj_name LIKE @lv_name_pattern.
    ELSE.
      SELECT COUNT( * ) FROM tadir
        INTO @rs_result-total
        WHERE devclass = @lv_package.
    ENDIF.

    " Build BY_TYPE aggregation
    SORT lt_objects BY type.
    LOOP AT lt_objects INTO ls_object.
      AT NEW type.
        CLEAR ls_count.
        ls_count-type = ls_object-type.
        ls_count-count = 1.
      ENDAT.
      AT END OF type.
        APPEND ls_count TO lt_counts.
      ENDAT.
    ENDLOOP.

    " Build result
    rs_result-success = abap_true.
    rs_result-command = 'LIST'.
    rs_result-package = lv_package.
    rs_result-limit = lv_limit.
    rs_result-offset = lv_offset.
    rs_result-objects = lt_objects.
    rs_result-by_type = lt_counts.
  ENDMETHOD.

ENDCLASS.
