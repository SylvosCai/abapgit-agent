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
             type TYPE tadir-object,
             name TYPE tadir-obj_name,
           END OF ty_object.

    TYPES ty_objects TYPE TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_type_count,
             type TYPE tadir-object,
             count TYPE i,
           END OF ty_type_count.

    TYPES ty_type_counts TYPE TABLE OF ty_type_count WITH NON-UNIQUE DEFAULT KEY.

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

    " Supported object types
    CONSTANTS:
      gc_type_clas TYPE string VALUE 'CLAS',
      gc_type_intf TYPE string VALUE 'INTF',
      gc_type_prog TYPE string VALUE 'PROG',
      gc_type_fugr TYPE string VALUE 'FUGR',
      gc_type_tabl TYPE string VALUE 'TABL',
      gc_type_stru TYPE string VALUE 'STRU',
      gc_type_dtel TYPE string VALUE 'DTEL',
      gc_type_ttyp TYPE string VALUE 'TTYP',
      gc_type_ddls TYPE string VALUE 'DDLS',
      gc_type_ddlx TYPE string VALUE 'DDLX'.

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

    " Validate object type if provided
    IF ls_params-type IS NOT INITIAL AND validate_type( ls_params-type ) = abap_false.
      ls_result-success = abap_false.
      ls_result-error = |Invalid object type: { ls_params-type }|.
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
    " Validate each type in the comma-separated list
    DATA lt_type_strings TYPE STANDARD TABLE OF string.
    DATA lv_type_str TYPE string.
    DATA lv_type TYPE string.

    SPLIT to_upper( iv_type ) AT ',' INTO TABLE lt_type_strings.

    LOOP AT lt_type_strings INTO lv_type_str.
      " Use CONV for string conversion and remove spaces manually
      lv_type = lv_type_str.
      CONDENSE lv_type.

      " Check if type is valid
      IF lv_type <> gc_type_clas AND
         lv_type <> gc_type_intf AND
         lv_type <> gc_type_prog AND
         lv_type <> gc_type_fugr AND
         lv_type <> gc_type_tabl AND
         lv_type <> gc_type_stru AND
         lv_type <> gc_type_dtel AND
         lv_type <> gc_type_ttyp AND
         lv_type <> gc_type_ddls AND
         lv_type <> gc_type_ddlx.
        rv_valid = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD execute_list.
    DATA: lt_objects TYPE ty_objects,
          lt_counts TYPE ty_type_counts,
          lv_package TYPE tdevc-devclass,
          lv_name_pattern TYPE tadir-obj_name,
          lv_type_filter TYPE string,
          lt_types TYPE STANDARD TABLE OF tadir-object,
          lt_type_strings TYPE STANDARD TABLE OF string,
          lv_type_str TYPE string,
          lv_type_trimmed TYPE tadir-object.

    lv_package = is_params-package.

    " Parse type filter and build range table
    IF is_params-type IS NOT INITIAL.
      SPLIT to_upper( is_params-type ) AT ',' INTO TABLE lt_type_strings.
      LOOP AT lt_type_strings INTO lv_type_str.
        lv_type_trimmed = lv_type_str.
        CONDENSE lv_type_trimmed.
        IF lv_type_trimmed IS NOT INITIAL.
          APPEND lv_type_trimmed TO lt_types.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Check if type filter is active
    DATA(lv_has_type_filter) = boolc( lines( lt_types ) > 0 ).

    " Convert name pattern for LIKE
    IF is_params-name IS NOT INITIAL.
      lv_name_pattern = is_params-name.
      " Convert * to % for SQL LIKE
      REPLACE ALL OCCURRENCES OF '*' IN lv_name_pattern WITH '%'.
    ENDIF.

    " Get objects with filters
    IF lv_has_type_filter = abap_true AND lv_name_pattern IS NOT INITIAL.
      SELECT object obj_name FROM tadir
        INTO CORRESPONDING FIELDS OF TABLE lt_objects
        WHERE devclass = lv_package
          AND object IN ( lt_types )
          AND obj_name LIKE lv_name_pattern
        ORDER BY object obj_name
        LIMIT is_params-limit
        OFFSET is_params-offset.
    ELSEIF lv_has_type_filter = abap_true.
      SELECT object obj_name FROM tadir
        INTO CORRESPONDING FIELDS OF TABLE lt_objects
        WHERE devclass = lv_package
          AND object IN ( lt_types )
        ORDER BY object obj_name
        LIMIT is_params-limit
        OFFSET is_params-offset.
    ELSEIF lv_name_pattern IS NOT INITIAL.
      SELECT object obj_name FROM tadir
        INTO CORRESPONDING FIELDS OF TABLE lt_objects
        WHERE devclass = lv_package
          AND obj_name LIKE lv_name_pattern
        ORDER BY object obj_name
        LIMIT is_params-limit
        OFFSET is_params-offset.
    ELSE.
      SELECT object obj_name FROM tadir
        INTO CORRESPONDING FIELDS OF TABLE lt_objects
        WHERE devclass = lv_package
        ORDER BY object obj_name
        LIMIT is_params-limit
        OFFSET is_params-offset.
    ENDIF.

    " Get total count for pagination
    IF lv_has_type_filter = abap_true AND lv_name_pattern IS NOT INITIAL.
      SELECT COUNT( * ) FROM tadir
        INTO rs_result-total
        WHERE devclass = lv_package
          AND object IN ( lt_types )
          AND obj_name LIKE lv_name_pattern.
    ELSEIF lv_has_type_filter = abap_true.
      SELECT COUNT( * ) FROM tadir
        INTO rs_result-total
        WHERE devclass = lv_package
          AND object IN ( lt_types ).
    ELSEIF lv_name_pattern IS NOT INITIAL.
      SELECT COUNT( * ) FROM tadir
        INTO rs_result-total
        WHERE devclass = lv_package
          AND obj_name LIKE lv_name_pattern.
    ELSE.
      SELECT COUNT( * ) FROM tadir
        INTO rs_result-total
        WHERE devclass = lv_package.
    ENDIF.

    rs_result-success = abap_true.
    rs_result-command = 'LIST'.
    rs_result-package = lv_package.
    rs_result-limit = is_params-limit.
    rs_result-offset = is_params-offset.
    rs_result-objects = lt_objects.

    " Get counts by type
    IF lv_has_type_filter = abap_true AND lv_name_pattern IS NOT INITIAL.
      SELECT object COUNT( * ) AS count FROM tadir
        INTO TABLE lt_counts
        WHERE devclass = lv_package
          AND object IN ( lt_types )
          AND obj_name LIKE lv_name_pattern
        GROUP BY object
        ORDER BY object.
    ELSEIF lv_has_type_filter = abap_true.
      SELECT object COUNT( * ) AS count FROM tadir
        INTO TABLE lt_counts
        WHERE devclass = lv_package
          AND object IN ( lt_types )
        GROUP BY object
        ORDER BY object.
    ELSEIF lv_name_pattern IS NOT INITIAL.
      SELECT object COUNT( * ) AS count FROM tadir
        INTO TABLE lt_counts
        WHERE devclass = lv_package
          AND obj_name LIKE lv_name_pattern
        GROUP BY object
        ORDER BY object.
    ELSE.
      SELECT object COUNT( * ) AS count FROM tadir
        INTO TABLE lt_counts
        WHERE devclass = lv_package
        GROUP BY object
        ORDER BY object.
    ENDIF.

    rs_result-by_type = lt_counts.
  ENDMETHOD.

ENDCLASS.
