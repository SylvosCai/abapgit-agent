*"*"use source
*"*"Local Interface:
*"**********************************************************************
" TREE command implementation - displays package hierarchy tree
CLASS zcl_abgagt_command_tree DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_tree_params,
             package TYPE string,
             depth TYPE i,
             include_objects TYPE abap_bool,
           END OF ty_tree_params.

    TYPES: BEGIN OF ty_object_count,
             object TYPE string,
             count TYPE i,
           END OF ty_object_count.

    TYPES ty_object_counts TYPE TABLE OF ty_object_count WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_subpackage,
             package TYPE string,
             description TYPE string,
             depth TYPE i,
             object_count TYPE i,
             objects TYPE ty_object_counts,
             subpackages TYPE TABLE OF ty_subpackage WITH NON-UNIQUE DEFAULT KEY,
           END OF ty_subpackage.

    TYPES: BEGIN OF ty_tree_result,
             success TYPE abap_bool,
             command TYPE string,
             package TYPE string,
             message TYPE string,
             hierarchy TYPE ty_subpackage,
             summary TYPE BEGIN OF ty_summary,
               total_packages TYPE i,
               total_objects TYPE i,
               objects_by_type TYPE ty_object_counts,
             END OF ty_summary,
             error TYPE string,
           END OF ty_tree_result.

    METHODS build_tree
      IMPORTING is_params TYPE ty_tree_params
      RETURNING VALUE(rs_result) TYPE ty_tree_result.

    METHODS get_subpackages
      IMPORTING iv_parent TYPE string
                iv_current_depth TYPE i
                iv_max_depth TYPE i
                iv_include_objects TYPE abap_bool
      RETURNING VALUE(rt_subpackages) TYPE TABLE OF ty_subpackage.

    METHODS get_object_count
      IMPORTING iv_package TYPE string
                iv_include_details TYPE abap_bool
      EXPORTING rv_count TYPE i
                rt_counts TYPE ty_object_counts.

    METHODS process_all_subpackages
      IMPORTING it_subpackages TYPE TABLE OF ty_subpackage
      CHANGING cv_total_packages TYPE i
               cv_total_objects TYPE i
               ct_types TYPE ty_object_counts.

ENDCLASS.

CLASS zcl_abgagt_command_tree IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_tree.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_tree_params,
          ls_result TYPE ty_tree_result.

    ls_result-command = zif_abgagt_command=>gc_tree.

    " Parse parameters from is_param
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

    " Set default depth
    IF ls_params-depth IS INITIAL OR ls_params-depth <= 0.
      ls_params-depth = 3.
    ENDIF.

    " Limit max depth
    IF ls_params-depth > 10.
      ls_params-depth = 10.
    ENDIF.

    " Execute tree query
    ls_result = build_tree( ls_params ).

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD build_tree.
    DATA: lv_package TYPE string,
          lv_max_depth TYPE i,
          lv_total_packages TYPE i,
          lv_total_objects TYPE i,
          lt_all_types TYPE ty_object_counts,
          lt_subpackages TYPE TABLE OF ty_subpackage.

    lv_package = is_params-package.
    lv_max_depth = is_params-depth.

    " Get package info
    SELECT SINGLE devclass, parent_pack, as4text
      FROM tdevc
      INTO @DATA(ls_package)
      WHERE devclass = @lv_package.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-package = lv_package.
      rs_result-error = |Package { lv_package } does not exist in the system|.
      RETURN.
    ENDIF.

    " Build hierarchy starting from root
    rs_result-success = abap_true.
    rs_result-package = ls_package-devclass.
    rs_result-message = 'Tree retrieved successfully'.
    rs_result-hierarchy-package = ls_package-devclass.
    rs_result-hierarchy-description = ls_package-as4text.
    rs_result-hierarchy-depth = 0.

    " Get parent info
    IF ls_package-parent_pack IS NOT INITIAL.
      SELECT SINGLE as4text
        FROM tdevc
        INTO @rs_result-hierarchy-parent-description
        WHERE devclass = @ls_package-parent_pack.
    ENDIF.

    " Get subpackages recursively
    rs_result-hierarchy-subpackages = get_subpackages(
      iv_parent = lv_package
      iv_current_depth = 1
      iv_max_depth = lv_max_depth
      iv_include_objects = is_params-include_objects ).

    " Get object counts for root
    get_object_count(
      EXPORTING iv_package = lv_package
                iv_include_details = is_params-include_objects
      IMPORTING rv_count = rs_result-hierarchy-object_count
                rt_counts = rs_result-hierarchy-objects ).

    " Calculate summary
    lv_total_packages = 1.
    lv_total_objects = rs_result-hierarchy-object_count.

    " Add object types from root
    LOOP AT rs_result-hierarchy-objects INTO DATA(ls_root_obj).
      APPEND ls_root_obj TO lt_all_types.
    ENDLOOP.

    " Process all subpackages recursively
    process_all_subpackages(
      EXPORTING it_subpackages = rs_result-hierarchy-subpackages
      CHANGING cv_total_packages = lv_total_packages
               cv_total_objects = lv_total_objects
               ct_types = lt_all_types ).

    " Set summary
    rs_result-summary-total_packages = lv_total_packages.
    rs_result-summary-total_objects = lv_total_objects.
    rs_result-summary-objects_by_type = lt_all_types.

  ENDMETHOD.

  METHOD get_subpackages.
    DATA: lt_result TYPE TABLE OF ty_subpackage.

    " Get direct subpackages
    SELECT devclass, as4text
      FROM tdevc
      INTO TABLE @DATA(lt_direct_subs)
      WHERE parent_pack = @iv_parent
      ORDER BY devclass.

    LOOP AT lt_direct_subs INTO DATA(ls_direct).
      DATA ls_subpackage TYPE ty_subpackage.
      ls_subpackage-package = ls_direct-devclass.
      ls_subpackage-description = ls_direct-as4text.
      ls_subpackage-depth = iv_current_depth.

      " Get object count for subpackage
      get_object_count(
        EXPORTING iv_package = ls_direct-devclass
                  iv_include_details = iv_include_objects
        IMPORTING rv_count = ls_subpackage-object_count
                  rt_counts = ls_subpackage-objects ).

      " Recursively get subpackages if not at max depth
      IF iv_current_depth < iv_max_depth.
        ls_subpackage-subpackages = get_subpackages(
          iv_parent = ls_direct-devclass
          iv_current_depth = iv_current_depth + 1
          iv_max_depth = iv_max_depth
          iv_include_objects = iv_include_objects ).
      ENDIF.

      APPEND ls_subpackage TO lt_result.
    ENDLOOP.

    rt_subpackages = lt_result.
  ENDMETHOD.

  METHOD get_object_count.
    DATA: lv_count TYPE i.

    " Get total object count
    SELECT COUNT(*)
      FROM tadir
      INTO @lv_count
      WHERE devclass = @iv_package
        AND object NOT IN ('DEVC', 'PACK').

    rv_count = lv_count.

    " Get breakdown by type if requested
    IF iv_include_details = abap_true.
      SELECT object, COUNT(*) AS count
        FROM tadir
        INTO TABLE @rt_counts
        WHERE devclass = @iv_package
          AND object NOT IN ('DEVC', 'PACK')
        GROUP BY object.
    ENDIF.
  ENDMETHOD.

  METHOD process_all_subpackages.
    LOOP AT it_subpackages INTO DATA(ls_sub).
      cv_total_packages = cv_total_packages + 1.
      cv_total_objects = cv_total_objects + ls_sub-object_count.

      " Add object types
      LOOP AT ls_sub-objects INTO DATA(ls_obj).
        READ TABLE ct_types WITH KEY object = ls_obj-object
          ASSIGNING FIELD-SYMBOL(<ls_type>).
        IF sy-subrc = 0.
          <ls_type>-count = <ls_type>-count + ls_obj-count.
        ELSE.
          APPEND ls_obj TO ct_types.
        ENDIF.
      ENDLOOP.

      " Process nested subpackages
      IF ls_sub-subpackages IS NOT INITIAL.
        process_all_subpackages(
          EXPORTING it_subpackages = ls_sub-subpackages
          CHANGING cv_total_packages = cv_total_packages
                   cv_total_objects = cv_total_objects
                   ct_types = ct_types ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
