*"*"use source
*"*"Local Interface:
*"**********************************************************************
" TREE command implementation - displays package hierarchy tree
CLASS zcl_abgagt_command_tree DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_tree_params,
             package TYPE tdevc-devclass,
             depth TYPE i,
             include_objects TYPE abap_bool,
           END OF ty_tree_params.

    TYPES: BEGIN OF ty_object_type,
             object TYPE tadir-object,
             count TYPE i,
           END OF ty_object_type.

    TYPES ty_object_counts TYPE TABLE OF ty_object_type WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_subpackage,
             package TYPE tdevc-devclass,
             description TYPE string,
             depth TYPE i,
             object_count TYPE i,
             parent TYPE tdevc-devclass,
           END OF ty_subpackage.

    TYPES ty_subpackages TYPE TABLE OF ty_subpackage WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_hierarchy,
             package TYPE tdevc-devclass,
             description TYPE string,
             parent_package TYPE tdevc-devclass,
             parent_description TYPE string,
             subpackages TYPE ty_subpackages,
             object_count TYPE i,
             total_subpackages TYPE i,
             total_objects TYPE i,
           END OF ty_hierarchy.

    TYPES: BEGIN OF ty_summary,
             total_packages TYPE i,
             total_objects TYPE i,
             objects_by_type TYPE ty_object_counts,
           END OF ty_summary.

    TYPES: BEGIN OF ty_tree_result,
             success TYPE abap_bool,
             command TYPE string,
             package TYPE tdevc-devclass,
             message TYPE string,
             hierarchy TYPE ty_hierarchy,
             summary TYPE ty_summary,
             error TYPE string,
           END OF ty_tree_result.

    METHODS build_tree
      IMPORTING is_params TYPE ty_tree_params
      RETURNING VALUE(rs_result) TYPE ty_tree_result.

    METHODS get_object_count
      IMPORTING iv_package TYPE tdevc-devclass
      RETURNING VALUE(rv_count) TYPE i.

    METHODS get_object_types
      IMPORTING iv_package TYPE tdevc-devclass
      CHANGING ct_counts TYPE ty_object_counts.

ENDCLASS.

CLASS zcl_abgagt_command_tree IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_tree.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_tree_params,
          ls_result TYPE ty_tree_result.

    ls_result-command = zif_abgagt_command=>gc_tree.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-package IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error = 'Package is required'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    IF ls_params-depth IS INITIAL OR ls_params-depth <= 0.
      ls_params-depth = 3.
    ENDIF.

    IF ls_params-depth > 10.
      ls_params-depth = 10.
    ENDIF.

    ls_result = build_tree( ls_params ).
    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD build_tree.
    DATA: lv_package TYPE tdevc-devclass,
          lv_max_depth TYPE i,
          lv_total_objects TYPE i,
          lt_all_types TYPE ty_object_counts,
          ls_package TYPE tdevc.

    lv_package = is_params-package.
    lv_max_depth = is_params-depth.

    SELECT SINGLE devclass parentcl FROM tdevc
      INTO ls_package
      WHERE devclass = lv_package.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-package = lv_package.
      rs_result-error = |Package { lv_package } does not exist|.
      RETURN.
    ENDIF.

    rs_result-success = abap_true.
    rs_result-package = lv_package.
    rs_result-message = 'Tree retrieved successfully'.

    rs_result-hierarchy-package = lv_package.
    rs_result-hierarchy-description = lv_package.
    rs_result-hierarchy-parent_package = ls_package-parentcl.
    rs_result-hierarchy-parent_description = ls_package-parentcl.
    rs_result-hierarchy-object_count = get_object_count( lv_package ).

    rs_result-hierarchy-subpackages = get_subpackages(
      iv_parent = lv_package
      iv_current_depth = 1
      iv_max_depth = lv_max_depth
      iv_include_objects = is_params-include_objects
      cv_total_objects = lv_total_objects
      cv_total_subpackages = rs_result-hierarchy-total_subpackages ).

    rs_result-hierarchy-total_objects = lv_total_objects + rs_result-hierarchy-object_count.

    IF is_params-include_objects = abap_true.
      DATA lt_root_types TYPE ty_object_counts.
      get_object_types(
        EXPORTING iv_package = lv_package
        CHANGING ct_counts = lt_root_types ).
      LOOP AT lt_root_types INTO DATA(ls_root_obj).
        APPEND ls_root_obj TO lt_all_types.
      ENDLOOP.
    ENDIF.

    rs_result-summary-total_packages = rs_result-hierarchy-total_subpackages + 1.
    rs_result-summary-total_objects = rs_result-hierarchy-total_objects.
    rs_result-summary-objects_by_type = lt_all_types.
  ENDMETHOD.

  METHOD get_object_count.
    DATA lv_count TYPE i.
    SELECT COUNT(*) FROM tadir
      INTO lv_count
      WHERE devclass = iv_package
        AND object NOT IN ('DEVC', 'PACK').
    rv_count = lv_count.
  ENDMETHOD.

  METHOD get_object_types.
    DATA lt_types TYPE ty_object_counts.
    SELECT object COUNT(*) AS count FROM tadir
      INTO TABLE lt_types
      WHERE devclass = iv_package
        AND object NOT IN ('DEVC', 'PACK')
      GROUP BY object.
    ct_counts = lt_types.
  ENDMETHOD.

  METHOD get_subpackages.
    DATA: lt_result TYPE ty_subpackages,
          lv_sub_count TYPE i.

    SELECT devclass FROM tdevc
      INTO TABLE DATA(lt_direct_subs)
      WHERE parentcl = iv_parent
      ORDER BY devclass.

    LOOP AT lt_direct_subs INTO DATA(ls_direct).
      DATA ls_sub TYPE ty_subpackage.
      ls_sub-package = ls_direct-devclass.
      ls_sub-description = ls_direct-devclass.
      ls_sub-depth = iv_current_depth.
      ls_sub-parent = iv_parent.
      ls_sub-object_count = get_object_count( ls_direct-devclass ).

      APPEND ls_sub TO lt_result.
      cv_total_objects = cv_total_objects + ls_sub-object_count.
      lv_sub_count = lv_sub_count + 1.

      IF iv_current_depth < iv_max_depth.
        DATA lt_nested TYPE ty_subpackages.
        lt_nested = get_subpackages(
          EXPORTING iv_parent = ls_direct-devclass
                    iv_current_depth = iv_current_depth + 1
                    iv_max_depth = iv_max_depth
                    iv_include_objects = iv_include_objects
          IMPORTING cv_total_objects = cv_total_objects
                   cv_total_subpackages = cv_total_subpackages ).
        LOOP AT lt_nested INTO DATA(ls_nested).
          APPEND ls_nested TO lt_result.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    cv_total_subpackages = cv_total_subpackages + lv_sub_count.
    rt_subpackages = lt_result.
  ENDMETHOD.

ENDCLASS.
