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
             subpackages TYPE TABLE OF ty_subpackage WITH NON-UNIQUE DEFAULT KEY,
           END OF ty_subpackage.

    TYPES: BEGIN OF ty_tree_result,
             success TYPE abap_bool,
             command TYPE string,
             package TYPE string,
             message TYPE string,
             hierarchy_package TYPE string,
             hierarchy_description TYPE string,
             hierarchy_parent TYPE string,
             hierarchy_parent_desc TYPE string,
             hierarchy_depth TYPE i,
             hierarchy_object_count TYPE i,
             subpackages TYPE TABLE OF ty_subpackage WITH NON-UNIQUE DEFAULT KEY,
             total_packages TYPE i,
             total_objects TYPE i,
             objects TYPE ty_object_counts,
             error TYPE string,
           END OF ty_tree_result.

    METHODS build_tree
      IMPORTING is_params TYPE ty_tree_params
      RETURNING VALUE(rs_result) TYPE ty_tree_result.

    METHODS get_object_count
      IMPORTING iv_package TYPE string
      RETURNING VALUE(rv_count) TYPE i.

    METHODS get_object_counts_by_type
      IMPORTING iv_package TYPE string
      CHANGING ct_counts TYPE ty_object_counts.

    METHODS get_subpackages
      IMPORTING iv_parent TYPE string
                iv_current_depth TYPE i
                iv_max_depth TYPE i
                iv_include_objects TYPE abap_bool
      RETURNING VALUE(rt_subpackages) TYPE TABLE OF ty_subpackage.

    METHODS count_all_packages
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
    DATA: lv_package TYPE string,
          lv_max_depth TYPE i,
          lv_total_packages TYPE i,
          lv_total_objects TYPE i,
          lt_all_types TYPE ty_object_counts.

    lv_package = is_params-package.
    lv_max_depth = is_params-depth.

    SELECT SINGLE devclass, parent_pack, as4text
      FROM tdevc
      INTO @DATA(ls_package)
      WHERE devclass = @lv_package.

    IF sy-subrc <> 0.
      rs_result-success = abap_false.
      rs_result-package = lv_package.
      rs_result-error = |Package { lv_package } does not exist|.
      RETURN.
    ENDIF.

    rs_result-success = abap_true.
    rs_result-package = lv_package.
    rs_result-message = 'Tree retrieved successfully'.
    rs_result-hierarchy_package = lv_package.
    rs_result-hierarchy_description = ls_package-as4text.
    rs_result-hierarchy_depth = 0.

    IF ls_package-parent_pack IS NOT INITIAL.
      rs_result-hierarchy_parent = ls_package-parent_pack.
      SELECT SINGLE as4text FROM tdevc
        INTO @rs_result-hierarchy_parent_desc
        WHERE devclass = @ls_package-parent_pack.
    ENDIF.

    rs_result-hierarchy_object_count = get_object_count( lv_package ).

    IF is_params-include_objects = abap_true.
      get_object_counts_by_type(
        EXPORTING iv_package = lv_package
        CHANGING ct_counts = rs_result-objects ).
    ENDIF.

    rs_result-subpackages = get_subpackages(
      iv_parent = lv_package
      iv_current_depth = 1
      iv_max_depth = lv_max_depth
      iv_include_objects = is_params-include_objects ).

    lv_total_packages = 1.
    lv_total_objects = rs_result-hierarchy_object_count.

    LOOP AT rs_result-objects INTO DATA(ls_obj).
      APPEND ls_obj TO lt_all_types.
    ENDLOOP.

    count_all_packages(
      EXPORTING it_subpackages = rs_result-subpackages
      CHANGING cv_total_packages = lv_total_packages
               cv_total_objects = lv_total_objects
               ct_types = lt_all_types ).

    rs_result-total_packages = lv_total_packages.
    rs_result-total_objects = lv_total_objects.
    rs_result-objects = lt_all_types.
  ENDMETHOD.

  METHOD get_object_count.
    SELECT COUNT(*) FROM tadir
      INTO @DATA(lv_count)
      WHERE devclass = @iv_package
        AND object NOT IN ('DEVC', 'PACK').
    rv_count = lv_count.
  ENDMETHOD.

  METHOD get_object_counts_by_type.
    SELECT object, COUNT(*) AS count
      FROM tadir
      INTO TABLE @ct_counts
      WHERE devclass = @iv_package
        AND object NOT IN ('DEVC', 'PACK')
      GROUP BY object.
  ENDMETHOD.

  METHOD get_subpackages.
    DATA: lt_result TYPE TABLE OF ty_subpackage.

    SELECT devclass, as4text
      FROM tdevc
      INTO TABLE @DATA(lt_direct_subs)
      WHERE parent_pack = @iv_parent
      ORDER BY devclass.

    LOOP AT lt_direct_subs INTO DATA(ls_direct).
      DATA ls_sub TYPE ty_subpackage.
      ls_sub-package = ls_direct-devclass.
      ls_sub-description = ls_direct-as4text.
      ls_sub-depth = iv_current_depth.
      ls_sub-object_count = get_object_count( ls_direct-devclass ).

      IF iv_current_depth < iv_max_depth.
        ls_sub-subpackages = get_subpackages(
          iv_parent = ls_direct-devclass
          iv_current_depth = iv_current_depth + 1
          iv_max_depth = iv_max_depth
          iv_include_objects = iv_include_objects ).
      ENDIF.

      APPEND ls_sub TO lt_result.
    ENDLOOP.

    rt_subpackages = lt_result.
  ENDMETHOD.

  METHOD count_all_packages.
    LOOP AT it_subpackages INTO DATA(ls_sub).
      cv_total_packages = cv_total_packages + 1.
      cv_total_objects = cv_total_objects + ls_sub-object_count.

      IF ls_sub-subpackages IS NOT INITIAL.
        count_all_packages(
          EXPORTING it_subpackages = ls_sub-subpackages
          CHANGING cv_total_packages = cv_total_packages
                   cv_total_objects = cv_total_objects
                   ct_types = ct_types ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
