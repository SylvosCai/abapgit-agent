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

    TYPES: BEGIN OF ty_package_node,
             package TYPE string,
             parent TYPE string,
             description TYPE string,
             depth TYPE i,
             object_count TYPE i,
           END OF ty_package_node.

    TYPES ty_package_nodes TYPE TABLE OF ty_package_node WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_object_count,
             object TYPE string,
             count TYPE i,
           END OF ty_object_count.

    TYPES ty_object_counts TYPE TABLE OF ty_object_count WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_tree_result,
             success TYPE abap_bool,
             command TYPE string,
             package TYPE string,
             message TYPE string,
             nodes TYPE ty_package_nodes,
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

    METHODS get_package_description
      IMPORTING iv_package TYPE string
      RETURNING VALUE(rv_description) TYPE string.

    METHODS get_object_counts_by_type
      IMPORTING iv_package TYPE string
      CHANGING ct_counts TYPE ty_object_counts.

    METHODS collect_subpackages
      IMPORTING iv_parent TYPE string
                iv_current_depth TYPE i
                iv_max_depth TYPE i
                iv_include_objects TYPE abap_bool
      CHANGING ct_nodes TYPE ty_package_nodes
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
          lv_total_objects TYPE i,
          lt_all_types TYPE ty_object_counts,
          lt_nodes TYPE ty_package_nodes,
          ls_package TYPE tdevc,
          ls_root TYPE ty_package_node.

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

    " Add root package
    ls_root-package = lv_package.
    ls_root-parent = ls_package-parentcl.
    ls_root-description = get_package_description( lv_package ).
    ls_root-depth = 0.
    ls_root-object_count = get_object_count( lv_package ).
    APPEND ls_root TO lt_nodes.

    lv_total_objects = ls_root-object_count.

    " Add object types for root
    IF is_params-include_objects = abap_true.
      get_object_counts_by_type(
        EXPORTING iv_package = lv_package
        CHANGING ct_counts = rs_result-objects ).
      LOOP AT rs_result-objects INTO DATA(ls_obj).
        APPEND ls_obj TO lt_all_types.
      ENDLOOP.
    ENDIF.

    " Add all subpackages
    collect_subpackages(
      EXPORTING iv_parent = lv_package
                iv_current_depth = 1
                iv_max_depth = lv_max_depth
                iv_include_objects = is_params-include_objects
      CHANGING ct_nodes = lt_nodes
               cv_total_objects = lv_total_objects
               ct_types = lt_all_types ).

    rs_result-nodes = lt_nodes.
    rs_result-total_packages = lines( lt_nodes ).
    rs_result-total_objects = lv_total_objects.
    rs_result-objects = lt_all_types.
  ENDMETHOD.

  METHOD get_package_description.
    DATA: lv_desc TYPE string.

    SELECT SINGLE as4text FROM tdevc
      INTO lv_desc
      WHERE devclass = iv_package.
    IF lv_desc IS INITIAL.
      lv_desc = iv_package.
    ENDIF.
    rv_description = lv_desc.
  ENDMETHOD.

  METHOD get_object_count.
    DATA lv_count TYPE i.
    SELECT COUNT(*) FROM tadir
      INTO lv_count
      WHERE devclass = iv_package
        AND object NOT IN ('DEVC', 'PACK').
    rv_count = lv_count.
  ENDMETHOD.

  METHOD get_object_counts_by_type.
    DATA lt_counts TYPE ty_object_counts.
    SELECT object COUNT(*) AS count FROM tadir
      INTO TABLE lt_counts
      WHERE devclass = iv_package
        AND object NOT IN ('DEVC', 'PACK')
      GROUP BY object.
    ct_counts = lt_counts.
  ENDMETHOD.

  METHOD collect_subpackages.
    DATA: lt_direct_subs TYPE TABLE OF tdevc,
          ls_direct TYPE tdevc,
          ls_node TYPE ty_package_node.

    SELECT devclass parentcl FROM tdevc
      INTO TABLE lt_direct_subs
      WHERE parentcl = iv_parent
      ORDER BY devclass.

    LOOP AT lt_direct_subs INTO ls_direct.
      CLEAR ls_node.
      ls_node-package = ls_direct-devclass.
      ls_node-parent = ls_direct-parentcl.
      ls_node-description = get_package_description( ls_direct-devclass ).
      ls_node-depth = iv_current_depth.
      ls_node-object_count = get_object_count( ls_direct-devclass ).

      APPEND ls_node TO ct_nodes.
      cv_total_objects = cv_total_objects + ls_node-object_count.

      " Add object types
      IF iv_include_objects = abap_true.
        DATA lt_types TYPE ty_object_counts.
        get_object_counts_by_type(
          EXPORTING iv_package = ls_direct-devclass
          CHANGING ct_counts = lt_types ).
        LOOP AT lt_types INTO DATA(ls_type).
          READ TABLE ct_types WITH KEY object = ls_type-object
            ASSIGNING FIELD-SYMBOL(<ls_existing>).
          IF sy-subrc = 0.
            <ls_existing>-count = <ls_existing>-count + ls_type-count.
          ELSE.
            APPEND ls_type TO ct_types.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF iv_current_depth < iv_max_depth.
        collect_subpackages(
          EXPORTING iv_parent = ls_direct-devclass
                    iv_current_depth = iv_current_depth + 1
                    iv_max_depth = iv_max_depth
                    iv_include_objects = iv_include_objects
          CHANGING ct_nodes = ct_nodes
                   cv_total_objects = cv_total_objects
                   ct_types = ct_types ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
