"! <p class="shorttext synchronized">Syntax Checker - Syntax Statement Implementation</p>
"! Checks ABAP source code syntax using SYNTAX-CHECK statement only.
"! This is an in-memory approach that doesn't write to any includes.
CLASS zcl_abgagt_syntax_chk_stmt DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

  PRIVATE SECTION.

    "! Build class skeleton for SYNTAX-CHECK statement
    METHODS build_class_skeleton
      IMPORTING iv_class_name      TYPE seoclsname
                it_source          TYPE string_table
      RETURNING VALUE(rt_skeleton) TYPE string_table.

ENDCLASS.


CLASS zcl_abgagt_syntax_chk_stmt IMPLEMENTATION.

  METHOD zif_abgagt_syntax_checker~check_class.
    DATA: lv_msg       TYPE string,
          lv_line      TYPE i,
          lv_word      TYPE string,
          ls_dir       TYPE trdir,
          lt_skeleton  TYPE string_table,
          lv_classpool TYPE syrepid.

    rs_result-object_type = 'CLAS'.
    rs_result-object_name = iv_class_name.

    " Build class skeleton
    lt_skeleton = build_class_skeleton(
      iv_class_name = iv_class_name
      it_source     = it_source ).

    IF lt_skeleton IS INITIAL.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line = 1
        text = 'Failed to build class skeleton for syntax check' ) ).
      rs_result-message = 'Failed to build class skeleton'.
      RETURN.
    ENDIF.

    " Try to get TRDIR entry from existing class (for context)
    lv_classpool = cl_oo_classname_service=>get_classpool_name( iv_class_name ).
    SELECT SINGLE * FROM trdir INTO ls_dir WHERE name = lv_classpool.
    IF sy-subrc <> 0.
      " Class doesn't exist - use default settings
      ls_dir-name = lv_classpool.
      ls_dir-subc = 'K'.  " Class pool
      ls_dir-uccheck = 'X'.
    ENDIF.

    " Run SYNTAX-CHECK statement on skeleton
    SYNTAX-CHECK FOR lt_skeleton
      MESSAGE lv_msg
      LINE lv_line
      WORD lv_word
      DIRECTORY ENTRY ls_dir.

    IF sy-subrc = 0.
      rs_result-success = abap_true.
      rs_result-error_count = 0.
      rs_result-message = 'Syntax check passed'.
    ELSE.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line = lv_line
        text = lv_msg
        word = lv_word ) ).
      rs_result-message = lv_msg.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abgagt_syntax_checker~check_class_with_locals.
    DATA: ls_dir         TYPE trdir,
          lv_msg         TYPE string,
          lv_line        TYPE i,
          lv_word        TYPE string,
          lv_classpool   TYPE syrepid,
          lt_skeleton    TYPE string_table.

    rs_result-object_type = 'CLAS'.
    rs_result-object_name = iv_class_name.

    " Build combined source: CLASS-POOL + locals_def + main source + locals_imp
    APPEND 'CLASS-POOL.' TO lt_skeleton.

    " Add local class definitions first (they need to be defined before used)
    IF it_locals_def IS NOT INITIAL.
      APPEND LINES OF it_locals_def TO lt_skeleton.
    ENDIF.

    " Add main class source
    APPEND LINES OF it_source TO lt_skeleton.

    " Add local class implementations
    IF it_locals_imp IS NOT INITIAL.
      APPEND LINES OF it_locals_imp TO lt_skeleton.
    ENDIF.

    IF lt_skeleton IS INITIAL OR lines( lt_skeleton ) <= 1.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line = 1
        text = 'No source provided for syntax check' ) ).
      rs_result-message = 'No source provided'.
      RETURN.
    ENDIF.

    " Get TRDIR entry for class pool (for context)
    lv_classpool = cl_oo_classname_service=>get_classpool_name( iv_class_name ).
    SELECT SINGLE * FROM trdir INTO ls_dir WHERE name = lv_classpool.
    IF sy-subrc <> 0.
      " Class doesn't exist - use default
      ls_dir-name = lv_classpool.
      ls_dir-subc = 'K'.  " Class pool
      ls_dir-uccheck = 'X'.
    ENDIF.

    " Run syntax check on the combined source
    SYNTAX-CHECK FOR lt_skeleton
      MESSAGE lv_msg
      LINE lv_line
      WORD lv_word
      DIRECTORY ENTRY ls_dir.

    IF sy-subrc = 0.
      rs_result-success = abap_true.
      rs_result-error_count = 0.
      rs_result-message = 'Syntax check passed'.
    ELSE.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line   = lv_line
        text   = lv_msg
        word   = lv_word ) ).
      rs_result-message = lv_msg.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abgagt_syntax_checker~check_interface.
    DATA: ls_dir        TYPE trdir,
          lv_msg        TYPE string,
          lv_line       TYPE i,
          lv_word       TYPE string,
          lv_intfpool   TYPE syrepid,
          lt_skeleton   TYPE string_table.

    rs_result-object_type = 'INTF'.
    rs_result-object_name = iv_intf_name.

    " Build interface skeleton from original source
    " Start with INTERFACE-POOL statement, then append source
    APPEND 'INTERFACE-POOL.' TO lt_skeleton.
    APPEND LINES OF it_source TO lt_skeleton.

    " Get TRDIR entry for interface pool (for context)
    lv_intfpool = cl_oo_classname_service=>get_interfacepool_name( iv_intf_name ).
    SELECT SINGLE * FROM trdir INTO ls_dir WHERE name = lv_intfpool.
    IF sy-subrc <> 0.
      ls_dir-name = lv_intfpool.
      ls_dir-subc = 'J'.  " Interface pool
      ls_dir-uccheck = 'X'.
    ENDIF.

    " Run syntax check on the complete source
    SYNTAX-CHECK FOR lt_skeleton
      MESSAGE lv_msg
      LINE lv_line
      WORD lv_word
      DIRECTORY ENTRY ls_dir.

    IF sy-subrc = 0.
      rs_result-success = abap_true.
      rs_result-error_count = 0.
      rs_result-message = 'Syntax check passed'.
    ELSE.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line   = lv_line
        text   = lv_msg
        word   = lv_word ) ).
      rs_result-message = lv_msg.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abgagt_syntax_checker~check_program.
    DATA: lv_msg  TYPE string,
          lv_line TYPE i,
          lv_word TYPE string,
          ls_dir  TYPE trdir.

    rs_result-object_type = 'PROG'.
    rs_result-object_name = iv_program_name.

    " Set TRDIR entry for syntax check context
    ls_dir-name = iv_program_name.
    ls_dir-uccheck = iv_uccheck.

    " Run SYNTAX-CHECK statement
    SYNTAX-CHECK FOR it_source
      MESSAGE lv_msg
      LINE lv_line
      WORD lv_word
      DIRECTORY ENTRY ls_dir.

    IF sy-subrc = 0.
      rs_result-success = abap_true.
      rs_result-error_count = 0.
      rs_result-message = 'Syntax check passed'.
    ELSE.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line = lv_line
        text = lv_msg
        word = lv_word ) ).
      rs_result-message = lv_msg.
    ENDIF.
  ENDMETHOD.


  METHOD build_class_skeleton.
    " Build a class skeleton that can be used with SYNTAX-CHECK statement
    " CLASS-POOL statement
    APPEND 'CLASS-POOL.' TO rt_skeleton.

    " Include the source directly (assumes it's a complete class definition + implementation)
    APPEND LINES OF it_source TO rt_skeleton.
  ENDMETHOD.

ENDCLASS.
