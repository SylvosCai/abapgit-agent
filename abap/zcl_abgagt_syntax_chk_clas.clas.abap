"! <p class="shorttext synchronized">Syntax Checker for Classes</p>
"! Checks ABAP class source code syntax using SYNTAX-CHECK statement.
"! Supports checking classes with local class definitions and implementations.
CLASS zcl_abgagt_syntax_chk_clas DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

    "! Set local class definitions source (CCDEF)
    "! @parameter it_locals_def | Local class definitions
    METHODS set_locals_def
      IMPORTING it_locals_def TYPE string_table.

    "! Set local class implementations source (CCIMP)
    "! @parameter it_locals_imp | Local class implementations
    METHODS set_locals_imp
      IMPORTING it_locals_imp TYPE string_table.

    "! Clear local class sources
    METHODS clear_locals.

  PRIVATE SECTION.
    DATA mt_locals_def TYPE string_table.
    DATA mt_locals_imp TYPE string_table.

    "! Run syntax check on skeleton
    METHODS run_check
      IMPORTING iv_class_name    TYPE seoclsname
                it_skeleton      TYPE string_table
      RETURNING VALUE(rs_result) TYPE zif_abgagt_syntax_checker=>ty_result.

ENDCLASS.


CLASS zcl_abgagt_syntax_chk_clas IMPLEMENTATION.

  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'CLAS'.
  ENDMETHOD.


  METHOD zif_abgagt_syntax_checker~check.
    DATA: lt_skeleton  TYPE string_table,
          lv_classname TYPE seoclsname.

    lv_classname = to_upper( iv_name ).

    " Build skeleton: CLASS-POOL + locals_def + main source + locals_imp
    APPEND 'CLASS-POOL.' TO lt_skeleton.

    " Add local class definitions first (they need to be defined before used)
    IF mt_locals_def IS NOT INITIAL.
      APPEND LINES OF mt_locals_def TO lt_skeleton.
    ENDIF.

    " Add main class source
    APPEND LINES OF it_source TO lt_skeleton.

    " Add local class implementations
    IF mt_locals_imp IS NOT INITIAL.
      APPEND LINES OF mt_locals_imp TO lt_skeleton.
    ENDIF.

    " Run syntax check
    rs_result = run_check(
      iv_class_name = lv_classname
      it_skeleton   = lt_skeleton ).
  ENDMETHOD.


  METHOD set_locals_def.
    mt_locals_def = it_locals_def.
  ENDMETHOD.


  METHOD set_locals_imp.
    mt_locals_imp = it_locals_imp.
  ENDMETHOD.


  METHOD clear_locals.
    CLEAR: mt_locals_def, mt_locals_imp.
  ENDMETHOD.


  METHOD run_check.
    DATA: ls_dir       TYPE trdir,
          lv_msg       TYPE string,
          lv_line      TYPE i,
          lv_word      TYPE string,
          lv_classpool TYPE syrepid.

    rs_result-object_type = 'CLAS'.
    rs_result-object_name = iv_class_name.

    IF it_skeleton IS INITIAL OR lines( it_skeleton ) <= 1.
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

    " Run syntax check
    SYNTAX-CHECK FOR it_skeleton
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

ENDCLASS.
