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

    "! Set test classes source (CCAU)
    "! @parameter it_testclasses | Test classes (definitions + implementations)
    METHODS set_testclasses
      IMPORTING it_testclasses TYPE string_table.

    "! Set FIXPT flag from XML metadata
    "! @parameter iv_fixpt | FIXPT flag ('X' or blank)
    METHODS set_fixpt
      IMPORTING iv_fixpt TYPE string.

    "! Clear local class sources
    METHODS clear_locals.

  PRIVATE SECTION.
    DATA mt_locals_def TYPE string_table.
    DATA mt_locals_imp TYPE string_table.
    DATA mt_testclasses TYPE string_table.
    DATA mv_fixpt TYPE string.

    "! Run syntax check on skeleton
    METHODS run_check
      IMPORTING iv_class_name      TYPE seoclsname
                it_skeleton        TYPE string_table
                iv_prepended_lines TYPE i DEFAULT 1
                iv_main_lines      TYPE i DEFAULT 0
                iv_locals_imp_lines TYPE i DEFAULT 0
      RETURNING VALUE(rs_result)   TYPE zif_abgagt_syntax_checker=>ty_result.

ENDCLASS.


CLASS zcl_abgagt_syntax_chk_clas IMPLEMENTATION.

  METHOD zif_abgagt_syntax_checker~get_object_type.
    rv_type = 'CLAS'.
  ENDMETHOD.


  METHOD zif_abgagt_syntax_checker~check.
    DATA: lt_skeleton  TYPE string_table,
          lv_classname TYPE seoclsname,
          lv_prepended TYPE i,
          lv_main_lines TYPE i,
          lv_locals_imp_lines TYPE i.

    lv_classname = to_upper( iv_name ).

    " Build skeleton: CLASS-POOL + locals_def + main source + locals_imp + testclasses
    APPEND 'CLASS-POOL.' TO lt_skeleton.
    lv_prepended = 1.  " CLASS-POOL. line

    " Add local class definitions first (they need to be defined before used)
    IF mt_locals_def IS NOT INITIAL.
      lv_prepended = lv_prepended + lines( mt_locals_def ).
      APPEND LINES OF mt_locals_def TO lt_skeleton.
    ENDIF.

    " Add main class source
    lv_main_lines = lines( it_source ).
    APPEND LINES OF it_source TO lt_skeleton.

    " Add local class implementations
    IF mt_locals_imp IS NOT INITIAL.
      lv_locals_imp_lines = lines( mt_locals_imp ).
      APPEND LINES OF mt_locals_imp TO lt_skeleton.
    ENDIF.

    " Add test classes (CCAU - contains both definition and implementation)
    IF mt_testclasses IS NOT INITIAL.
      APPEND LINES OF mt_testclasses TO lt_skeleton.
    ENDIF.

    " Run syntax check
    rs_result = run_check(
      iv_class_name       = lv_classname
      it_skeleton         = lt_skeleton
      iv_prepended_lines  = lv_prepended
      iv_main_lines       = lv_main_lines
      iv_locals_imp_lines = lv_locals_imp_lines ).
  ENDMETHOD.


  METHOD set_locals_def.
    mt_locals_def = it_locals_def.
  ENDMETHOD.


  METHOD set_locals_imp.
    mt_locals_imp = it_locals_imp.
  ENDMETHOD.


  METHOD set_testclasses.
    mt_testclasses = it_testclasses.
  ENDMETHOD.


  METHOD set_fixpt.
    mv_fixpt = iv_fixpt.
  ENDMETHOD.


  METHOD clear_locals.
    CLEAR: mt_locals_def, mt_locals_imp, mt_testclasses, mv_fixpt.
  ENDMETHOD.


  METHOD run_check.
    DATA: ls_dir       TYPE trdir,
          lv_msg       TYPE string,
          lv_line      TYPE i,
          lv_word      TYPE string,
          lv_classpool TYPE syrepid,
          lv_testclasses_start TYPE i,
          lv_include TYPE string.

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
      " Use FIXPT from XML metadata (default to 'X' if not specified for modern ABAP)
      IF mv_fixpt IS NOT INITIAL.
        ls_dir-fixpt = mv_fixpt.
      ELSE.
        ls_dir-fixpt = 'X'.  " Default to FIXPT=X for new classes
      ENDIF.
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
      " Calculate where testclasses section starts
      " Skeleton: CLASS-POOL (1) + locals_def + main + locals_imp + testclasses
      lv_testclasses_start = iv_prepended_lines + iv_main_lines + iv_locals_imp_lines.

      DATA(lv_adjusted_line) = lv_line.

      " Determine which section the error is in and adjust accordingly
      IF lv_line > lv_testclasses_start.
        " Error is in testclasses section
        lv_adjusted_line = lv_line - lv_testclasses_start.
        lv_include = 'testclasses'.
      ELSEIF lv_line > iv_prepended_lines + iv_main_lines.
        " Error is in locals_imp section
        lv_adjusted_line = lv_line - ( iv_prepended_lines + iv_main_lines ).
        lv_include = 'locals_imp'.
      ELSEIF lv_line > iv_prepended_lines.
        " Error is in main source section
        lv_adjusted_line = lv_line - iv_prepended_lines.
        lv_include = 'main'.
      ELSE.
        " Error is in CLASS-POOL or locals_def section
        IF lv_line > 1.
          lv_adjusted_line = lv_line - 1.  " Subtract CLASS-POOL line
          lv_include = 'locals_def'.
        ELSE.
          lv_adjusted_line = 1.
          lv_include = 'class_pool'.
        ENDIF.
      ENDIF.

      IF lv_adjusted_line < 1.
        lv_adjusted_line = 1.
      ENDIF.

      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rs_result-errors = VALUE #( (
        line = lv_adjusted_line
        text = lv_msg
        word = lv_word
        include = lv_include ) ).
      rs_result-message = lv_msg.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
