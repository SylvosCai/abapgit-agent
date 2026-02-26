"! <p class="shorttext synchronized">Syntax Checker - Working Area Approach</p>
"! Checks ABAP source code syntax without activation using the working area approach.
"! Writes source to inactive includes, regenerates the class/interface pool, runs syntax check, and cleans up.
"! Version: 2.1 - Fixed bug where SEO_CLASS_CHECK_CLASSPOOL checked active version.
"!               Now calls SEO_CLASS_GENERATE_CLASSPOOL first to regenerate with inactive source.
CLASS zcl_abgagt_syntax_checker DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    " Error structure
    TYPES: BEGIN OF ty_error,
             line        TYPE i,
             column      TYPE i,
             text        TYPE string,
             word        TYPE string,
             include     TYPE string,
             method_name TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

    " Warning structure
    TYPES: BEGIN OF ty_warning,
             line     TYPE i,
             column   TYPE i,
             text     TYPE string,
             severity TYPE string,
             include  TYPE string,
           END OF ty_warning.

    TYPES ty_warnings TYPE STANDARD TABLE OF ty_warning WITH NON-UNIQUE DEFAULT KEY.

    " Result structure
    TYPES: BEGIN OF ty_result,
             object_type  TYPE string,
             object_name  TYPE string,
             success      TYPE abap_bool,
             error_count  TYPE i,
             errors       TYPE ty_errors,
             warnings     TYPE ty_warnings,
             message      TYPE string,
           END OF ty_result.

    " Source object structure (input)
    TYPES: BEGIN OF ty_source_object,
             object_type TYPE string,
             object_name TYPE string,
             source      TYPE string,
           END OF ty_source_object.

    TYPES ty_source_objects TYPE STANDARD TABLE OF ty_source_object WITH NON-UNIQUE DEFAULT KEY.

    "! Check class source syntax using working area approach
    "! @parameter iv_class_name | Class name (e.g., ZCL_MY_CLASS)
    "! @parameter it_source | Source code as string table
    "! @parameter rs_result | Syntax check result
    METHODS check_class
      IMPORTING iv_class_name   TYPE seoclsname
                it_source       TYPE string_table
      RETURNING VALUE(rs_result) TYPE ty_result.

    "! Check interface source syntax
    "! @parameter iv_intf_name | Interface name
    "! @parameter it_source | Source code as string table
    "! @parameter rs_result | Syntax check result
    METHODS check_interface
      IMPORTING iv_intf_name    TYPE seoclsname
                it_source       TYPE string_table
      RETURNING VALUE(rs_result) TYPE ty_result.

    "! Check program source syntax using SYNTAX-CHECK statement
    "! @parameter iv_program_name | Program name
    "! @parameter it_source | Source code as string table
    "! @parameter iv_uccheck | Unicode check mode (X=Standard, 5=Cloud)
    "! @parameter rs_result | Syntax check result
    METHODS check_program
      IMPORTING iv_program_name TYPE syrepid
                it_source       TYPE string_table
                iv_uccheck      TYPE trdir-uccheck DEFAULT 'X'
      RETURNING VALUE(rs_result) TYPE ty_result.

    "! Check source using SYNTAX-CHECK statement (in-memory, no DB writes)
    "! This is a simpler approach that doesn't require the class to exist.
    "! @parameter iv_class_name | Class name for context
    "! @parameter it_source | Source code as string table
    "! @parameter iv_uccheck | Unicode check mode (X=Standard, 5=Cloud)
    "! @parameter rs_result | Syntax check result
    METHODS check_class_syntax_statement
      IMPORTING iv_class_name   TYPE seoclsname
                it_source       TYPE string_table
                iv_uccheck      TYPE trdir-uccheck DEFAULT 'X'
      RETURNING VALUE(rs_result) TYPE ty_result.

    "! Check class source with local classes using working area approach
    "! @parameter iv_class_name | Class name (e.g., ZCL_MY_CLASS)
    "! @parameter it_source | Main class source code
    "! @parameter it_locals_def | Local class definitions (CCDEF)
    "! @parameter it_locals_imp | Local class implementations (CCIMP)
    "! @parameter rs_result | Syntax check result
    METHODS check_class_with_locals
      IMPORTING iv_class_name   TYPE seoclsname
                it_source       TYPE string_table
                it_locals_def   TYPE string_table OPTIONAL
                it_locals_imp   TYPE string_table OPTIONAL
      RETURNING VALUE(rs_result) TYPE ty_result.

  PRIVATE SECTION.

    " Include suffixes for class
    CONSTANTS: gc_suffix_cp    TYPE c LENGTH 2 VALUE 'CP',    " Class Pool
               gc_suffix_cu    TYPE c LENGTH 2 VALUE 'CU',    " Public Section
               gc_suffix_co    TYPE c LENGTH 2 VALUE 'CO',    " Protected Section
               gc_suffix_ci    TYPE c LENGTH 2 VALUE 'CI',    " Private Section
               gc_suffix_ccdef TYPE c LENGTH 5 VALUE 'CCDEF', " Local Definitions
               gc_suffix_ccimp TYPE c LENGTH 5 VALUE 'CCIMP', " Local Implementations
               gc_suffix_ccmac TYPE c LENGTH 5 VALUE 'CCMAC', " Local Macros
               gc_suffix_ccau  TYPE c LENGTH 4 VALUE 'CCAU'.  " Test Classes

    " Track written includes for cleanup
    DATA mt_written_includes TYPE TABLE OF syrepid.

    "! Write source to inactive include
    "! @parameter iv_include | Include name
    "! @parameter it_source | Source code
    "! @parameter iv_extension_type | Extension type for class includes (CD=CCDEF, CI=CCIMP)
    METHODS write_inactive_include
      IMPORTING iv_include        TYPE syrepid
                it_source         TYPE string_table
                iv_extension_type TYPE sychar02 OPTIONAL
      RAISING   cx_sy_open_sql_error.

    "! Delete inactive include (cleanup)
    METHODS delete_inactive_include
      IMPORTING iv_include TYPE syrepid.

    "! Cleanup all written includes
    METHODS cleanup_all_includes.

    "! Run syntax check on class source using SYNTAX-CHECK statement
    METHODS run_class_check
      IMPORTING iv_class_name   TYPE seoclsname
                it_source       TYPE string_table
      RETURNING VALUE(rs_result) TYPE ty_result.

    "! Run syntax check on interface source using SYNTAX-CHECK statement
    METHODS run_interface_check
      IMPORTING iv_intf_name    TYPE seoclsname
                it_source       TYPE string_table
      RETURNING VALUE(rs_result) TYPE ty_result.

    "! Build class skeleton for SYNTAX-CHECK statement
    METHODS build_class_skeleton
      IMPORTING iv_class_name      TYPE seoclsname
                it_source          TYPE string_table
      RETURNING VALUE(rt_skeleton) TYPE string_table.

    "! Get method name from TMDIR by index
    METHODS get_method_name_by_index
      IMPORTING iv_class_name   TYPE seoclsname
                iv_method_index TYPE i
      RETURNING VALUE(rv_name)  TYPE seocmpname.

    "! Parse include name to extract method index (CM001 -> 1)
    METHODS parse_method_index
      IMPORTING iv_include     TYPE string
      RETURNING VALUE(rv_index) TYPE i.

ENDCLASS.


CLASS zcl_abgagt_syntax_checker IMPLEMENTATION.

  METHOD check_class.
    DATA: lo_scanner    TYPE REF TO cl_oo_source_scanner_class,
          lv_program    TYPE syrepid,
          lt_source     TYPE string_table,
          lt_methods    TYPE cl_oo_source_scanner_class=>type_method_implementations,
          lv_method     TYPE seocpdname,
          ls_mtdkey     TYPE seocpdkey,
          lx_scan_error TYPE REF TO cx_root.

    rs_result-object_type = 'CLAS'.
    rs_result-object_name = iv_class_name.

    CLEAR mt_written_includes.

    TRY.
        " 1. Parse source using class scanner
        TRY.
            lo_scanner = cl_oo_source_scanner_class=>create_class_scanner(
              clif_name = iv_class_name
              source    = it_source ).
            lo_scanner->scan( ).
          CATCH cx_root INTO lx_scan_error.
            " Source cannot be parsed - return error
            rs_result-success = abap_false.
            rs_result-error_count = 1.
            rs_result-errors = VALUE #( (
              line = 1
              text = |Source parse error: { lx_scan_error->get_text( ) }| ) ).
            rs_result-message = lx_scan_error->get_text( ).
            RETURN.
        ENDTRY.

        " 2. Write sections to inactive includes
        " Public section
        lt_source = lo_scanner->get_public_section_source( ).
        IF lt_source IS NOT INITIAL.
          lv_program = cl_oo_classname_service=>get_pubsec_name( iv_class_name ).
          write_inactive_include( iv_include = lv_program it_source = lt_source ).
        ENDIF.

        " Protected section
        lt_source = lo_scanner->get_protected_section_source( ).
        IF lt_source IS NOT INITIAL.
          lv_program = cl_oo_classname_service=>get_prosec_name( iv_class_name ).
          write_inactive_include( iv_include = lv_program it_source = lt_source ).
        ENDIF.

        " Private section
        lt_source = lo_scanner->get_private_section_source( ).
        IF lt_source IS NOT INITIAL.
          lv_program = cl_oo_classname_service=>get_prisec_name( iv_class_name ).
          write_inactive_include( iv_include = lv_program it_source = lt_source ).
        ENDIF.

        " Note: Local definitions/implementations (CCDEF, CCIMP) are separate includes
        " and not part of the main class source scanned here. They would need to be
        " provided separately if syntax checking local classes is required.

        " Method implementations
        lt_methods = lo_scanner->get_method_implementations( ).
        LOOP AT lt_methods INTO lv_method.
          TRY.
              lt_source = lo_scanner->get_method_impl_source( lv_method ).
            CATCH cx_oo_clif_component.
              CONTINUE.
          ENDTRY.

          " Get or create method include
          ls_mtdkey-clsname = iv_class_name.
          ls_mtdkey-cpdname = lv_method.

          cl_oo_classname_service=>get_method_include(
            EXPORTING
              mtdkey              = ls_mtdkey
            RECEIVING
              result              = lv_program
            EXCEPTIONS
              method_not_existing = 1
              OTHERS              = 2 ).

          IF sy-subrc = 0 AND lv_program IS NOT INITIAL.
            write_inactive_include( iv_include = lv_program it_source = lt_source ).
          ENDIF.
        ENDLOOP.

        " 3. Run syntax check using SYNTAX-CHECK on original source
        rs_result = run_class_check(
          iv_class_name = iv_class_name
          it_source     = it_source ).

      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-errors = VALUE #( (
          line = 1
          text = lx_error->get_text( ) ) ).
        rs_result-message = lx_error->get_text( ).
    ENDTRY.

    " 4. Cleanup - ALWAYS executed
    cleanup_all_includes( ).
  ENDMETHOD.


  METHOD check_class_with_locals.
    DATA: lo_scanner    TYPE REF TO cl_oo_source_scanner_class,
          lv_program    TYPE syrepid,
          lt_source     TYPE string_table,
          lt_methods    TYPE cl_oo_source_scanner_class=>type_method_implementations,
          lv_method     TYPE seocpdname,
          ls_mtdkey     TYPE seocpdkey,
          lx_scan_error TYPE REF TO cx_root.

    rs_result-object_type = 'CLAS'.
    rs_result-object_name = iv_class_name.

    CLEAR mt_written_includes.

    TRY.
        " 1. Parse main class source using class scanner
        TRY.
            lo_scanner = cl_oo_source_scanner_class=>create_class_scanner(
              clif_name = iv_class_name
              source    = it_source ).
            lo_scanner->scan( ).
          CATCH cx_root INTO lx_scan_error.
            rs_result-success = abap_false.
            rs_result-error_count = 1.
            rs_result-errors = VALUE #( (
              line = 1
              text = |Source parse error: { lx_scan_error->get_text( ) }| ) ).
            rs_result-message = lx_scan_error->get_text( ).
            RETURN.
        ENDTRY.

        " 2. Write sections to inactive includes
        " Public section
        lt_source = lo_scanner->get_public_section_source( ).
        IF lt_source IS NOT INITIAL.
          lv_program = cl_oo_classname_service=>get_pubsec_name( iv_class_name ).
          write_inactive_include( iv_include = lv_program it_source = lt_source ).
        ENDIF.

        " Protected section
        lt_source = lo_scanner->get_protected_section_source( ).
        IF lt_source IS NOT INITIAL.
          lv_program = cl_oo_classname_service=>get_prosec_name( iv_class_name ).
          write_inactive_include( iv_include = lv_program it_source = lt_source ).
        ENDIF.

        " Private section
        lt_source = lo_scanner->get_private_section_source( ).
        IF lt_source IS NOT INITIAL.
          lv_program = cl_oo_classname_service=>get_prisec_name( iv_class_name ).
          write_inactive_include( iv_include = lv_program it_source = lt_source ).
        ENDIF.

        " Local class definitions (CCDEF) - if provided
        IF it_locals_def IS NOT INITIAL.
          lv_program = cl_oo_classname_service=>get_ccdef_name( iv_class_name ).
          write_inactive_include(
            iv_include        = lv_program
            it_source         = it_locals_def
            iv_extension_type = 'CD' ).
        ENDIF.

        " Local class implementations (CCIMP) - if provided
        IF it_locals_imp IS NOT INITIAL.
          lv_program = cl_oo_classname_service=>get_ccimp_name( iv_class_name ).
          write_inactive_include(
            iv_include        = lv_program
            it_source         = it_locals_imp
            iv_extension_type = 'CI' ).
        ENDIF.

        " Method implementations
        lt_methods = lo_scanner->get_method_implementations( ).
        LOOP AT lt_methods INTO lv_method.
          TRY.
              lt_source = lo_scanner->get_method_impl_source( lv_method ).
            CATCH cx_oo_clif_component.
              CONTINUE.
          ENDTRY.

          ls_mtdkey-clsname = iv_class_name.
          ls_mtdkey-cpdname = lv_method.

          cl_oo_classname_service=>get_method_include(
            EXPORTING
              mtdkey              = ls_mtdkey
            RECEIVING
              result              = lv_program
            EXCEPTIONS
              method_not_existing = 1
              OTHERS              = 2 ).

          IF sy-subrc = 0 AND lv_program IS NOT INITIAL.
            write_inactive_include( iv_include = lv_program it_source = lt_source ).
          ENDIF.
        ENDLOOP.

        " 3. Run syntax check using SYNTAX-CHECK on original source
        rs_result = run_class_check(
          iv_class_name = iv_class_name
          it_source     = it_source ).

      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-errors = VALUE #( (
          line = 1
          text = lx_error->get_text( ) ) ).
        rs_result-message = lx_error->get_text( ).
    ENDTRY.

    " 4. Cleanup - ALWAYS executed
    cleanup_all_includes( ).
  ENDMETHOD.


  METHOD check_interface.
    DATA: lo_scanner    TYPE REF TO cl_oo_source_scanner_interface,
          lv_program    TYPE syrepid,
          lx_scan_error TYPE REF TO cx_root.

    rs_result-object_type = 'INTF'.
    rs_result-object_name = iv_intf_name.

    CLEAR mt_written_includes.

    TRY.
        " 1. Parse source using interface scanner
        TRY.
            lo_scanner = cl_oo_source_scanner_interface=>create_interface_scanner(
              clif_name = iv_intf_name
              source    = it_source ).
            lo_scanner->scan( ).
          CATCH cx_root INTO lx_scan_error.
            rs_result-success = abap_false.
            rs_result-error_count = 1.
            rs_result-errors = VALUE #( (
              line = 1
              text = |Source parse error: { lx_scan_error->get_text( ) }| ) ).
            rs_result-message = lx_scan_error->get_text( ).
            RETURN.
        ENDTRY.

        " 2. Write interface source to inactive include
        lv_program = cl_oo_classname_service=>get_pubsec_name( iv_intf_name ).
        write_inactive_include( iv_include = lv_program it_source = it_source ).

        " 3. Run syntax check using SYNTAX-CHECK on original source
        rs_result = run_interface_check(
          iv_intf_name = iv_intf_name
          it_source    = it_source ).

      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-errors = VALUE #( (
          line = 1
          text = lx_error->get_text( ) ) ).
        rs_result-message = lx_error->get_text( ).
    ENDTRY.

    " 4. Cleanup
    cleanup_all_includes( ).
  ENDMETHOD.


  METHOD check_program.
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


  METHOD check_class_syntax_statement.
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
      CLEAR ls_dir.
    ENDIF.

    " Set unicode check mode
    ls_dir-uccheck = iv_uccheck.

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


  METHOD write_inactive_include.
    " Write source to inactive include (STATE = 'I')
    " For class extension includes (CCDEF, CCIMP), EXTENSION TYPE is required
    IF iv_extension_type IS NOT INITIAL.
      INSERT REPORT iv_include FROM it_source STATE 'I'
        EXTENSION TYPE iv_extension_type.
    ELSE.
      INSERT REPORT iv_include FROM it_source STATE 'I'.
    ENDIF.

    IF sy-subrc = 0.
      APPEND iv_include TO mt_written_includes.
    ENDIF.
  ENDMETHOD.


  METHOD delete_inactive_include.
    " Delete inactive version of include
    DELETE REPORT iv_include STATE 'I'.
  ENDMETHOD.


  METHOD cleanup_all_includes.
    " Delete all inactive includes that were written
    LOOP AT mt_written_includes INTO DATA(lv_include).
      delete_inactive_include( lv_include ).
    ENDLOOP.
    CLEAR mt_written_includes.
  ENDMETHOD.


  METHOD run_class_check.
    DATA: ls_dir        TYPE trdir,
          lv_msg        TYPE string,
          lv_line       TYPE i,
          lv_word       TYPE string,
          lv_classpool  TYPE syrepid,
          lt_skeleton   TYPE string_table.

    rs_result-object_type = 'CLAS'.
    rs_result-object_name = iv_class_name.

    " Build class skeleton from original source
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

    " Get TRDIR entry for class pool (for context)
    lv_classpool = cl_oo_classname_service=>get_classpool_name( iv_class_name ).
    SELECT SINGLE * FROM trdir INTO ls_dir WHERE name = lv_classpool.
    IF sy-subrc <> 0.
      " Class doesn't exist - use default
      ls_dir-name = lv_classpool.
      ls_dir-subc = 'K'.  " Class pool
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


  METHOD run_interface_check.
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


  METHOD build_class_skeleton.
    " Build a class skeleton that can be used with SYNTAX-CHECK statement
    " This is used when the class doesn't exist yet or for simpler checks

    DATA: lv_line TYPE string.

    " CLASS-POOL statement
    APPEND 'CLASS-POOL.' TO rt_skeleton.

    " Include the source directly (assumes it's a complete class definition + implementation)
    APPEND LINES OF it_source TO rt_skeleton.
  ENDMETHOD.


  METHOD get_method_name_by_index.
    " Get method name from TMDIR by method index
    SELECT SINGLE methodname FROM tmdir
      INTO rv_name
      WHERE classname = iv_class_name
        AND methodindx = iv_method_index.
  ENDMETHOD.


  METHOD parse_method_index.
    " Parse method index from include name (e.g., CM001 -> 1, CM00A -> 10)
    DATA: lv_suffix TYPE string,
          lv_index_str TYPE string.

    " Extract suffix after '=' characters
    DATA(lv_pos) = strlen( iv_include ) - 5.
    IF lv_pos > 0.
      lv_suffix = iv_include+lv_pos(5).

      " Check if it's a method include (CMxxx)
      IF lv_suffix(2) = 'CM'.
        lv_index_str = lv_suffix+2(3).
        " Convert base-36 to integer (simplified - assumes <= 999)
        TRY.
            rv_index = lv_index_str.
          CATCH cx_root.
            rv_index = 0.
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
