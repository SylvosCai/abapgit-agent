*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_clas DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_clas IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lo_util TYPE REF TO zif_abgagt_util,
          ls_include_info TYPE zif_abgagt_util=>ty_include_info,
          ls_obj_info TYPE zif_abgagt_util=>ty_object_info,
          lv_prog TYPE program,
          lt_source TYPE TABLE OF string,
          lv_line TYPE string,
          lv_clsname TYPE seoclsname,
          lv_obj_name TYPE tadir-obj_name.

    rs_info-name = iv_name.
    rs_info-type = 'CLAS'.
    rs_info-type_text = 'Class'.

    " Use util to detect source include
    lo_util = zcl_abgagt_util=>get_instance( ).
    ls_include_info = lo_util->detect_include_info( iv_name ).

    IF ls_include_info-is_source_include = abap_true.
      " Source include detected - get devclass using unpadded obj_name
      lv_obj_name = ls_include_info-obj_name.
      ls_obj_info = lo_util->get_object_info_from_tadir( lv_obj_name ).
      rs_info-devclass = ls_obj_info-devclass.
      IF rs_info-devclass IS NOT INITIAL.
        rs_info-description = |Class { ls_include_info-obj_name } in { rs_info-devclass } (Source Include)|.
      ELSE.
        rs_info-description = |Class { ls_include_info-obj_name } (Source Include)|.
      ENDIF.
    ELSE.
      " Standard class - get info from TADIR
      lv_obj_name = iv_name.
      ls_obj_info = lo_util->get_object_info_from_tadir( lv_obj_name ).
      rs_info-devclass = ls_obj_info-devclass.
      IF rs_info-devclass IS NOT INITIAL.
        rs_info-description = |Class { iv_name } in { rs_info-devclass }|.
      ELSE.
        rs_info-not_found = abap_true.
      ENDIF.
    ENDIF.

    " Get class name (strip include suffix if needed)
    lv_clsname = ls_include_info-obj_name.
    IF lv_clsname IS INITIAL.
      lv_clsname = iv_name.
    ENDIF.

    IF iv_full = abap_false.
      " Default: return public section source only
      CALL METHOD cl_oo_classname_service=>get_pubsec_name
        EXPORTING
          clsname = lv_clsname
        RECEIVING
          result  = lv_prog.

      READ REPORT lv_prog INTO lt_source.
      LOOP AT lt_source INTO lv_line.
        IF rs_info-source IS INITIAL.
          rs_info-source = lv_line.
        ELSE.
          rs_info-source = rs_info-source && |\n| && lv_line.
        ENDIF.
      ENDLOOP.
    ELSE.
      " Full mode: assemble all sections into rs_info-sections
      DATA: ls_section     TYPE zcl_abgagt_command_view=>ty_section,
            lv_pubsec      TYPE program,
            lv_cm_suffix   TYPE string,
            lv_include_pad TYPE program,
            lv_condensed   TYPE string.

      " Build padded class name prefix (30 chars) for direct include reads
      DATA lv_pad30 TYPE program.
      lv_pad30 = lv_clsname.
      WHILE strlen( lv_pad30 ) < 30.
        lv_pad30 = lv_pad30 && '='.
      ENDWHILE.

      " --- Attempt to read the full assembled source via the class pool program ---
      " cl_oo_classname_service=>get_classpool_name returns the class pool main program
      " which, when read via READ REPORT, gives the full assembled source with line
      " numbers that match ADT's breakpoint coordinate system exactly.
      DATA lv_classpool TYPE program.
      DATA lt_full_source TYPE string_table.
      DATA lv_pool_available TYPE abap_bool VALUE abap_false.

      CALL METHOD cl_oo_classname_service=>get_classpool_name
        EXPORTING  clsname = lv_clsname
        RECEIVING  result  = lv_classpool.

      IF lv_classpool IS NOT INITIAL.
        READ REPORT lv_classpool INTO lt_full_source.
        IF sy-subrc = 0 AND lines( lt_full_source ) > 0.
          lv_pool_available = abap_true.
        ENDIF.
      ENDIF.

      " Fall back to CS include if classpool read failed
      DATA lv_cs_available TYPE abap_bool VALUE abap_false.
      DATA lv_cs_pos TYPE i VALUE 1.
      DATA lv_search_line TYPE string.
      DATA lv_found_pos   TYPE i.

      IF lv_pool_available = abap_false.
        " Fallback: use CS include (complete assembled source — may differ slightly from ADT)
        DATA lt_cs_source TYPE string_table.
        lv_prog = lv_pad30 && 'CS'.
        READ REPORT lv_prog INTO lt_cs_source.
        IF sy-subrc = 0.
          lv_cs_available = abap_true.
          lt_full_source  = lt_cs_source.
          lv_pool_available = abap_true.
        ENDIF.
      ENDIF.

      " --- Definition sections: CU (public), CO (protected), CP (private) ---
      " For CU use cl_oo_classname_service as before; CO and CP via padded include names
      CALL METHOD cl_oo_classname_service=>get_pubsec_name
        EXPORTING  clsname = lv_clsname
        RECEIVING  result  = lv_pubsec.

      " ---------------------------------------------------------------
      " Helper: compute global_start for a section/method by scanning
      " lt_full_source for the first line of that section's include content.
      " We search forward from lv_scan_from for a line that matches the
      " first non-blank line of lt_needle. Returns 0 if not found.
      " ---------------------------------------------------------------

      " Scan helper variables
      DATA lv_scan_from TYPE i VALUE 1.

      " --- Public section (CU) ---
      CLEAR ls_section.
      ls_section-suffix      = 'CU'.
      ls_section-description = 'Public Section'.
      CLEAR lt_source.
      READ REPORT lv_pubsec INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.

      IF lv_pool_available = abap_true.
        " Find the first non-blank line of the CU include in the full source.
        " CU always starts near the top; search from line 1.
        DATA lv_cu_needle TYPE string.
        LOOP AT lt_source INTO lv_cu_needle.
          CONDENSE lv_cu_needle.
          IF lv_cu_needle IS NOT INITIAL.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF lv_cu_needle IS NOT INITIAL.
          lv_scan_from = 1.
          WHILE lv_scan_from <= lines( lt_full_source ).
            DATA lv_full_cmp TYPE string.
            lv_full_cmp = lt_full_source[ lv_scan_from ].
            CONDENSE lv_full_cmp.
            IF lv_full_cmp = lv_cu_needle.
              EXIT.
            ENDIF.
            lv_scan_from = lv_scan_from + 1.
          ENDWHILE.
          ls_section-global_start = lv_scan_from.
        ELSE.
          ls_section-global_start = 1.
          lv_scan_from = 1.
        ENDIF.
        " Advance scan past CU content
        lv_scan_from = ls_section-global_start + lines( lt_source ).
      ELSE.
        ls_section-global_start = 1.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      " --- Protected section (CO) ---
      CLEAR ls_section.
      ls_section-suffix      = 'CO'.
      ls_section-description = 'Protected Section'.
      lv_prog = lv_pad30 && 'CO'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.

      IF lv_pool_available = abap_true.
        " Skip blank lines between sections
        WHILE lv_scan_from <= lines( lt_full_source ).
          DATA lv_tmp TYPE string.
          lv_tmp = lt_full_source[ lv_scan_from ].
          CONDENSE lv_tmp.
          IF lv_tmp IS NOT INITIAL.
            EXIT.
          ENDIF.
          lv_scan_from = lv_scan_from + 1.
        ENDWHILE.
        ls_section-global_start = lv_scan_from.
        lv_scan_from = lv_scan_from + lines( lt_source ).
      ELSE.
        ls_section-global_start = 0.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      " --- Private section (CP) ---
      CLEAR ls_section.
      ls_section-suffix      = 'CP'.
      ls_section-description = 'Private Section'.
      lv_prog = lv_pad30 && 'CP'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.

      IF lv_pool_available = abap_true.
        " Skip blank lines between sections
        WHILE lv_scan_from <= lines( lt_full_source ).
          lv_tmp = lt_full_source[ lv_scan_from ].
          CONDENSE lv_tmp.
          IF lv_tmp IS NOT INITIAL.
            EXIT.
          ENDIF.
          lv_scan_from = lv_scan_from + 1.
        ENDWHILE.
        ls_section-global_start = lv_scan_from.
        lv_scan_from = lv_scan_from + lines( lt_source ).
      ELSE.
        ls_section-global_start = 0.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      " --- Advance past the implementation wrapper ---
      " The assembled source has:
      "   CLASS <clsname> IMPLEMENTATION.   ← one line
      "   (blank line)                       ← one line
      " Scan forward until we hit the first METHOD statement.
      IF lv_pool_available = abap_true.
        WHILE lv_scan_from <= lines( lt_full_source ).
          lv_condensed = lt_full_source[ lv_scan_from ].
          CONDENSE lv_condensed.
          IF strlen( lv_condensed ) >= 7
              AND ( lv_condensed(7) = 'METHOD ' OR to_lower( lv_condensed(7) ) = 'method ' ).
            EXIT.
          ENDIF.
          lv_scan_from = lv_scan_from + 1.
        ENDWHILE.
      ENDIF.

      SELECT methodname, methodindx FROM tmdir
        INTO TABLE @DATA(lt_methods)
        WHERE classname = @lv_clsname
        ORDER BY methodindx.

      LOOP AT lt_methods INTO DATA(ls_method).
        lv_cm_suffix = lo_util->convert_index_to_cm_suffix( CONV i( ls_method-methodindx ) ).
        lv_include_pad = lv_pad30 && lv_cm_suffix.

        " Compute global_start for this CM method by scanning the full source
        " for its METHOD statement, starting from lv_scan_from (current position).
        DATA lv_method_upper TYPE string.
        lv_method_upper = to_upper( ls_method-methodname ).
        lv_found_pos = 0.

        IF lv_pool_available = abap_true.
          DATA lv_scan TYPE i.
          lv_scan = lv_scan_from.
          WHILE lv_scan <= lines( lt_full_source ).
            lv_condensed = to_upper( lt_full_source[ lv_scan ] ).
            CONDENSE lv_condensed.
            " Match only actual METHOD statements (starts with "METHOD "), not comments
            IF strlen( lv_condensed ) >= 7 AND lv_condensed(7) = 'METHOD '
                AND lv_condensed CS lv_method_upper.
              lv_found_pos = lv_scan.
              EXIT.
            ENDIF.
            lv_scan = lv_scan + 1.
          ENDWHILE.
          IF lv_found_pos > 0.
            lv_scan_from = lv_found_pos.
          ENDIF.
        ENDIF.

        CLEAR ls_section.
        ls_section-suffix       = lv_cm_suffix.
        ls_section-description  = 'Class Method'.
        ls_section-method_name  = CONV string( ls_method-methodname ).
        ls_section-global_start = COND #( WHEN lv_found_pos > 0 THEN lv_found_pos ELSE 0 ).
        lv_prog = lv_include_pad.
        CLEAR lt_source.
        READ REPORT lv_prog INTO lt_source.
        IF sy-subrc = 0.
          ls_section-lines = lt_source.
        ENDIF.
        " Advance scan past this method's content (include lines) then skip
        " ENDMETHOD / blank lines until the next METHOD statement.
        IF lv_pool_available = abap_true AND lv_found_pos > 0.
          lv_scan_from = lv_found_pos + lines( lt_source ).
          WHILE lv_scan_from <= lines( lt_full_source ).
            lv_condensed = to_upper( lt_full_source[ lv_scan_from ] ).
            CONDENSE lv_condensed.
            IF strlen( lv_condensed ) >= 7 AND lv_condensed(7) = 'METHOD '.
              EXIT.
            ELSEIF lv_condensed CS 'ENDCLASS'.
              EXIT.
            ENDIF.
            lv_scan_from = lv_scan_from + 1.
          ENDWHILE.
        ENDIF.
        APPEND ls_section TO rs_info-sections.
      ENDLOOP.

      " --- Auxiliary sections: CCDEF, CCIMP, CCAU ---
      " These live in separate git files and are not part of the assembled source.
      " global_start = 0 signals that global line numbers are not applicable.
      CLEAR ls_section.
      ls_section-suffix      = 'CCDEF'.
      ls_section-description = 'Local Definitions'.
      ls_section-file        = 'locals_def'.
      lv_prog = lv_pad30 && 'CCDEF'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      CLEAR ls_section.
      ls_section-suffix      = 'CCIMP'.
      ls_section-description = 'Local Implementations'.
      ls_section-file        = 'locals_imp'.
      lv_prog = lv_pad30 && 'CCIMP'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      CLEAR ls_section.
      ls_section-suffix      = 'CCAU'.
      ls_section-description = 'Unit Test'.
      ls_section-file        = 'testclasses'.
      lv_prog = lv_pad30 && 'CCAU'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
