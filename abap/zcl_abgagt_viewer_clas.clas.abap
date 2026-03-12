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
            lv_include_pad TYPE program.

      " Build padded class name prefix (30 chars) for direct include reads
      DATA lv_pad30 TYPE program.
      lv_pad30 = lv_clsname.
      WHILE strlen( lv_pad30 ) < 30.
        lv_pad30 = lv_pad30 && '='.
      ENDWHILE.

      " Read the CS include (complete assembled source as served by ADT).
      " This is the ground truth for global line numbers.
      DATA lt_cs_source TYPE string_table.
      DATA lv_cs_available TYPE abap_bool VALUE abap_false.
      lv_prog = lv_pad30 && 'CS'.
      READ REPORT lv_prog INTO lt_cs_source.
      IF sy-subrc = 0.
        lv_cs_available = abap_true.
      ENDIF.
      " lv_cs_pos tracks our current scan position within the assembled source
      DATA lv_cs_pos TYPE i VALUE 1.
      DATA lv_search_line TYPE string.
      DATA lv_found_pos   TYPE i.

      " --- Definition sections: CU (public), CO (protected), CP (private) ---
      " For CU use cl_oo_classname_service as before; CO and CP via padded include names
      CALL METHOD cl_oo_classname_service=>get_pubsec_name
        EXPORTING  clsname = lv_clsname
        RECEIVING  result  = lv_pubsec.

      " Public section (CU): always starts at global line 1
      CLEAR ls_section.
      ls_section-suffix       = 'CU'.
      ls_section-description  = 'Public Section'.
      ls_section-global_start = 1.
      CLEAR lt_source.
      READ REPORT lv_pubsec INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      " Advance CS scan position past CU content (READ REPORT trims trailing blanks,
      " so scan past known lines and then skip any trailing blanks in CS)
      lv_cs_pos = lv_cs_pos + lines( lt_source ).
      " Skip trailing blank lines in CS that belong to CU
      WHILE lv_cs_pos <= lines( lt_cs_source ).
        IF lt_cs_source[ lv_cs_pos ] IS INITIAL.
          lv_cs_pos = lv_cs_pos + 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDWHILE.
      APPEND ls_section TO rs_info-sections.

      " Protected section (CO): global_start = current CS scan position
      CLEAR ls_section.
      ls_section-suffix       = 'CO'.
      ls_section-description  = 'Protected Section'.
      ls_section-global_start = lv_cs_pos.
      lv_prog = lv_pad30 && 'CO'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      lv_cs_pos = lv_cs_pos + lines( lt_source ).
      WHILE lv_cs_pos <= lines( lt_cs_source ).
        IF lt_cs_source[ lv_cs_pos ] IS INITIAL.
          lv_cs_pos = lv_cs_pos + 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDWHILE.
      APPEND ls_section TO rs_info-sections.

      " Private section (CP): global_start = current CS scan position
      CLEAR ls_section.
      ls_section-suffix       = 'CP'.
      ls_section-description  = 'Private Section'.
      ls_section-global_start = lv_cs_pos.
      lv_prog = lv_pad30 && 'CP'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      lv_cs_pos = lv_cs_pos + lines( lt_source ).
      " Skip trailing blank lines of CP in CS (these precede the CLASS IMPLEMENTATION. line)
      WHILE lv_cs_pos <= lines( lt_cs_source ).
        IF lt_cs_source[ lv_cs_pos ] IS INITIAL.
          lv_cs_pos = lv_cs_pos + 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDWHILE.
      APPEND ls_section TO rs_info-sections.

      " Advance past the implementation wrapper in CS.
      " The assembled source has:
      "   CLASS <clsname> IMPLEMENTATION.   ← one line
      "   (blank line)                       ← one line
      " We scan forward past lines until we hit METHOD (the first method line)
      " or exhaust CS. We skip lines that are NOT method implementations.
      WHILE lv_cs_pos <= lines( lt_cs_source ).
        lv_search_line = lt_cs_source[ lv_cs_pos ].
        " A method implementation block starts with "METHOD " (case-insensitive)
        IF lv_search_line CS 'METHOD ' OR lv_search_line CS 'method '.
          EXIT.
        ENDIF.
        lv_cs_pos = lv_cs_pos + 1.
      ENDWHILE.

      SELECT methodname, methodindx FROM tmdir
        INTO TABLE @DATA(lt_methods)
        WHERE classname = @lv_clsname
        ORDER BY methodindx.

      LOOP AT lt_methods INTO DATA(ls_method).
        lv_cm_suffix = lo_util->convert_index_to_cm_suffix( CONV i( ls_method-methodindx ) ).
        lv_include_pad = lv_pad30 && lv_cm_suffix.

        " Scan CS forward to find the METHOD line for this method.
        " The CM include begins exactly at the METHOD statement.
        DATA lv_method_upper TYPE string.
        lv_method_upper = to_upper( ls_method-methodname ).
        lv_found_pos = 0.
        DATA lv_scan TYPE i.
        lv_scan = lv_cs_pos.
        WHILE lv_scan <= lines( lt_cs_source ).
          lv_search_line = to_upper( lt_cs_source[ lv_scan ] ).
          IF lv_search_line CS 'METHOD' AND lv_search_line CS lv_method_upper.
            lv_found_pos = lv_scan.
            EXIT.
          ENDIF.
          lv_scan = lv_scan + 1.
        ENDWHILE.
        IF lv_found_pos > 0.
          lv_cs_pos = lv_found_pos.
        ENDIF.

        CLEAR ls_section.
        ls_section-suffix       = lv_cm_suffix.
        ls_section-description  = 'Class Method'.
        ls_section-method_name  = CONV string( ls_method-methodname ).
        ls_section-global_start = lv_cs_pos.
        lv_prog = lv_include_pad.
        CLEAR lt_source.
        READ REPORT lv_prog INTO lt_source.
        IF sy-subrc = 0.
          ls_section-lines = lt_source.
        ENDIF.
        " Advance CS past this method's content (include lines + trailing blanks/ENDMETHOD)
        lv_cs_pos = lv_cs_pos + lines( lt_source ).
        " Skip trailing blank lines / ENDMETHOD line in CS before the next METHOD
        WHILE lv_cs_pos <= lines( lt_cs_source ).
          lv_search_line = to_upper( lt_cs_source[ lv_cs_pos ] ).
          IF lv_search_line CS 'METHOD '.
            EXIT.
          ELSEIF lv_search_line CS 'ENDCLASS'.
            EXIT.
          ENDIF.
          lv_cs_pos = lv_cs_pos + 1.
        ENDWHILE.
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
