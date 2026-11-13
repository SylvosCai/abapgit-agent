"* Use this source text module for the class definition
CLASS zcl_abgagt_util DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_util.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_util) TYPE REF TO zif_abgagt_util.
  PRIVATE SECTION.
    DATA mv_log TYPE string.
    DATA mv_has_error TYPE abap_bool.
ENDCLASS.

CLASS zcl_abgagt_util IMPLEMENTATION.
  METHOD get_instance.
    DATA lo_util TYPE REF TO zcl_abgagt_util.
    lo_util = NEW #( ).
    ro_util = lo_util.
  ENDMETHOD.

  METHOD zif_abgagt_util~parse_file_to_object.
    " Parse file path to extract obj_type and obj_name
    " 3-part pattern: "zcl_my_class.clas.abap"         -> CLAS, ZCL_MY_CLASS
    " 3-part pattern: "src/zcl_my_class.clas.abap"     -> CLAS, ZCL_MY_CLASS
    " 4-part pattern: "zcl_foo.enho.28bbfe2f.abap"     -> ENHO, ZCL_FOO
    "                 (hash middle segment is discarded; only obj_type+obj_name needed)

    CLEAR: ev_obj_type, ev_obj_name.
    DATA(lv_upper) = to_upper( iv_file ).
    REPLACE ALL OCCURRENCES OF '\' IN lv_upper WITH '/'.
    DATA(lv_path_offset) = find( val = reverse( lv_upper ) sub = '/' ).
    IF lv_path_offset >= 0.
      lv_path_offset = strlen( lv_upper ) - lv_path_offset.
      lv_upper = lv_upper+lv_path_offset.
    ENDIF.

    DATA lt_parts TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    SPLIT lv_upper AT '.' INTO TABLE lt_parts.
    DATA(lv_part_count) = lines( lt_parts ).
    DATA lv_obj_name TYPE string.

    DATA(lv_type_hint) = VALUE string( lt_parts[ 2 ] OPTIONAL ).
    DATA(lv_ext_hint) = VALUE string( lt_parts[ lv_part_count ] OPTIONAL ).
    IF lv_type_hint = 'BDEF' OR lv_type_hint = 'SRVD' OR lv_type_hint = 'DDLX'
       OR lv_ext_hint = 'ASBDEF' OR lv_ext_hint = 'SRVDSRV'
       OR lv_ext_hint = 'ASSRVD' OR lv_ext_hint = 'ASDDLXS'.
      IF lv_part_count <> 3.
        RETURN.
      ENDIF.
      CASE lv_type_hint.
        WHEN 'BDEF'.
          IF lv_ext_hint <> 'ASBDEF' AND lv_ext_hint <> 'XML'.
            RETURN.
          ENDIF.
        WHEN 'SRVD'.
          IF lv_ext_hint <> 'SRVDSRV' AND lv_ext_hint <> 'ASSRVD' AND lv_ext_hint <> 'XML'.
            RETURN.
          ENDIF.
        WHEN 'DDLX'.
          IF lv_ext_hint <> 'ASDDLXS' AND lv_ext_hint <> 'XML'.
            RETURN.
          ENDIF.
        WHEN OTHERS.
          RETURN.
      ENDCASE.
      lv_obj_name = lt_parts[ 1 ].
      IF lv_obj_name IS INITIAL OR lv_obj_name CN 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_#'.
        RETURN.
      ENDIF.
      IF lv_obj_name CS '#'.
        DATA lt_name_parts TYPE STANDARD TABLE OF string WITH EMPTY KEY.
        SPLIT lv_obj_name AT '#' INTO TABLE lt_name_parts.
        IF lines( lt_name_parts ) <> 3.
          RETURN.
        ENDIF.
        IF lt_name_parts[ 1 ] IS NOT INITIAL OR lt_name_parts[ 2 ] IS INITIAL OR lt_name_parts[ 3 ] IS INITIAL.
          RETURN.
        ENDIF.
        REPLACE ALL OCCURRENCES OF '#' IN lv_obj_name WITH '/'.
      ENDIF.
      IF strlen( lv_obj_name ) > 30.
        RETURN.
      ENDIF.
      ev_obj_type = lv_type_hint.
      ev_obj_name = lv_obj_name.
      RETURN.
    ENDIF.

    IF lv_part_count = 4.
      " 4-part patterns:
      "   <name>.clas.testclasses.abap  → CLAS  (class test include)
      "   <name>.clas.locals_def.abap   → CLAS  (class local types include)
      "   <name>.clas.locals_imp.abap   → CLAS  (class local impl include)
      "   <name>.enho.<hash>.abap       → ENHO  (enhancement hook source)
      READ TABLE lt_parts INDEX 4 INTO DATA(lv_ext4).
      IF lv_ext4 <> 'ABAP' AND lv_ext4 <> 'ASDDLS' AND lv_ext4 <> 'ASDCLS' AND lv_ext4 <> 'XML'.
        RETURN.
      ENDIF.
      READ TABLE lt_parts INDEX 2 INTO DATA(lv_type4).
      READ TABLE lt_parts INDEX 3 INTO DATA(lv_sub4).
      IF lv_type4 = 'CLAS' AND
         ( lv_sub4 = 'TESTCLASSES' OR lv_sub4 = 'LOCALS_DEF' OR lv_sub4 = 'LOCALS_IMP' ).
        READ TABLE lt_parts INDEX 1 INTO lv_obj_name.
        ev_obj_type = 'CLAS'.
      ELSEIF lv_type4 = 'ENHO'.
        READ TABLE lt_parts INDEX 1 INTO lv_obj_name.
        ev_obj_type = 'ENHO'.
      ELSE.
        RETURN.  " Unknown 4-part pattern
      ENDIF.

    ELSEIF lv_part_count = 3.
      " 3-part pattern: <name>.<type>.<ext>
      " Last part should be 'ABAP', 'ASDDLS', 'ASDCLS', or 'XML'
      " (XML-only objects: TABL, STRU, DTEL, TTYP, ...)
      READ TABLE lt_parts INDEX 3 INTO DATA(lv_last).
      IF lv_last <> 'ABAP' AND lv_last <> 'ASDDLS' AND lv_last <> 'ASDCLS' AND lv_last <> 'XML'.
        RETURN.
      ENDIF.

      READ TABLE lt_parts INDEX 1 INTO lv_obj_name.
      READ TABLE lt_parts INDEX 2 INTO DATA(lv_obj_type_raw).

      " Convert file extension to object type
      CASE lv_obj_type_raw.
        WHEN 'CLAS' OR 'CLASS'.
          ev_obj_type = 'CLAS'.
        WHEN 'INTF' OR 'INTERFACE'.
          ev_obj_type = 'INTF'.
        WHEN 'PROG' OR 'PROGRAM'.
          ev_obj_type = 'PROG'.
        WHEN 'FUGR' OR 'FUGROUP'.
          ev_obj_type = 'FUGR'.
        WHEN 'TABL' OR 'TABLE'.
          ev_obj_type = 'TABL'.
        WHEN 'DDLS'.
          ev_obj_type = 'DDLS'.
        WHEN OTHERS.
          ev_obj_type = lv_obj_type_raw.
      ENDCASE.

    ELSE.
      RETURN.
    ENDIF.

    IF lv_obj_name IS INITIAL.
      CLEAR ev_obj_type.
      RETURN.
    ENDIF.

    " abapGit encodes classic namespaces with '#'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_obj_name WITH '/'.
    ev_obj_name = lv_obj_name.
  ENDMETHOD.

  METHOD zif_abgagt_util~check_log_for_errors.
    rv_has_error = mv_has_error.
  ENDMETHOD.

  METHOD zif_abgagt_util~get_log_detail.
    rv_detail = mv_log.
  ENDMETHOD.

  METHOD zif_abgagt_util~convert_method_index.
    " Convert base-36 string to integer
    " CM001 -> 1, CM00F -> 15, CM00A -> 10

    rv_method_index = 0.
    DATA(lv_len) = strlen( iv_include_name ).

    IF lv_len = 0.
      RETURN.
    ENDIF.

    " Extract the numeric part (after 'CM')

    DATA lv_num_str TYPE string.
    IF lv_len >= 2.
      lv_num_str = iv_include_name+2.
    ELSE.
      lv_num_str = iv_include_name.
    ENDIF.

    " Convert base-36 string to integer (0-9 = 0-9, A-Z = 10-35)

    DATA(lv_num_len) = strlen( lv_num_str ).

    DO lv_num_len TIMES.
      DATA(lv_idx) = sy-index - 1.
      DATA(lv_digit) = lv_num_str+lv_idx(1).
      lv_digit = to_upper( lv_digit ).

      IF lv_digit CO '0123456789'.
        DATA(lv_val) = CONV i( lv_digit ).
      ELSEIF lv_digit CO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
        " Convert A-Z to 10-35 using CASE
        CASE lv_digit.
          WHEN 'A'. lv_val = 10.
          WHEN 'B'. lv_val = 11.
          WHEN 'C'. lv_val = 12.
          WHEN 'D'. lv_val = 13.
          WHEN 'E'. lv_val = 14.
          WHEN 'F'. lv_val = 15.
          WHEN 'G'. lv_val = 16.
          WHEN 'H'. lv_val = 17.
          WHEN 'I'. lv_val = 18.
          WHEN 'J'. lv_val = 19.
          WHEN 'K'. lv_val = 20.
          WHEN 'L'. lv_val = 21.
          WHEN 'M'. lv_val = 22.
          WHEN 'N'. lv_val = 23.
          WHEN 'O'. lv_val = 24.
          WHEN 'P'. lv_val = 25.
          WHEN 'Q'. lv_val = 26.
          WHEN 'R'. lv_val = 27.
          WHEN 'S'. lv_val = 28.
          WHEN 'T'. lv_val = 29.
          WHEN 'U'. lv_val = 30.
          WHEN 'V'. lv_val = 31.
          WHEN 'W'. lv_val = 32.
          WHEN 'X'. lv_val = 33.
          WHEN 'Y'. lv_val = 34.
          WHEN 'Z'. lv_val = 35.
        ENDCASE.
      ELSE.
        CONTINUE.
      ENDIF.

      rv_method_index = rv_method_index * 36 + lv_val.
    ENDDO.
  ENDMETHOD.

  METHOD zif_abgagt_util~convert_index_to_cm_suffix.
    " Convert integer to 3-digit base-36 string for CM include suffix
    " 1 -> 'CM001', 10 -> 'CM00A', 35 -> 'CM00Z', 36 -> 'CM010'

    DATA(lv_quotient) = iv_method_index.
    DATA lv_digits TYPE string.

    DO 3 TIMES.
      DATA(lv_remainder) = lv_quotient MOD 36.
      lv_quotient  = lv_quotient DIV 36.
      IF lv_remainder < 10.
        CASE lv_remainder.
          WHEN 0. lv_digits = '0' && lv_digits.
          WHEN 1. lv_digits = '1' && lv_digits.
          WHEN 2. lv_digits = '2' && lv_digits.
          WHEN 3. lv_digits = '3' && lv_digits.
          WHEN 4. lv_digits = '4' && lv_digits.
          WHEN 5. lv_digits = '5' && lv_digits.
          WHEN 6. lv_digits = '6' && lv_digits.
          WHEN 7. lv_digits = '7' && lv_digits.
          WHEN 8. lv_digits = '8' && lv_digits.
          WHEN 9. lv_digits = '9' && lv_digits.
        ENDCASE.
      ELSE.
        DATA(lv_val) = lv_remainder - 10.
        CASE lv_val.
          WHEN 0.  lv_digits = 'A' && lv_digits.
          WHEN 1.  lv_digits = 'B' && lv_digits.
          WHEN 2.  lv_digits = 'C' && lv_digits.
          WHEN 3.  lv_digits = 'D' && lv_digits.
          WHEN 4.  lv_digits = 'E' && lv_digits.
          WHEN 5.  lv_digits = 'F' && lv_digits.
          WHEN 6.  lv_digits = 'G' && lv_digits.
          WHEN 7.  lv_digits = 'H' && lv_digits.
          WHEN 8.  lv_digits = 'I' && lv_digits.
          WHEN 9.  lv_digits = 'J' && lv_digits.
          WHEN 10. lv_digits = 'K' && lv_digits.
          WHEN 11. lv_digits = 'L' && lv_digits.
          WHEN 12. lv_digits = 'M' && lv_digits.
          WHEN 13. lv_digits = 'N' && lv_digits.
          WHEN 14. lv_digits = 'O' && lv_digits.
          WHEN 15. lv_digits = 'P' && lv_digits.
          WHEN 16. lv_digits = 'Q' && lv_digits.
          WHEN 17. lv_digits = 'R' && lv_digits.
          WHEN 18. lv_digits = 'S' && lv_digits.
          WHEN 19. lv_digits = 'T' && lv_digits.
          WHEN 20. lv_digits = 'U' && lv_digits.
          WHEN 21. lv_digits = 'V' && lv_digits.
          WHEN 22. lv_digits = 'W' && lv_digits.
          WHEN 23. lv_digits = 'X' && lv_digits.
          WHEN 24. lv_digits = 'Y' && lv_digits.
          WHEN 25. lv_digits = 'Z' && lv_digits.
        ENDCASE.
      ENDIF.
    ENDDO.

    rv_cm_suffix = 'CM' && lv_digits.
  ENDMETHOD.

  METHOD zif_abgagt_util~get_method_name.
    SELECT SINGLE methodname
      FROM tmdir
      INTO rv_method_name
      WHERE classname = iv_classname
        AND methodindx = iv_method_index.
  ENDMETHOD.

  METHOD zif_abgagt_util~get_include_description.
    " Get human-readable description of include type
    " Extract include type from full include name
    " Example: ZCL_CLASS=============CM001 -> Class Method
    " Example: ZCL_CLASS=============CCAU -> Unit Test

    rv_description = 'Unknown'.

    DATA(lv_include_len) = strlen( iv_include_name ).
    " Check from longest to shortest (35, 34, 32)
    DATA lv_include TYPE string.
    IF lv_include_len >= 35.
      lv_include = iv_include_name+30(5).
    ELSEIF lv_include_len >= 34.
      lv_include = iv_include_name+30(4).
    ELSEIF lv_include_len >= 32.
      lv_include = iv_include_name+30(2).
    ELSE.
      lv_include = iv_include_name.
    ENDIF.

    CASE lv_include.
      WHEN 'CU'.
        rv_description = 'Public Section'.
      WHEN 'CO'.
        rv_description = 'Protected Section'.
      WHEN 'CP'.
        rv_description = 'Private Section'.
      WHEN 'CCAU'.
        rv_description = 'Unit Test'.
      WHEN 'CCIMP'.
        rv_description = 'Local Implementations'.
      WHEN 'CCDEF'.
        rv_description = 'Local Definitions'.
      WHEN 'CI'.
        rv_description = 'Local Interfaces'.
      WHEN 'CT'.
        rv_description = 'Macros'.
      WHEN 'IU'.
        rv_description = 'Interface Section'.
      WHEN OTHERS.
        " Check if it's a method include (CM###)
        IF strlen( lv_include ) >= 2 AND lv_include(2) = 'CM'.
          rv_description = 'Class Method'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_abgagt_util~detect_include_info.
    " Detect object type, name, and include type from include name
    " Example: ZCL_CLASS=============CM001 -> CLAS, ZCL_CLASS, CM001
    " Example: ZIF_INTERFACE============IU -> INTF, ZIF_INTERFACE, IU

    DATA: lv_name TYPE tadir-obj_name,
          lv_obj_name TYPE tadir-obj_name,
          lt_source_check TYPE STANDARD TABLE OF string.

    rs_info-is_source_include = abap_false.
    rs_info-obj_type = ''.
    rs_info-obj_name = ''.
    rs_info-include_type = ''.
    rs_info-type_text = ''.

    lv_name = iv_name.
    DATA(lv_name_len) = strlen( lv_name ).

    " Check if name looks like a source include (>= 32 chars)
    IF lv_name_len >= 32.
      " Try to read as program/include to verify it exists
      DATA lv_prog TYPE program.
      lv_prog = lv_name.
      READ REPORT lv_prog INTO lt_source_check.
      IF sy-subrc = 0.
        " This is a source include - extract suffix
        DATA lv_suffix TYPE string.
        lv_suffix = lv_name+30.
        rs_info-is_source_include = abap_true.

        " Determine object type based on suffix
        IF lv_name_len = 35 AND lv_suffix(2) = 'CM'.
          " Method implementation include - belongs to a class
          rs_info-obj_type = 'CLAS'.
          lv_obj_name = lv_name(30).
          REPLACE ALL OCCURRENCES OF '=' IN lv_obj_name WITH ''.
          rs_info-obj_name = lv_obj_name.
          rs_info-include_type = lv_suffix.
          rs_info-type_text = 'Class'.
        ELSEIF lv_name_len = 34 AND ( lv_suffix = 'CCAU' OR lv_suffix = 'CCDEF' OR lv_suffix = 'CCIMP' ).
          " Test class or local types - belongs to a class
          rs_info-obj_type = 'CLAS'.
          lv_obj_name = lv_name(30).
          REPLACE ALL OCCURRENCES OF '=' IN lv_obj_name WITH ''.
          rs_info-obj_name = lv_obj_name.
          rs_info-include_type = lv_suffix.
          rs_info-type_text = 'Class'.
        ELSEIF lv_name_len = 32 AND ( lv_suffix = 'CU' OR lv_suffix = 'CO' OR lv_suffix = 'CP' ).
          " Class section - belongs to a class
          rs_info-obj_type = 'CLAS'.
          lv_obj_name = lv_name(30).
          REPLACE ALL OCCURRENCES OF '=' IN lv_obj_name WITH ''.
          rs_info-obj_name = lv_obj_name.
          rs_info-include_type = lv_suffix.
          rs_info-type_text = 'Class'.
        ELSEIF lv_name_len = 32 AND lv_suffix = 'IU'.
          " Interface section - belongs to an interface
          rs_info-obj_type = 'INTF'.
          lv_obj_name = lv_name(30).
          REPLACE ALL OCCURRENCES OF '=' IN lv_obj_name WITH ''.
          rs_info-obj_name = lv_obj_name.
          rs_info-include_type = lv_suffix.
          rs_info-type_text = 'Interface'.
        ELSE.
          " Other program/include - verify it exists in TADIR.
          " #EC CI_GENBUFF: TADIR's single-record buffer can't satisfy this lookup
          " pattern (key not fully specified); Code Inspector reports the bypass
          " as informational — the suppression makes it stay green.
          SELECT SINGLE object FROM tadir              "#EC CI_GENBUFF
            INTO rs_info-obj_type
            WHERE obj_name = lv_name
              AND object = 'PROG'.
          IF sy-subrc = 0.
            rs_info-obj_name = lv_name.
            rs_info-include_type = lv_suffix.
            rs_info-type_text = 'Program'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_util~get_object_info_from_tadir.
    " Get object info from TADIR by object name
    DATA lv_obj_name TYPE tadir-obj_name.
    lv_obj_name = iv_obj_name.
    TRANSLATE lv_obj_name TO UPPER CASE.

    " #EC CI_GENBUFF: WHERE has IN list on OBJECT — single-record buffer can't
    " serve this; the bypass is intended.
    SELECT SINGLE object obj_name devclass FROM tadir   "#EC CI_GENBUFF
      INTO (rs_info-obj_type, rs_info-obj_name, rs_info-devclass)
      WHERE obj_name = lv_obj_name
        AND object IN ('CLAS', 'INTF', 'TABL', 'DTEL', 'STRU', 'TTYP', 'DDLS', 'PROG', 'STOB', 'ENHO').

    IF sy-subrc <> 0.
      SELECT object AS obj_type, obj_name, devclass FROM tadir
        WHERE pgmid = 'R3TR' AND obj_name = @lv_obj_name
          AND object IN ( 'BDEF', 'SRVD', 'DDLX' )
        ORDER BY object
        INTO TABLE @DATA(lt_matches)
        UP TO 1 ROWS.
      IF lt_matches IS NOT INITIAL.
        rs_info = CORRESPONDING #( lt_matches[ 1 ] ).
      ENDIF.
    ENDIF.

    CASE rs_info-obj_type.
      WHEN 'CLAS'. rs_info-type_text = 'Class'.
      WHEN 'INTF'. rs_info-type_text = 'Interface'.
      WHEN 'TABL'. rs_info-type_text = 'Table'.
      WHEN 'STRU'. rs_info-type_text = 'Structure'.
      WHEN 'DTEL'. rs_info-type_text = 'Data Element'.
      WHEN 'TTYP'. rs_info-type_text = 'Table Type'.
      WHEN 'DDLS'. rs_info-type_text = 'CDS View'.
      WHEN 'PROG'. rs_info-type_text = 'Program'.
      WHEN 'STOB'. rs_info-type_text = 'Structured Object'.
      WHEN 'ENHO'. rs_info-type_text = 'Enhancement'.
      WHEN 'BDEF'. rs_info-type_text = 'Behavior Definition'.
      WHEN 'SRVD'. rs_info-type_text = 'Service Definition'.
      WHEN 'DDLX'. rs_info-type_text = 'CDS Metadata Extension'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
