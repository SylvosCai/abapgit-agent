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
    CREATE OBJECT lo_util.
    ro_util = lo_util.
  ENDMETHOD.

  METHOD zif_abgagt_util~parse_file_to_object.
    " Parse file path to extract obj_type and obj_name
    " Example: "zcl_my_class.clas.abap" -> CLAS, ZCL_MY_CLASS
    " Example: "src/zcl_my_class.clas.abap" -> CLAS, ZCL_MY_CLASS

    DATA lv_upper TYPE string.
    lv_upper = iv_file.
    TRANSLATE lv_upper TO UPPER CASE.

    " Split filename by '.' to get parts
    DATA lt_parts TYPE TABLE OF string.
    SPLIT lv_upper AT '.' INTO TABLE lt_parts.
    DATA lv_part_count TYPE i.
    lv_part_count = lines( lt_parts ).

    IF lv_part_count < 3.
      RETURN.
    ENDIF.

    " Last part should be 'ABAP' or 'ASDDLS' for verification
    READ TABLE lt_parts INDEX lv_part_count INTO DATA(lv_last).
    IF lv_last <> 'ABAP' AND lv_last <> 'ASDDLS'.
      RETURN.
    ENDIF.

    " First part is obj_name (may contain path), second part is obj_type
    DATA lv_obj_name TYPE string.
    DATA lv_obj_type_raw TYPE string.
    READ TABLE lt_parts INDEX 1 INTO lv_obj_name.
    READ TABLE lt_parts INDEX 2 INTO lv_obj_type_raw.

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

    " Extract file name from obj_name (remove path prefix)
    DATA lv_len TYPE i.
    lv_len = strlen( lv_obj_name ).
    DATA lv_offs TYPE i.
    lv_offs = find( val = reverse( lv_obj_name ) sub = '/' ).
    IF lv_offs > 0.
      lv_offs = lv_len - lv_offs - 1.
      lv_obj_name = lv_obj_name+lv_offs.
    ENDIF.

    " Remove leading '/' if present
    IF lv_obj_name(1) = '/'.
      lv_obj_name = lv_obj_name+1.
    ENDIF.

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
    DATA lv_len TYPE i.
    DATA lv_idx TYPE i.
    DATA lv_val TYPE i.
    DATA lv_digit TYPE c.

    rv_method_index = 0.
    lv_len = strlen( iv_include_name ).

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
    DATA lv_num_len TYPE i.
    lv_num_len = strlen( lv_num_str ).

    DO lv_num_len TIMES.
      lv_idx = sy-index - 1.
      lv_digit = lv_num_str+lv_idx(1).
      lv_digit = to_upper( lv_digit ).

      IF lv_digit CO '0123456789'.
        lv_val = CONV i( lv_digit ).
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

  METHOD zif_abgagt_util~get_method_name.
    " Get method name from TMDIR by class name and method index
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

    DATA lv_include_len TYPE i.
    DATA lv_include TYPE string.

    rv_description = 'Unknown'.

    lv_include_len = strlen( iv_include_name ).
    " Check from longest to shortest (35, 34, 32)
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
          lv_name_len TYPE i,
          lv_suffix TYPE string,
          lt_source_check TYPE STANDARD TABLE OF string.

    rs_info-is_source_include = abap_false.
    rs_info-obj_type = ''.
    rs_info-obj_name = ''.
    rs_info-include_type = ''.
    rs_info-type_text = ''.

    lv_name = iv_name.
    lv_name_len = strlen( lv_name ).

    " Check if name looks like a source include (>= 32 chars)
    IF lv_name_len >= 32.
      " Try to read as program/include to verify it exists
      DATA lv_prog TYPE program.
      lv_prog = lv_name.
      READ REPORT lv_prog INTO lt_source_check.
      IF sy-subrc = 0.
        " This is a source include - extract suffix
        lv_suffix = lv_name+30.
        rs_info-is_source_include = abap_true.

        " Determine object type based on suffix
        IF lv_name_len = 35 AND lv_suffix(2) = 'CM'.
          " Method implementation include - belongs to a class
          rs_info-obj_type = 'CLAS'.
          lv_obj_name = lv_name(30).
          SHIFT lv_obj_name RIGHT DELETING TRAILING '='.
          rs_info-obj_name = lv_obj_name.
          rs_info-include_type = lv_suffix.
          rs_info-type_text = 'Class'.
        ELSEIF lv_name_len = 34 AND ( lv_suffix = 'CCAU' OR lv_suffix = 'CCDEF' OR lv_suffix = 'CCIMP' ).
          " Test class or local types - belongs to a class
          rs_info-obj_type = 'CLAS'.
          lv_obj_name = lv_name(30).
          SHIFT lv_obj_name RIGHT DELETING TRAILING '='.
          rs_info-obj_name = lv_obj_name.
          rs_info-include_type = lv_suffix.
          rs_info-type_text = 'Class'.
        ELSEIF lv_name_len = 32 AND ( lv_suffix = 'CU' OR lv_suffix = 'CO' OR lv_suffix = 'CP' ).
          " Class section - belongs to a class
          rs_info-obj_type = 'CLAS'.
          lv_obj_name = lv_name(30).
          SHIFT lv_obj_name RIGHT DELETING TRAILING '='.
          rs_info-obj_name = lv_obj_name.
          rs_info-include_type = lv_suffix.
          rs_info-type_text = 'Class'.
        ELSEIF lv_name_len = 32 AND lv_suffix = 'IU'.
          " Interface section - belongs to an interface
          rs_info-obj_type = 'INTF'.
          lv_obj_name = lv_name(30).
          SHIFT lv_obj_name RIGHT DELETING TRAILING '='.
          rs_info-obj_name = lv_obj_name.
          rs_info-include_type = lv_suffix.
          rs_info-type_text = 'Interface'.
        ELSE.
          " Other program/include - verify it exists in TADIR
          SELECT SINGLE object FROM tadir
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
    SELECT SINGLE object obj_name devclass FROM tadir
      INTO (rs_info-obj_type, rs_info-obj_name, rs_info-devclass)
      WHERE obj_name = iv_obj_name
        AND object IN ('CLAS', 'INTF', 'TABL', 'DTEL', 'STRU', 'TTYP', 'DDLS', 'PROG').

    CASE rs_info-obj_type.
      WHEN 'CLAS'. rs_info-type_text = 'Class'.
      WHEN 'INTF'. rs_info-type_text = 'Interface'.
      WHEN 'TABL'. rs_info-type_text = 'Table'.
      WHEN 'STRU'. rs_info-type_text = 'Structure'.
      WHEN 'DTEL'. rs_info-type_text = 'Data Element'.
      WHEN 'TTYP'. rs_info-type_text = 'Table Type'.
      WHEN 'DDLS'. rs_info-type_text = 'CDS View'.
      WHEN 'PROG'. rs_info-type_text = 'Program'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
