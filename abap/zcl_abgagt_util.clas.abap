"* Use this source text module for the class definition
CLASS zcl_abgagt_util DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_util.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_util) TYPE REF TO zcl_abgagt_util.
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
ENDCLASS.
