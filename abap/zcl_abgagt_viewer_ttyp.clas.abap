*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_ttyp DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_ttyp IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass,
          lv_linetype TYPE string,
          lv_tabprottype TYPE string,
          lv_keydef TYPE string.

    rs_info-name = iv_name.
    rs_info-type = 'TTYP'.
    rs_info-type_text = 'Table Type'.

    " Get package from TADIR
    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = iv_name
        AND object = 'TTYP'.
    IF sy-subrc = 0.
      rs_info-description = |Table Type { iv_name } in { lv_devclass }|.
    ELSE.
      rs_info-not_found = abap_true.
    ENDIF.

    " Get TTYP details from DD40L
    SELECT SINGLE rowtype accessmode keydef FROM dd40l
      INTO (lv_linetype, lv_tabprottype, lv_keydef)
      WHERE typename = iv_name
        AND as4local = 'A'.

    " Build components table with TTYP details
    IF lv_linetype IS NOT INITIAL.
      DATA ls_comp_lt LIKE LINE OF rs_info-components.
      CLEAR ls_comp_lt.
      ls_comp_lt-field       = 'LINE_TYPE'.
      ls_comp_lt-key         = abap_false.
      ls_comp_lt-type        = 'CHAR'.
      ls_comp_lt-length      = 30.
      ls_comp_lt-dataelement = ''.
      ls_comp_lt-description = |Line Type: { lv_linetype }|.
      APPEND ls_comp_lt TO rs_info-components.
    ENDIF.

    " Convert access mode to text
    CASE lv_tabprottype.
      WHEN 'T'. DATA(lv_access_mode) = 'STANDARD'.
      WHEN 'S'. lv_access_mode = 'SORTED'.
      WHEN 'H'. lv_access_mode = 'HASHED'.
      WHEN OTHERS. lv_access_mode = lv_tabprottype.
    ENDCASE.

    IF lv_access_mode IS NOT INITIAL.
      DATA ls_comp_am LIKE LINE OF rs_info-components.
      CLEAR ls_comp_am.
      ls_comp_am-field       = 'ACCESS_MODE'.
      ls_comp_am-key         = abap_false.
      ls_comp_am-type        = 'CHAR'.
      ls_comp_am-length      = 10.
      ls_comp_am-dataelement = ''.
      ls_comp_am-description = |Access Mode: { lv_access_mode }|.
      APPEND ls_comp_am TO rs_info-components.
    ENDIF.

    " Convert key definition to text
    CASE lv_keydef.
      WHEN 'D'. DATA(lv_key_definition) = 'WITH KEY'.
      WHEN 'N'. lv_key_definition = 'NO KEY'.
      WHEN OTHERS. lv_key_definition = lv_keydef.
    ENDCASE.

    IF lv_key_definition IS NOT INITIAL.
      DATA ls_comp_kd LIKE LINE OF rs_info-components.
      CLEAR ls_comp_kd.
      ls_comp_kd-field       = 'KEY_DEFINITION'.
      ls_comp_kd-key         = abap_false.
      ls_comp_kd-type        = 'CHAR'.
      ls_comp_kd-length      = 10.
      ls_comp_kd-dataelement = ''.
      ls_comp_kd-description = |Key Definition: { lv_key_definition }|.
      APPEND ls_comp_kd TO rs_info-components.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
