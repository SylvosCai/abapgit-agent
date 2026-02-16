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
          lv_keydef TYPE string,
          lv_access_mode TYPE string,
          lv_key_definition TYPE string.

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

    " Get TTYP details from DD40V
    SELECT SINGLE rowtype accessmode keydef FROM dd40v
      INTO (lv_linetype, lv_tabprottype, lv_keydef)
      WHERE typename = iv_name.

    " Build components table with TTYP details
    IF lv_linetype IS NOT INITIAL.
      APPEND VALUE #(
        field = 'LINE_TYPE'
        key = abap_false
        type = 'CHAR'
        length = 30
        dataelement = ''
        description = |Line Type: { lv_linetype }|
      ) TO rs_info-components.
    ENDIF.

    " Convert access mode to text
    CASE lv_tabprottype.
      WHEN '1'. lv_access_mode = 'STANDARD'.
      WHEN '2'. lv_access_mode = 'SORTED'.
      WHEN '3'. lv_access_mode = 'HASHED'.
      WHEN OTHERS. lv_access_mode = lv_tabprottype.
    ENDCASE.

    IF lv_access_mode IS NOT INITIAL.
      APPEND VALUE #(
        field = 'ACCESS_MODE'
        key = abap_false
        type = 'CHAR'
        length = 10
        dataelement = ''
        description = |Access Mode: { lv_access_mode }|
      ) TO rs_info-components.
    ENDIF.

    " Convert key definition to text
    CASE lv_keydef.
      WHEN '1'. lv_key_definition = 'WITH KEY'.
      WHEN '2'. lv_key_definition = 'NO KEY'.
      WHEN OTHERS. lv_key_definition = lv_keydef.
    ENDCASE.

    IF lv_key_definition IS NOT INITIAL.
      APPEND VALUE #(
        field = 'KEY_DEFINITION'
        key = abap_false
        type = 'CHAR'
        length = 10
        dataelement = ''
        description = |Key Definition: { lv_key_definition }|
      ) TO rs_info-components.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
