*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_dtel DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_dtel IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass,
          lv_leng TYPE dd04v-leng,
          lv_decimals TYPE dd04v-decimals.

    rs_info-name = iv_name.
    rs_info-type = 'DTEL'.
    rs_info-type_text = 'Data Element'.

    " Get package from TADIR
    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = iv_name
        AND object = 'DTEL'.
    IF sy-subrc = 0.
      rs_info-description = |Data Element { iv_name } in { lv_devclass }|.
    ELSE.
      rs_info-not_found = abap_true.
    ENDIF.

    " Get data element details from DD04V
    SELECT SINGLE dd04v~rollname dd04v~ddtext dd04v~domname
                  dd04v~datatype dd04v~leng dd04v~decimals
      INTO (rs_info-name, rs_info-description, rs_info-domain,
            rs_info-domain_type, lv_leng, lv_decimals)
      FROM dd04v
      WHERE rollname = iv_name
        AND ddlanguage = 'E'.
    IF sy-subrc <> 0.
      " Fallback: try without language filter
      SELECT SINGLE rollname ddtext domname datatype leng decimals
        INTO (rs_info-name, rs_info-description, rs_info-domain,
              rs_info-domain_type, lv_leng, lv_decimals)
        FROM dd04v
        WHERE rollname = iv_name.
    ENDIF.

    rs_info-domain_length = lv_leng.
    rs_info-domain_decimals = lv_decimals.

    " Build components table for additional info
    IF rs_info-domain IS NOT INITIAL.
      APPEND VALUE #(
        field = 'DOMAIN'
        key = abap_false
        type = rs_info-domain_type
        length = rs_info-domain_length
        dataelement = rs_info-domain
        description = |Domain: { rs_info-domain }|
      ) TO rs_info-components.
    ENDIF.

    APPEND VALUE #(
      field = 'DATA_TYPE'
      key = abap_false
      type = 'CHAR'
      length = 10
      dataelement = ''
      description = |ABAP Type: { rs_info-domain_type }|
    ) TO rs_info-components.

    IF rs_info-domain_length > 0.
      APPEND VALUE #(
        field = 'LENGTH'
        key = abap_false
        type = 'NUMC'
        length = 5
        dataelement = ''
        description = |Length: { rs_info-domain_length }|
      ) TO rs_info-components.
    ENDIF.

    IF rs_info-domain_decimals > 0.
      APPEND VALUE #(
        field = 'DECIMALS'
        key = abap_false
        type = 'NUMC'
        length = 2
        dataelement = ''
        description = |Decimals: { rs_info-domain_decimals }|
      ) TO rs_info-components.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
