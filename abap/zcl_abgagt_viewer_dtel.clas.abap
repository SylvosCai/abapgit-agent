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
      DATA ls_comp_dom LIKE LINE OF rs_info-components.
      CLEAR ls_comp_dom.
      ls_comp_dom-field       = 'DOMAIN'.
      ls_comp_dom-key         = abap_false.
      ls_comp_dom-type        = rs_info-domain_type.
      ls_comp_dom-length      = rs_info-domain_length.
      ls_comp_dom-dataelement = rs_info-domain.
      ls_comp_dom-description = |Domain: { rs_info-domain }|.
      APPEND ls_comp_dom TO rs_info-components.
    ENDIF.

    DATA ls_comp_type LIKE LINE OF rs_info-components.
    CLEAR ls_comp_type.
    ls_comp_type-field       = 'DATA_TYPE'.
    ls_comp_type-key         = abap_false.
    ls_comp_type-type        = 'CHAR'.
    ls_comp_type-length      = 10.
    ls_comp_type-dataelement = ''.
    ls_comp_type-description = |ABAP Type: { rs_info-domain_type }|.
    APPEND ls_comp_type TO rs_info-components.

    IF rs_info-domain_length > 0.
      DATA ls_comp_len LIKE LINE OF rs_info-components.
      CLEAR ls_comp_len.
      ls_comp_len-field       = 'LENGTH'.
      ls_comp_len-key         = abap_false.
      ls_comp_len-type        = 'NUMC'.
      ls_comp_len-length      = 5.
      ls_comp_len-dataelement = ''.
      ls_comp_len-description = |Length: { rs_info-domain_length }|.
      APPEND ls_comp_len TO rs_info-components.
    ENDIF.

    IF rs_info-domain_decimals > 0.
      DATA ls_comp_dec LIKE LINE OF rs_info-components.
      CLEAR ls_comp_dec.
      ls_comp_dec-field       = 'DECIMALS'.
      ls_comp_dec-key         = abap_false.
      ls_comp_dec-type        = 'NUMC'.
      ls_comp_dec-length      = 2.
      ls_comp_dec-dataelement = ''.
      ls_comp_dec-description = |Decimals: { rs_info-domain_decimals }|.
      APPEND ls_comp_dec TO rs_info-components.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
