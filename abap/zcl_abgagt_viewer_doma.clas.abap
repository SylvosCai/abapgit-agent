*"*"use source
*"*"Local Interface:
**********************************************************************
CLASS zcl_abgagt_viewer_doma DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_doma IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    TYPES:
      BEGIN OF ty_value,
        valpos     TYPE dd07l-valpos,
        domvalue_l TYPE dd07l-domvalue_l,
        domvalue_h TYPE dd07l-domvalue_h,
        ddtext     TYPE dd07t-ddtext,
      END OF ty_value.

    DATA: lv_devclass  TYPE tadir-devclass,
          lv_datatype  TYPE dd01l-datatype,
          lv_leng      TYPE dd01l-leng,
          lv_decimals  TYPE dd01l-decimals,
          lv_ddtext    TYPE dd01t-ddtext,
          lv_convexit  TYPE dd01l-convexit,
          lv_entitytab TYPE dd01l-entitytab,
          lt_values    TYPE STANDARD TABLE OF ty_value,
          ls_val       TYPE ty_value.

    rs_info-name      = iv_name.
    rs_info-type      = 'DOMA'.
    rs_info-type_text = 'Domain'.

    " Get package from TADIR
    SELECT SINGLE devclass FROM tadir
      INTO lv_devclass
      WHERE obj_name = iv_name
        AND object   = 'DOMA'.
    IF sy-subrc <> 0.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.

    " Get domain definition: join DD01L (structure) with DD01T (text, English first)
    SELECT SINGLE l~datatype, l~leng, l~decimals, l~convexit, l~entitytab, t~ddtext
      INTO (@lv_datatype, @lv_leng, @lv_decimals, @lv_convexit, @lv_entitytab, @lv_ddtext)
      FROM dd01l AS l
      LEFT JOIN dd01t AS t
        ON t~domname    = l~domname
       AND t~as4local   = l~as4local
       AND t~ddlanguage = 'E'
      WHERE l~domname  = @iv_name
        AND l~as4local = 'A'.
    IF sy-subrc <> 0.
      " Try without language filter
      SELECT SINGLE l~datatype, l~leng, l~decimals, l~convexit, l~entitytab
        INTO (@lv_datatype, @lv_leng, @lv_decimals, @lv_convexit, @lv_entitytab)
        FROM dd01l AS l
        WHERE l~domname  = @iv_name
          AND l~as4local = 'A'.
    ENDIF.

    rs_info-description     = COND #( WHEN lv_ddtext IS NOT INITIAL THEN lv_ddtext
                                      ELSE |Domain { iv_name }| ).
    rs_info-devclass        = lv_devclass.
    rs_info-domain_type     = lv_datatype.
    rs_info-domain_length   = lv_leng.
    rs_info-domain_decimals = lv_decimals.

    " Fixed values: join DD07L (values) with DD07T (texts, English)
    SELECT l~valpos, l~domvalue_l, l~domvalue_h, t~ddtext
      INTO TABLE @lt_values
      FROM dd07l AS l
      LEFT JOIN dd07t AS t
        ON t~domname    = l~domname
       AND t~as4local   = l~as4local
       AND t~valpos     = l~valpos
       AND t~ddlanguage = 'E'
      WHERE l~domname  = @iv_name
        AND l~as4local = 'A'
      ORDER BY l~valpos.

    " Basic info components
    APPEND VALUE #(
      field       = 'DATA_TYPE'
      key         = abap_false
      type        = 'CHAR'
      length      = 10
      dataelement = ''
      description = |Type: { lv_datatype }|
    ) TO rs_info-components.

    IF lv_leng > 0.
      APPEND VALUE #(
        field       = 'LENGTH'
        key         = abap_false
        type        = 'NUMC'
        length      = 5
        dataelement = ''
        description = |Length: { lv_leng }|
      ) TO rs_info-components.
    ENDIF.

    IF lv_decimals > 0.
      APPEND VALUE #(
        field       = 'DECIMALS'
        key         = abap_false
        type        = 'NUMC'
        length      = 2
        dataelement = ''
        description = |Decimals: { lv_decimals }|
      ) TO rs_info-components.
    ENDIF.

    IF lv_convexit IS NOT INITIAL.
      APPEND VALUE #(
        field       = 'CONV_EXIT'
        key         = abap_false
        type        = 'CHAR'
        length      = 5
        dataelement = ''
        description = |Conversion Exit: { lv_convexit }|
      ) TO rs_info-components.
    ENDIF.

    IF lv_entitytab IS NOT INITIAL.
      APPEND VALUE #(
        field       = 'VALUE_TABLE'
        key         = abap_false
        type        = 'CHAR'
        length      = 30
        dataelement = ''
        description = |Value Table: { lv_entitytab }|
      ) TO rs_info-components.
    ENDIF.

    " Fixed domain values
    LOOP AT lt_values INTO ls_val.
      DATA(lv_range) = ls_val-domvalue_l.
      IF ls_val-domvalue_h IS NOT INITIAL.
        lv_range = |{ ls_val-domvalue_l } - { ls_val-domvalue_h }|.
      ENDIF.
      APPEND VALUE #(
        field       = |VALUE_{ ls_val-valpos }|
        key         = abap_false
        type        = 'CHAR'
        length      = 30
        dataelement = ''
        description = |{ lv_range }: { ls_val-ddtext }|
      ) TO rs_info-components.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
