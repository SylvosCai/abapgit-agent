*"*"use source
*"*"Local Interface:
**********************************************************************
CLASS zcl_abgagt_viewer_suso DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_suso IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_devclass TYPE tadir-devclass,
          lv_ttext    TYPE tobjt-ttext,
          ls_tobj     TYPE tobj.

    rs_info-name      = iv_name.
    rs_info-type      = 'SUSO'.
    rs_info-type_text = 'Authorization Object'.

    " Check TADIR for existence
    SELECT SINGLE devclass FROM tadir
      INTO @lv_devclass
      WHERE pgmid    = 'R3TR'
        AND obj_name = @iv_name
        AND object   = 'SUSO'.
    IF sy-subrc <> 0.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.
    rs_info-devclass = lv_devclass.

    " Get auth object record (fields + object class)
    SELECT SINGLE * FROM tobj
      INTO @ls_tobj
      WHERE objct = @iv_name.

    " Get description
    SELECT SINGLE ttext FROM tobjt
      INTO @lv_ttext
      WHERE object = @iv_name
        AND langu  = 'E'.

    rs_info-description = COND #(
      WHEN lv_ttext IS NOT INITIAL THEN lv_ttext
      ELSE |Authorization Object { iv_name }|
    ).

    " Object class
    IF ls_tobj-oclss IS NOT INITIAL.
      APPEND VALUE #(
        field       = 'OCLSS'
        key         = abap_false
        type        = 'CHAR'
        length      = 4
        dataelement = 'XUCLASS'
        description = |Object Class: { ls_tobj-oclss }|
      ) TO rs_info-components.
    ENDIF.

    " Authorization fields (FIEL1..FIEL0)
    DATA(lt_fields) = VALUE string_table(
      ( CONV #( ls_tobj-fiel1 ) )
      ( CONV #( ls_tobj-fiel2 ) )
      ( CONV #( ls_tobj-fiel3 ) )
      ( CONV #( ls_tobj-fiel4 ) )
      ( CONV #( ls_tobj-fiel5 ) )
      ( CONV #( ls_tobj-fiel6 ) )
      ( CONV #( ls_tobj-fiel7 ) )
      ( CONV #( ls_tobj-fiel8 ) )
      ( CONV #( ls_tobj-fiel9 ) )
      ( CONV #( ls_tobj-fiel0 ) )
    ).

    LOOP AT lt_fields INTO DATA(lv_field) WHERE table_line IS NOT INITIAL.
      APPEND VALUE #(
        field       = |FIELD_{ sy-tabix }|
        key         = abap_false
        type        = 'CHAR'
        length      = 30
        dataelement = 'FIELDNAME'
        description = |Field: { lv_field }|
      ) TO rs_info-components.
    ENDLOOP.

    " Allowed activities from TACTZ (buffered — read separately to use buffer)
    SELECT brobj, actvt FROM tactz
      INTO TABLE @DATA(lt_tactz)
      WHERE brobj = @iv_name
      ORDER BY actvt.

    " Activity texts from TACTT (buffered — read separately to use buffer)
    DATA lt_actvt_vals TYPE RANGE OF tactz-actvt.
    lt_actvt_vals = VALUE #( FOR ls IN lt_tactz ( sign = 'I' option = 'EQ' low = ls-actvt ) ).

    DATA lt_tactt TYPE HASHED TABLE OF tactt WITH UNIQUE KEY actvt.
    IF lt_actvt_vals IS NOT INITIAL.
      SELECT spras, actvt, ltext FROM tactt
        INTO TABLE @lt_tactt
        WHERE actvt IN @lt_actvt_vals
          AND spras = 'E'.
    ENDIF.

    LOOP AT lt_tactz INTO DATA(ls_tactz).
      DATA(lv_ltext) = VALUE tactt-ltext( lt_tactt[ actvt = ls_tactz-actvt ]-ltext DEFAULT '' ).
      APPEND VALUE #(
        field       = |ACTVT_{ ls_tactz-actvt }|
        key         = abap_false
        type        = 'CHAR'
        length      = 2
        dataelement = 'ACTVT'
        description = |Activity { ls_tactz-actvt }{ COND #( WHEN lv_ltext IS NOT INITIAL THEN | - { lv_ltext }| ELSE || ) }|
      ) TO rs_info-components.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
