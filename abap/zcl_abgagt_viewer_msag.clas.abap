*"*"use source
*"*"Local Interface:
**********************************************************************
CLASS zcl_abgagt_viewer_msag DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_msag IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    TYPES:
      BEGIN OF ty_msg,
        sprsl TYPE t100-sprsl,
        arbgb TYPE t100-arbgb,
        msgnr TYPE t100-msgnr,
        text  TYPE t100-text,
      END OF ty_msg.

    DATA: lv_devclass TYPE tadir-devclass,
          lv_stext    TYPE t100a-stext,
          lt_msgs     TYPE STANDARD TABLE OF ty_msg,
          ls_msg      TYPE ty_msg.

    rs_info-name      = iv_name.
    rs_info-type      = 'MSAG'.
    rs_info-type_text = 'Message Class'.

    " Get package from TADIR
    SELECT SINGLE devclass FROM tadir
      INTO lv_devclass
      WHERE obj_name = iv_name
        AND object   = 'MSAG'.
    IF sy-subrc <> 0.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.

    rs_info-devclass = lv_devclass.

    SELECT SINGLE stext FROM t100a
      INTO lv_stext
      WHERE arbgb = iv_name.
    rs_info-description = COND #(
      WHEN lv_stext IS NOT INITIAL THEN lv_stext
      ELSE |Message Class { iv_name }| ).

    " Get messages from T100 (English first, then any language)
    SELECT sprsl, arbgb, msgnr, text
      INTO TABLE @lt_msgs
      FROM t100
      WHERE arbgb = @iv_name
        AND sprsl = 'E'
      ORDER BY msgnr.
    IF sy-subrc <> 0.
      SELECT sprsl, arbgb, msgnr, text
        INTO TABLE @lt_msgs
        FROM t100
        WHERE arbgb = @iv_name
        ORDER BY msgnr.
    ENDIF.

    " Each message becomes a component row
    LOOP AT lt_msgs INTO ls_msg.
      APPEND VALUE #(
        field       = ls_msg-msgnr
        key         = abap_false
        type        = 'CHAR'
        length      = 73
        dataelement = ''
        description = |{ ls_msg-msgnr }: { ls_msg-text }|
      ) TO rs_info-components.
    ENDLOOP.

    " Summary: total message count
    APPEND VALUE #(
      field       = 'COUNT'
      key         = abap_false
      type        = 'INT4'
      length      = 4
      dataelement = ''
      description = |Total messages: { lines( lt_msgs ) }|
    ) TO rs_info-components.
  ENDMETHOD.

ENDCLASS.
