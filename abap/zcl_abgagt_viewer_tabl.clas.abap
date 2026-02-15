*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_tabl DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_tabl IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass.

    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = iv_name
        AND object = 'TABL'.
    IF sy-subrc = 0.
      rs_info-name = iv_name.
      rs_info-type = 'TABL'.
      rs_info-type_text = 'Table'.
      rs_info-description = |Table { iv_name } in { lv_devclass }|.
    ENDIF.

    " Build field list as JSON array
    DATA lv_json TYPE string.
    lv_json = '['.

    SELECT fieldname position datatype leng FROM dd03l
      WHERE tabname = @iv_name
        AND as4local = 'A'
      ORDER BY position
      INTO TABLE @DATA(lt_fields).

    DATA lv_first TYPE abap_bool VALUE abap_true.
    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      IF lv_first = abap_false.
        lv_json = lv_json && ','.
      ENDIF.
      lv_first = abap_false.
      lv_json = lv_json && |\{\"fieldname\":\"{<ls_field>-fieldname}\",\"position\":{<ls_field>-position},\"datatype\":\"{<ls_field>-datatype}\",\"length\":{<ls_field>-leng}\}|.
    ENDLOOP.

    lv_json = lv_json && ']'.
    rs_info-details = lv_json.
  ENDMETHOD.

ENDCLASS.
