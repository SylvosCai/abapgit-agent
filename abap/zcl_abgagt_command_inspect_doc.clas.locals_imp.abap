*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type declarations

"-----------------------------------------------------------------------
" Production implementation — delegates to real FM / static class / DB
"-----------------------------------------------------------------------
CLASS lcl_docu_db IMPLEMENTATION.
  METHOD zif_abgagt_docu_db~get_ktd_html.
    rv_html = cl_satc_ac_ui_ktd_docu=>get_ktd_docu_for_gui_if_exist(
      check_class_name = iv_class
      message_code     = iv_code ).
  ENDMETHOD.

  METHOD zif_abgagt_docu_db~get_docu.
    DATA lv_head TYPE thead.
    CALL FUNCTION 'DOCU_GET_FOR_F1HELP'
      EXPORTING
        id     = iv_id
        langu  = iv_langu
        object = iv_object
      IMPORTING
        head   = lv_head
      TABLES
        line   = ct_lines
      EXCEPTIONS
        OTHERS = 0.
    ev_title = lv_head-tdtitle.
  ENDMETHOD.

  METHOD zif_abgagt_docu_db~get_dokil_for_candidates.
    IF it_cands IS INITIAL.
      RETURN.
    ENDIF.
    SELECT id, object, langu FROM dokil "#EC CI_GENBUFF
      INTO CORRESPONDING FIELDS OF TABLE @rt_hits
      FOR ALL ENTRIES IN @it_cands
      WHERE id = @it_cands-id AND object = @it_cands-object AND typ = 'E'.
  ENDMETHOD.

  METHOD zif_abgagt_docu_db~get_dokil_for_includes.
    IF it_objects IS INITIAL.
      RETURN.
    ENDIF.
    DATA lt_objs TYPE STANDARD TABLE OF doku_obj WITH DEFAULT KEY.
    lt_objs = it_objects.
    SELECT id, object, langu FROM dokil "#EC CI_GENBUFF
      INTO CORRESPONDING FIELDS OF TABLE @rt_hits
      FOR ALL ENTRIES IN @lt_objs
      WHERE object = @lt_objs-table_line AND typ = 'E'.
  ENDMETHOD.
ENDCLASS.
