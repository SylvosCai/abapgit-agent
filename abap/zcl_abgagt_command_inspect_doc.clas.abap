*"*"use source
*"*"Local Interface:
**********************************************************************
" INSPECT_DOC command - fetch SCI check documentation
CLASS zcl_abgagt_command_inspect_doc DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_inspect_doc_params,
             check_class TYPE string,
             check_code  TYPE string,
             severity    TYPE string,
           END OF ty_inspect_doc_params.

    TYPES: BEGIN OF ty_inspect_doc_result,
             title TYPE string,
             text  TYPE string,
           END OF ty_inspect_doc_result.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_inc_doc,
             obj   TYPE doku_obj,
             lines TYPE tlinetab,
           END OF ty_inc_doc.
    TYPES ty_inc_doc_tab TYPE STANDARD TABLE OF ty_inc_doc WITH DEFAULT KEY.

    CLASS-METHODS find_docu_key
      IMPORTING iv_class  TYPE string
                iv_code   TYPE string
      EXPORTING ev_id     TYPE doku_id
                ev_object TYPE doku_obj
                ev_langu  TYPE sylangu.

    CLASS-METHODS itf_lines_to_text
      IMPORTING it_lines   TYPE tlinetab
                it_inc_doc TYPE ty_inc_doc_tab
      RETURNING VALUE(rv_text) TYPE string.

    CLASS-METHODS append_line
      IMPORTING iv_fmt     TYPE c
                iv_line    TYPE tdline
      CHANGING  cv_text    TYPE string.

ENDCLASS.

CLASS zcl_abgagt_command_inspect_doc IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_inspect_doc.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_inspect_doc_params,
          ls_result TYPE ty_inspect_doc_result.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-check_class IS INITIAL OR ls_params-check_code IS INITIAL.
      ls_result-title = 'Error'.
      ls_result-text  = 'check_class and check_code are required'.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    TRY.
        " Try KTD documentation first (newer SAP systems)
        DATA(lv_html) = cl_satc_ac_ui_ktd_docu=>get_ktd_docu_for_gui_if_exist(
          check_class_name = CONV #( ls_params-check_class )
          message_code     = CONV #( ls_params-check_code ) ).

        IF lv_html IS NOT INITIAL.
          ls_result-title = ls_params-check_class && '/' && ls_params-check_code.
          DATA(lv_text) = lv_html.
          DATA: lv_before_style TYPE string,
                lv_after_style  TYPE string,
                lv_style_inner  TYPE string.
          SPLIT lv_text AT '<style' INTO lv_before_style lv_style_inner.
          IF sy-subrc = 0.
            SPLIT lv_style_inner AT '</style>' INTO lv_style_inner lv_after_style.
            lv_text = lv_before_style && lv_after_style.
          ENDIF.
          REPLACE ALL OCCURRENCES OF PCRE '<[^>]+>' IN lv_text WITH ''.
          ls_result-text = lv_text.
          rv_result = /ui2/cl_json=>serialize( data = ls_result ).
          RETURN.
        ENDIF.

        " Fall back: call DOCU_GET_FOR_F1HELP directly
        DATA: lv_docu_id     TYPE doku_id,
              lv_docu_object TYPE doku_obj,
              lv_langu       TYPE sylangu,
              lv_header      TYPE thead,
              lt_lines       TYPE tlinetab.

        find_docu_key(
          EXPORTING iv_class  = ls_params-check_class
                    iv_code   = ls_params-check_code
          IMPORTING ev_id     = lv_docu_id
                    ev_object = lv_docu_object
                    ev_langu  = lv_langu ).

        IF lv_docu_object IS NOT INITIAL.
          CALL FUNCTION 'DOCU_GET_FOR_F1HELP'
            EXPORTING
              id     = lv_docu_id
              langu  = lv_langu
              object = lv_docu_object
            IMPORTING
              head   = lv_header
            TABLES
              line   = lt_lines
            EXCEPTIONS
              OTHERS = 0.
        ENDIF.

        IF lt_lines IS NOT INITIAL.
          ls_result-title = lv_header-tdtitle.
          IF ls_result-title IS INITIAL.
            ls_result-title = ls_params-check_class && '/' && ls_params-check_code.
          ENDIF.

          " Pre-fetch any ITF include documents
          DATA lt_inc_doc TYPE ty_inc_doc_tab.
          DATA lt_inc_objs TYPE STANDARD TABLE OF doku_obj WITH DEFAULT KEY.
          LOOP AT lt_lines INTO DATA(ls_line)
              WHERE tdformat = 'I'
                 OR tdline(8) = 'INCLUDE '.
            SPLIT ls_line-tdline AT ' ' INTO TABLE DATA(lt_parts).
            DATA(lv_inc_obj) = VALUE doku_obj( ).
            LOOP AT lt_parts INTO DATA(lv_part) WHERE table_line IS NOT INITIAL.
              IF lv_part <> 'INCLUDE' AND lv_part <> 'OBJECT' AND lv_part <> 'DOKU'
                  AND lv_part <> 'ID' AND lv_inc_obj IS INITIAL.
                lv_inc_obj = lv_part.
              ENDIF.
            ENDLOOP.
            IF lv_inc_obj IS NOT INITIAL.
              APPEND lv_inc_obj TO lt_inc_objs.
            ENDIF.
          ENDLOOP.
          SORT lt_inc_objs. DELETE ADJACENT DUPLICATES FROM lt_inc_objs.

          " Batch-select languages for all include objects (try both same id and 'E' fallback)
          DATA lt_inc_langus TYPE SORTED TABLE OF dokil WITH NON-UNIQUE KEY object.
          IF lt_inc_objs IS NOT INITIAL.
            SELECT id, object, langu FROM dokil "#EC CI_GENBUFF
              INTO TABLE @lt_inc_langus
              FOR ALL ENTRIES IN @lt_inc_objs
              WHERE object = @lt_inc_objs-table_line AND typ = 'E'.
          ENDIF.

          " Fetch each include document
          LOOP AT lt_inc_objs INTO DATA(lv_fetch_obj).
            DATA: lv_inc_head  TYPE thead.
            DATA lt_inc_lines TYPE tlinetab.
            " Prefer same id as parent; then logon language, English, any
            DATA lv_inc_langu TYPE sylangu.
            DATA lv_inc_id    TYPE doku_id.
            LOOP AT lt_inc_langus INTO DATA(ls_il_row)
              WHERE object = lv_fetch_obj AND id = lv_docu_id.
              IF ls_il_row-langu = sy-langu.
                lv_inc_langu = ls_il_row-langu.
                lv_inc_id    = ls_il_row-id.
                EXIT.
              ELSEIF ls_il_row-langu = 'E' AND lv_inc_langu IS INITIAL.
                lv_inc_langu = ls_il_row-langu.
                lv_inc_id    = ls_il_row-id.
              ELSEIF lv_inc_langu IS INITIAL.
                lv_inc_langu = ls_il_row-langu.
                lv_inc_id    = ls_il_row-id.
              ENDIF.
            ENDLOOP.
            IF lv_inc_langu IS INITIAL.
              " Try any id
              LOOP AT lt_inc_langus INTO ls_il_row WHERE object = lv_fetch_obj.
                lv_inc_langu = ls_il_row-langu.
                lv_inc_id    = ls_il_row-id.
                EXIT.
              ENDLOOP.
            ENDIF.
            IF lv_inc_id IS INITIAL.
              lv_inc_id = lv_docu_id.
            ENDIF.
            IF lv_inc_langu IS NOT INITIAL.
              CALL FUNCTION 'DOCU_GET_FOR_F1HELP'
                EXPORTING
                  id     = lv_inc_id
                  langu  = lv_inc_langu
                  object = lv_fetch_obj
                IMPORTING
                  head   = lv_inc_head
                TABLES
                  line   = lt_inc_lines
                EXCEPTIONS
                  OTHERS = 0.
            ENDIF.
            APPEND VALUE ty_inc_doc( obj = lv_fetch_obj lines = lt_inc_lines ) TO lt_inc_doc.
          ENDLOOP.

          ls_result-text = itf_lines_to_text( it_lines = lt_lines it_inc_doc = lt_inc_doc ).
        ELSE.
          ls_result-title = 'No documentation'.
          ls_result-text  = |No documentation available for { ls_params-check_class }/{ ls_params-check_code }|.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        ls_result-title = 'Error'.
        ls_result-text  = lx_error->get_text( ).
    ENDTRY.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.

  METHOD find_docu_key.
    TYPES: BEGIN OF ty_candidate,
             id     TYPE doku_id,
             object TYPE doku_obj,
             prio   TYPE i,
           END OF ty_candidate.

    DATA: lv_class_part TYPE sci_chk,
          lv_attr_part  TYPE c LENGTH 30,
          lv_attr2      TYPE c LENGTH 30,
          lv_obj        TYPE doku_obj.

    lv_class_part = iv_class.
    DATA(lv_code) = CONV sci_errc( iv_code ).

    " Build primary attr: code starting with letter → classic; else → MCODE_ prefix
    IF lv_code(1) CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
      lv_attr_part = lv_code.
      DATA(lv_sanitized) = condense( CONV string( lv_code ) ).
      REPLACE ALL OCCURRENCES OF PCRE '[^[:alnum:]]' IN lv_sanitized WITH '_'.
      lv_attr2 = |MCODE_{ lv_sanitized }|.
    ELSE.
      DATA(lv_num) = condense( CONV string( lv_code ) ).
      REPLACE ALL OCCURRENCES OF PCRE '[^[:alnum:]]' IN lv_num WITH '_'.
      lv_attr_part = |MCODE_{ lv_num }|.
      lv_attr2 = lv_code.
    ENDIF.

    " Build all candidate (id, object) keys at once
    DATA lt_cands TYPE STANDARD TABLE OF ty_candidate WITH DEFAULT KEY.
    lv_obj = lv_class_part. lv_obj+30 = lv_attr_part.
    APPEND VALUE #( id = 'CA' object = lv_obj prio = 1 ) TO lt_cands.
    lv_obj = lv_class_part. lv_obj+30 = lv_attr2.
    APPEND VALUE #( id = 'CA' object = lv_obj prio = 2 ) TO lt_cands.
    " 0000 is the class-level overview doc, only looked up when explicitly requested
    IF condense( CONV string( lv_code ) ) = '0000'.
      lv_obj = lv_class_part. lv_obj+30 = '0000'.
      APPEND VALUE #( id = 'CA' object = lv_obj prio = 3 ) TO lt_cands.
    ENDIF.
    " HY fallback: CL_CI_TEST_EXTENDED_CHECK stores numeric codes under id='HY'
    lv_obj = |CHAPBC_SLIN{ lv_code }|.
    APPEND VALUE #( id = 'HY' object = lv_obj prio = 4 ) TO lt_cands.

    " One SELECT to fetch all matches, prefer logon language then 'E' then any
    DATA lt_hits TYPE SORTED TABLE OF dokil WITH NON-UNIQUE KEY object id.
    SELECT id, object, langu FROM dokil "#EC CI_GENBUFF
      INTO TABLE @lt_hits
      FOR ALL ENTRIES IN @lt_cands
      WHERE id = @lt_cands-id AND object = @lt_cands-object AND typ = 'E'.

    LOOP AT lt_cands INTO DATA(ls_cand).
      " Prefer sy-langu, then 'E', then any
      DATA lv_langu TYPE sylangu.
      LOOP AT lt_hits INTO DATA(ls_hit)
          WHERE id = ls_cand-id AND object = ls_cand-object.
        IF ls_hit-langu = sy-langu.
          lv_langu = ls_hit-langu. EXIT.
        ELSEIF ls_hit-langu = 'E' AND lv_langu IS INITIAL.
          lv_langu = ls_hit-langu.
        ELSEIF lv_langu IS INITIAL.
          lv_langu = ls_hit-langu.
        ENDIF.
      ENDLOOP.
      IF lv_langu IS NOT INITIAL.
        ev_id = ls_cand-id. ev_object = ls_cand-object. ev_langu = lv_langu.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_line.
    CASE iv_fmt.
      WHEN 'U1' OR 'U2' OR 'U3'.
        IF cv_text IS NOT INITIAL.
          cv_text = cv_text && cl_abap_char_utilities=>newline.
        ENDIF.
        cv_text = cv_text && iv_line && cl_abap_char_utilities=>newline.
      WHEN '/'.
        cv_text = cv_text && cl_abap_char_utilities=>newline.
      WHEN OTHERS.
        IF iv_line IS NOT INITIAL.
          cv_text = cv_text && iv_line && cl_abap_char_utilities=>newline.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD itf_lines_to_text.
    LOOP AT it_lines INTO DATA(ls_ln).
      IF ls_ln-tdformat = 'I'
          OR ls_ln-tdline(8) = 'INCLUDE '.
        " Follow ITF include: find the pre-fetched doc and render its lines
        SPLIT ls_ln-tdline AT ' ' INTO TABLE DATA(lt_p).
        DATA(lv_iobj) = VALUE doku_obj( ).
        LOOP AT lt_p INTO DATA(lv_p) WHERE table_line IS NOT INITIAL.
          IF lv_p <> 'INCLUDE' AND lv_p <> 'OBJECT' AND lv_p <> 'DOKU'
              AND lv_p <> 'ID' AND lv_iobj IS INITIAL.
            lv_iobj = lv_p.
          ENDIF.
        ENDLOOP.
        READ TABLE it_inc_doc INTO DATA(ls_inc) WITH KEY ('OBJ') = lv_iobj.
        IF sy-subrc = 0.
          LOOP AT ls_inc-lines INTO DATA(ls_il).
            append_line( EXPORTING iv_fmt = ls_il-tdformat iv_line = ls_il-tdline
                         CHANGING  cv_text = rv_text ).
          ENDLOOP.
        ENDIF.
      ELSE.
        append_line( EXPORTING iv_fmt = ls_ln-tdformat iv_line = ls_ln-tdline
                     CHANGING  cv_text = rv_text ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
