*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_clas DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

  PRIVATE SECTION.
    "! Inject active ENHO hook bodies into the matching CM sections
    "! @parameter iv_clsname | Target class name
    "! @parameter ct_sections | Sections table to inject into
    METHODS inject_enho_hooks
      IMPORTING iv_clsname  TYPE seoclsname
      CHANGING  ct_sections TYPE zcl_abgagt_command_view=>ty_sections.

ENDCLASS.

CLASS zcl_abgagt_viewer_clas IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: ls_include_info TYPE zif_abgagt_util=>ty_include_info,
          ls_obj_info TYPE zif_abgagt_util=>ty_object_info,
          lv_prog TYPE program,
          lt_source TYPE TABLE OF string,
          lv_clsname TYPE seoclsname,
          lv_obj_name TYPE tadir-obj_name.

    rs_info-name = iv_name.
    rs_info-type = 'CLAS'.
    rs_info-type_text = 'Class'.

    " Use util to detect source include
    DATA(lo_util) = zcl_abgagt_util=>get_instance( ).
    ls_include_info = lo_util->detect_include_info( iv_name ).

    IF ls_include_info-is_source_include = abap_true.
      " Source include detected - get devclass using unpadded obj_name
      lv_obj_name = ls_include_info-obj_name.
      ls_obj_info = lo_util->get_object_info_from_tadir( lv_obj_name ).
      rs_info-devclass = ls_obj_info-devclass.
      IF rs_info-devclass IS NOT INITIAL.
        rs_info-description = |Class { ls_include_info-obj_name } in { rs_info-devclass } (Source Include)|.
      ELSE.
        rs_info-description = |Class { ls_include_info-obj_name } (Source Include)|.
      ENDIF.
    ELSE.
      " Standard class - get info from TADIR
      lv_obj_name = iv_name.
      ls_obj_info = lo_util->get_object_info_from_tadir( lv_obj_name ).
      rs_info-devclass = ls_obj_info-devclass.
      IF rs_info-devclass IS NOT INITIAL.
        rs_info-description = |Class { iv_name } in { rs_info-devclass }|.
      ELSE.
        rs_info-not_found = abap_true.
      ENDIF.
    ENDIF.

    " Get class name (strip include suffix if needed)
    lv_clsname = ls_include_info-obj_name.
    IF lv_clsname IS INITIAL.
      lv_clsname = iv_name.
    ENDIF.

    IF iv_full = abap_false.
      " Default: return public section source only
      cl_oo_classname_service=>get_pubsec_name( EXPORTING clsname = lv_clsname RECEIVING result = lv_prog ).

      READ REPORT lv_prog INTO lt_source.
      LOOP AT lt_source INTO DATA(lv_line).
        IF rs_info-source IS INITIAL.
          rs_info-source = lv_line.
        ELSE.
          rs_info-source = rs_info-source && |\n| && lv_line.
        ENDIF.
      ENDLOOP.
    ELSE.
      " Full mode: assemble all sections into rs_info-sections.
      " global_start (assembled-source line numbers) is computed client-side
      " by the Node.js CLI from the local .clas.abap file or ADT source fetch.
      DATA: ls_section     TYPE zcl_abgagt_command_view=>ty_section,
            lv_pubsec      TYPE program,
            lv_include_pad TYPE program.

      " Build padded class name prefix (30 chars) for direct include reads
      DATA lv_pad30 TYPE program.
      lv_pad30 = lv_clsname.
      WHILE strlen( lv_pad30 ) < 30.
        lv_pad30 = lv_pad30 && '='.
      ENDWHILE.

      cl_oo_classname_service=>get_pubsec_name( EXPORTING clsname = lv_clsname RECEIVING result = lv_pubsec ).

      " Public section (CU)
      CLEAR ls_section.
      ls_section-suffix      = 'CU'.
      ls_section-description = 'Public Section'.
      CLEAR lt_source.
      READ REPORT lv_pubsec INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      " Protected section (CO)
      CLEAR ls_section.
      ls_section-suffix      = 'CO'.
      ls_section-description = 'Protected Section'.
      lv_prog = lv_pad30 && 'CO'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      " Private section (CP)
      CLEAR ls_section.
      ls_section-suffix      = 'CP'.
      ls_section-description = 'Private Section'.
      lv_prog = lv_pad30 && 'CP'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      " Method implementations (CM*) in methodindx order (= assembled-source order)
      SELECT methodname, methodindx FROM tmdir
        INTO TABLE @DATA(lt_methods)
        WHERE classname = @lv_clsname
        ORDER BY methodindx.

      LOOP AT lt_methods INTO DATA(ls_method).
        DATA(lv_cm_suffix)   = lo_util->convert_index_to_cm_suffix( CONV i( ls_method-methodindx ) ).
        lv_include_pad = lv_pad30 && lv_cm_suffix.

        CLEAR ls_section.
        ls_section-suffix      = lv_cm_suffix.
        ls_section-description = 'Class Method'.
        ls_section-method_name = ls_method-methodname.
        CLEAR lt_source.
        READ REPORT lv_include_pad INTO lt_source.
        IF sy-subrc = 0.
          ls_section-lines = lt_source.
        ENDIF.
        APPEND ls_section TO rs_info-sections.
      ENDLOOP.

      inject_enho_hooks( EXPORTING iv_clsname  = lv_clsname
                         CHANGING  ct_sections = rs_info-sections ).

      " Auxiliary sections: CCDEF, CCIMP, CCAU
      " These live in separate git files (not part of the assembled .clas.abap source).
      CLEAR ls_section.
      ls_section-suffix      = 'CCDEF'.
      ls_section-description = 'Local Definitions'.
      ls_section-file        = 'locals_def'.
      lv_prog = lv_pad30 && 'CCDEF'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      CLEAR ls_section.
      ls_section-suffix      = 'CCIMP'.
      ls_section-description = 'Local Implementations'.
      ls_section-file        = 'locals_imp'.
      lv_prog = lv_pad30 && 'CCIMP'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.

      CLEAR ls_section.
      ls_section-suffix      = 'CCAU'.
      ls_section-description = 'Unit Test'.
      ls_section-file        = 'testclasses'.
      lv_prog = lv_pad30 && 'CCAU'.
      CLEAR lt_source.
      READ REPORT lv_prog INTO lt_source.
      IF sy-subrc = 0.
        ls_section-lines = lt_source.
      ENDIF.
      APPEND ls_section TO rs_info-sections.
    ENDIF.
  ENDMETHOD.

  METHOD inject_enho_hooks.
    " Inject active ENHO hook bodies into CM sections.
    " This makes view --full --lines show runtime-accurate line numbers:
    " the hook lines are injected at the same position as the runtime CP program,
    " so the G numbers in the output match ADT breakpoint coordinates.
    TRY.
        SELECT obj_name FROM tadir
          INTO TABLE @DATA(lt_enho_names)
          WHERE pgmid  = 'R3TR'
            AND object = 'ENHO'.

        LOOP AT lt_enho_names INTO DATA(ls_enho_entry).
          TRY.
              DATA(li_enh_tool) = cl_enh_factory=>get_enhancement(
                enhancement_id = CONV enhname( ls_enho_entry-obj_name ) ).
              DATA(lo_hook_impl) = CAST cl_enh_tool_hook_impl( li_enh_tool ).

              " Check if this ENHO targets our class
              DATA: lv_org_name TYPE trobj_name,
                    lv_org_type TYPE trobjtype.
              lo_hook_impl->get_original_object(
                IMPORTING obj_name = lv_org_name
                          obj_type = lv_org_type ).

              IF lv_org_name <> iv_clsname.
                CONTINUE.
              ENDIF.

              " This ENHO targets our class — inject each hook into its CM section
              DATA(lt_hooks) = lo_hook_impl->get_hook_impls( ).
              LOOP AT lt_hooks INTO DATA(ls_hook).
                " Parse method name and spot from full_name: \TY:CLASS\ME:METHOD\SE:SPOT\EI
                DATA(lv_me_pos) = find( val = ls_hook-full_name sub = '\ME:' ).
                DATA(lv_se_pos) = find( val = ls_hook-full_name sub = '\SE:' ).
                DATA(lv_ei_pos) = find( val = ls_hook-full_name sub = '\EI' ).
                IF lv_me_pos < 0 OR lv_se_pos <= lv_me_pos.
                  CONTINUE.
                ENDIF.
                DATA(lv_hook_method) = substring(
                  val = ls_hook-full_name
                  off = lv_me_pos + 4
                  len = lv_se_pos - lv_me_pos - 4 ).
                TRANSLATE lv_hook_method TO UPPER CASE.
                DATA(lv_hook_spot) = substring(
                  val = ls_hook-full_name
                  off = lv_se_pos + 4
                  len = lv_ei_pos - lv_se_pos - 4 ).
                TRANSLATE lv_hook_spot TO UPPER CASE.

                " Find the matching CM section
                LOOP AT ct_sections ASSIGNING FIELD-SYMBOL(<ls_cm>).
                  DATA(lv_cm_method_upper) = <ls_cm>-method_name.
                  TRANSLATE lv_cm_method_upper TO UPPER CASE.
                  IF lv_cm_method_upper <> lv_hook_method.
                    CONTINUE.
                  ENDIF.

                  " Build marker + hook lines to inject
                  DATA(lt_inject) = VALUE string_table(
                    ( |*"* ENHO: { ls_enho_entry-obj_name } ({ lv_hook_spot })| ) ).
                  LOOP AT ls_hook-source INTO DATA(lv_src_line).
                    APPEND lv_src_line TO lt_inject.
                  ENDLOOP.
                  APPEND |*"* ENHO END| TO lt_inject.

                  IF lv_hook_spot = 'BEGIN'.
                    " Insert after METHOD statement (index 1)
                    DATA(lv_insert_idx) = 1.
                  ELSE.
                    " END spot: insert before ENDMETHOD (last line)
                    lv_insert_idx = lines( <ls_cm>-lines ).
                    IF lv_insert_idx > 0.
                      lv_insert_idx = lv_insert_idx - 1.
                    ENDIF.
                  ENDIF.

                  " Insert lt_inject into <ls_cm>-lines after lv_insert_idx
                  DATA(lv_inject_count) = lines( lt_inject ).
                  DO lv_inject_count TIMES.
                    DATA(lv_inject_line_idx) = lv_inject_count - sy-index + 1.
                    READ TABLE lt_inject INDEX lv_inject_line_idx INTO DATA(lv_inject_line).
                    INSERT lv_inject_line INTO <ls_cm>-lines INDEX lv_insert_idx + 1.
                  ENDDO.
                ENDLOOP.
              ENDLOOP.
            CATCH cx_root.
              " ENHO not loadable or wrong tool type — skip silently
              CONTINUE.
          ENDTRY.
        ENDLOOP.
      CATCH cx_root.
        " TADIR SELECT or outer failure — skip ENHO injection entirely
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
