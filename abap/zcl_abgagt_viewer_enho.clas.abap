*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_enho DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_enho IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_devclass  TYPE tadir-devclass,
          lo_hook_impl TYPE REF TO cl_enh_tool_hook_impl.

    rs_info-name      = iv_name.
    rs_info-type      = 'ENHO'.
    rs_info-type_text = 'Enhancement'.

    " Check TADIR for object existence and package
    SELECT SINGLE devclass FROM tadir
      INTO @lv_devclass
      WHERE obj_name = @iv_name
        AND object   = 'ENHO'.
    IF sy-subrc <> 0.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.

    rs_info-devclass = lv_devclass.

    " Load the enhancement object
    TRY.
        DATA(li_tool) = cl_enh_factory=>get_enhancement(
          enhancement_id = CONV #( iv_name ) ).
        lo_hook_impl ?= li_tool.
      CATCH cx_root.
        rs_info-description = |Enhancement { iv_name } in { lv_devclass }|.
        RETURN.
    ENDTRY.

    " Get original object info
    DATA: lv_org_name TYPE trobj_name,
          lv_org_type TYPE trobjtype.
    lo_hook_impl->get_original_object(
      IMPORTING obj_name = lv_org_name
                obj_type = lv_org_type ).

    rs_info-description = |Enhancement { iv_name } in { lv_devclass } ({ lv_org_type } { lv_org_name })|.

    IF iv_full = abap_false.
      RETURN.
    ENDIF.

    " Full view: populate sections with hook source
    DATA(lt_hooks) = lo_hook_impl->get_hook_impls( ).

    LOOP AT lt_hooks INTO DATA(ls_hook).
      " Extract the hash from full_name for the suffix
      " full_name format: \TY:CLASS\ME:METHOD\SE:BEGIN\EI
      " Parse out method and section for a human-readable description
      DATA(lv_method) = ls_hook-full_name.
      DATA(lv_desc)   = ls_hook-full_name.

      " Build a readable description: METHOD@BEGIN or METHOD@END
      DATA(lv_me_pos)  = find( val = lv_method  sub = '\ME:' ).
      DATA(lv_se_pos)  = find( val = lv_method  sub = '\SE:' ).
      DATA(lv_ei_pos)  = find( val = lv_method  sub = '\EI' ).
      IF lv_me_pos >= 0 AND lv_se_pos > lv_me_pos.
        DATA(lv_mname) = substring( val = lv_method off = lv_me_pos + 4
                                    len = lv_se_pos - lv_me_pos - 4 ).
        IF lv_ei_pos > lv_se_pos.
          DATA(lv_section) = substring( val = lv_method off = lv_se_pos + 4
                                        len = lv_ei_pos - lv_se_pos - 4 ).
        ENDIF.
        lv_desc = |{ lv_mname }@{ lv_section }|.
      ENDIF.

      " Compute SHA1 hash for section suffix (first 8 chars of SHA1(full_name))
      DATA lv_hash_raw TYPE xstring.
      DATA(lv_suffix) = to_lower( substring( val = ls_hook-full_name off = 0 len = 8 ) ).
      TRY.
          cl_abap_message_digest=>calculate_hash_for_char(
            EXPORTING if_algorithm = 'SHA1'
                      if_data      = ls_hook-full_name
            IMPORTING ef_hashxstring = lv_hash_raw ).
          DATA(lv_hex) = CONV string( lv_hash_raw ).
          TRANSLATE lv_hex TO LOWER CASE.
          lv_suffix = lv_hex(8).
        CATCH cx_root.
          " fallback already set above
      ENDTRY.

      APPEND VALUE zcl_abgagt_command_view=>ty_section(
        suffix      = lv_suffix
        description = ls_hook-full_name
        method_name = lv_desc
        lines       = ls_hook-source
      ) TO rs_info-sections.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
