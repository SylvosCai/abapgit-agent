*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_dcls DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_dcls IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_devclass TYPE tadir-devclass,
          lr_data     TYPE REF TO data,
          lo_handler  TYPE REF TO object.

    FIELD-SYMBOLS: <ls_data>   TYPE any,
                   <lv_source> TYPE any.

    rs_info-name      = iv_name.
    rs_info-type      = 'DCLS'.
    rs_info-type_text = 'Access Control'.

    " Get package from TADIR
    SELECT SINGLE devclass FROM tadir
      INTO lv_devclass
      WHERE obj_name = iv_name
        AND object   = 'DCLS'.
    IF sy-subrc <> 0.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.

    rs_info-devclass    = lv_devclass.
    rs_info-description = |Access Control { iv_name } in { lv_devclass }|.

    " Read DCL source via CL_ACM_DCL_HANDLER_FACTORY (dynamic call — only on systems with ACM)
    TRY.
        CALL METHOD ('CL_ACM_DCL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            ro_handler = lo_handler.

        CREATE DATA lr_data TYPE ('ACM_S_DCLSRC').
        ASSIGN lr_data->* TO <ls_data>.

        CALL METHOD lo_handler->('READ')
          EXPORTING
            iv_dclname = iv_name
          IMPORTING
            es_dclsrc  = <ls_data>.

        ASSIGN COMPONENT 'SOURCE' OF STRUCTURE <ls_data> TO <lv_source>.
        IF sy-subrc = 0.
          rs_info-source = <lv_source>.
        ENDIF.

        IF iv_full = abap_true AND rs_info-source IS NOT INITIAL.
          " Split source into lines for the section
          DATA(lt_lines) = VALUE string_table( ).
          SPLIT rs_info-source AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

          APPEND VALUE zcl_abgagt_command_view=>ty_section(
            suffix      = 'asdcls'
            description = 'Access Control Source'
            method_name = ''
            file        = |{ iv_name }.dcls.asdcls|
            lines       = lt_lines
          ) TO rs_info-sections.
        ENDIF.

      CATCH cx_static_check cx_dynamic_check.
        " ACM not available on this system — basic info already set above
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
