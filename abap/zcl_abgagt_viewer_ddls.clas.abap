*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_ddls DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_ddls IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    RAISING cx_dd_ddl_read.
    DATA: lv_ddls_name TYPE ddlname,
          lo_handler TYPE REF TO if_dd_ddl_handler,
          ls_ddlsrcv TYPE ddddlsrcv,
          lv_devclass TYPE tadir-devclass,
          lv_found TYPE abap_bool.

    rs_info-name = iv_name.
    rs_info-type = 'DDLS'.
    rs_info-type_text = 'CDS View'.

    " Get package from TADIR
    SELECT SINGLE devclass FROM tadir
      INTO lv_devclass
      WHERE obj_name = iv_name
        AND object = 'DDLS'.
    IF sy-subrc = 0.
      rs_info-description = |CDS View { iv_name } in { lv_devclass }|.
    ELSE.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.

    " Use DDL handler to read CDS view source
    lo_handler = cl_dd_ddl_handler_factory=>create( ).
    lv_ddls_name = iv_name.

    " First try to read inactive version (get_state = 'M')
    TRY.
        lo_handler->read(
          EXPORTING
            name       = lv_ddls_name
            get_state  = 'M'
          IMPORTING
            ddddlsrcv_wa = ls_ddlsrcv ).

        IF ls_ddlsrcv-source IS NOT INITIAL.
          lv_found = abap_true.
        ENDIF.

      CATCH cx_dd_ddl_check.
        " Ignore - will try active version
    ENDTRY.

    " If no inactive version, try active version
    IF lv_found = abap_false.
      TRY.
          lo_handler->read(
            EXPORTING
              name       = lv_ddls_name
              get_state  = 'A'
            IMPORTING
              ddddlsrcv_wa = ls_ddlsrcv ).

          IF ls_ddlsrcv-source IS NOT INITIAL.
            lv_found = abap_true.
          ENDIF.
        CATCH cx_dd_ddl_check.
          " Not found
      ENDTRY.
    ENDIF.

    " Set source code if found
    IF lv_found = abap_true.
      rs_info-source = ls_ddlsrcv-source.
    ELSE.
      rs_info-not_found = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
