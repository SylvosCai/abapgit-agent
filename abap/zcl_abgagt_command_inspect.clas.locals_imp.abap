CLASS lcl_ddl_handler_default IMPLEMENTATION.

  METHOD zif_abgagt_ddl_handler~read.
    " Use real SAP DDL handler factory
    DATA: lo_handler TYPE REF TO if_dd_ddl_handler,
          ls_ddlsrcv_wa TYPE ddddlsrcv.

    lo_handler = cl_dd_ddl_handler_factory=>create( ).
    TRY.
        lo_handler->read(
          EXPORTING
            name       = iv_name
            get_state  = iv_get_state
          IMPORTING
            ddddlsrcv_wa = ls_ddlsrcv_wa ).
      CATCH cx_dd_ddl_check cx_dd_ddl_read.
        " Return empty on error
    ENDTRY.

    es_ddlsrcv = VALUE #( ddlname = ls_ddlsrcv_wa-ddlname
                          source  = ls_ddlsrcv_wa-source ).
  ENDMETHOD.

  METHOD zif_abgagt_ddl_handler~check.
    " Use real SAP DDL handler
    DATA: lo_handler TYPE REF TO if_dd_ddl_handler,
          ls_ddlsrcv_wa TYPE ddddlsrcv,
          lt_warnings TYPE ddl2ddicwarnings.

    ls_ddlsrcv_wa = VALUE #( ddlname = cs_ddlsrcv-ddlname
                            source  = cs_ddlsrcv-source ).

    lo_handler = cl_dd_ddl_handler_factory=>create( ).
    TRY.
        lo_handler->check(
          EXPORTING
            name = iv_name
          IMPORTING
            warnings = lt_warnings
          CHANGING
            ddlsrcv_wa = ls_ddlsrcv_wa ).
      CATCH cx_dd_ddl_check cx_dd_ddl_read.
        " Continue with empty warnings on error
    ENDTRY.

    " Convert warnings to interface type
    mt_warnings = VALUE #( FOR wa IN lt_warnings
      ( type = wa-type line = wa-line column = wa-column severity = wa-severity
        arbgb = wa-arbgb msgnr = wa-msgnr var1 = wa-var1 var2 = wa-var2 var3 = wa-var3 var4 = wa-var4 ) ).

    cs_ddlsrcv = VALUE #( ddlname = ls_ddlsrcv_wa-ddlname
                         source  = ls_ddlsrcv_wa-source ).
  ENDMETHOD.

  METHOD zif_abgagt_ddl_handler~get_warnings.
    rt_warnings = mt_warnings.
  ENDMETHOD.

ENDCLASS.
