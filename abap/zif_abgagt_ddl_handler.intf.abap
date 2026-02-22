"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">DDL Handler Interface for ABAP Git Agent</p>
"! Interface for dependency injection to enable testing with mocks
INTERFACE zif_abgagt_ddl_handler PUBLIC.

  " Structure for DDLS source
  TYPES: BEGIN OF ty_ddlsrcv,
           ddlname TYPE ddlname,
           source  TYPE string,
         END OF ty_ddlsrcv.

  " Warning type from check
  TYPES: BEGIN OF ty_warn,
           type      TYPE char1,
           line      TYPE i,
           column    TYPE i,
           severity  TYPE char1,
           arbgb     TYPE arbgb,
           msgnr     TYPE msgnr,
           var1      TYPE string,
           var2      TYPE string,
           var3      TYPE string,
           var4      TYPE string,
         END OF ty_warn.

  TYPES ty_warnings TYPE STANDARD TABLE OF ty_warn WITH NON-UNIQUE DEFAULT KEY.

  " Error type from exception
  TYPES: BEGIN OF ty_err,
           type      TYPE char1,
           line      TYPE i,
           column    TYPE i,
           severity  TYPE char1,
           arbgb     TYPE arbgb,
           msgnr     TYPE msgnr,
           var1      TYPE string,
           var2      TYPE string,
           var3      TYPE string,
           var4      TYPE string,
         END OF ty_err.

  TYPES ty_errors TYPE STANDARD TABLE OF ty_err WITH NON-UNIQUE DEFAULT KEY.

  " Read DDLS source (inactive or active)
  METHODS read
    IMPORTING iv_name      TYPE ddlname
              iv_get_state TYPE char1 DEFAULT 'M'
    EXPORTING es_ddlsrcv   TYPE ty_ddlsrcv
    RAISING   cx_dd_ddl_check.

  " Validate DDLS source
  METHODS check
    IMPORTING iv_name         TYPE ddlname
    CHANGING  cs_ddlsrcv      TYPE ty_ddlsrcv
    RAISING   cx_dd_ddl_check.

  " Get warnings from last check
  METHODS get_warnings
    RETURNING VALUE(rt_warnings) TYPE ty_warnings.

ENDINTERFACE.
