"! <p class="shorttext synchronized">DDL Handler Interface for ABAP Git Agent</p>
"! Interface for dependency injection to enable testing with mocks
INTERFACE zif_abgagt_ddl_handler PUBLIC.

  TYPES: BEGIN OF ty_ddlsrcv,
           ddlname TYPE ddlname,
           source  TYPE string,
         END OF ty_ddlsrcv.

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

  "! Read DDLS source (inactive or active)
  "! @parameter iv_name | DDL source name
  "! @parameter iv_get_state | State to read: 'M' = inactive, 'A' = active
  "! @parameter es_ddlsrcv | Retrieved DDL source record
  "! @raising cx_dd_ddl_check | On read failure
  METHODS read
    IMPORTING iv_name      TYPE ddlname
              iv_get_state TYPE char1 DEFAULT 'M'
    EXPORTING es_ddlsrcv   TYPE ty_ddlsrcv
    RAISING   cx_dd_ddl_check.

  "! Validate DDLS source and collect warnings
  "! @parameter iv_name | DDL source name
  "! @parameter cs_ddlsrcv | DDL source record (updated during validation)
  "! @raising cx_dd_ddl_check | On validation failure with errors
  METHODS check
    IMPORTING iv_name         TYPE ddlname
    CHANGING  cs_ddlsrcv      TYPE ty_ddlsrcv
    RAISING   cx_dd_ddl_check.

  "! Get warnings collected during the last check call
  "! @parameter rt_warnings | List of warnings from last check
  METHODS get_warnings
    RETURNING VALUE(rt_warnings) TYPE ty_warnings.

ENDINTERFACE.
