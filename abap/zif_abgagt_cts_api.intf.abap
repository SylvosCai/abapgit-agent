"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">CTS API Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_cts_api PUBLIC.

  TYPES: BEGIN OF ty_create_result,
           subrc  TYPE i,
           trkorr TYPE trkorr,
         END OF ty_create_result.

  METHODS create_transport
    IMPORTING iv_description   TYPE string
              iv_category      TYPE char01
    RETURNING VALUE(rs_result) TYPE ty_create_result.

  METHODS check_transport
    IMPORTING iv_trkorr        TYPE trkorr
    RETURNING VALUE(rv_subrc)  TYPE i.

  METHODS release_transport
    IMPORTING iv_trkorr        TYPE trkorr
    RETURNING VALUE(rv_subrc)  TYPE i.

ENDINTERFACE.
