"! <p class="shorttext synchronized">CTS API Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_cts_api PUBLIC.

  TYPES: BEGIN OF ty_create_result,
           subrc  TYPE i,
           trkorr TYPE trkorr,
         END OF ty_create_result.

  "! Create a new transport request
  "! @parameter iv_description | Short description for the transport
  "! @parameter iv_category | Transport category (e.g., 'K' = workbench)
  "! @parameter rs_result | Return code and created transport number
  METHODS create_transport
    IMPORTING iv_description   TYPE string
              iv_category      TYPE char01
    RETURNING VALUE(rs_result) TYPE ty_create_result.

  "! Check whether a transport request exists and is open
  "! @parameter iv_trkorr | Transport request number
  "! @parameter rv_subrc | 0 = open, non-zero = not found or already released
  METHODS check_transport
    IMPORTING iv_trkorr        TYPE trkorr
    RETURNING VALUE(rv_subrc)  TYPE i.

  "! Release a transport request
  "! @parameter iv_trkorr | Transport request number to release
  "! @parameter rv_subrc | 0 = released successfully, non-zero = failure
  METHODS release_transport
    IMPORTING iv_trkorr        TYPE trkorr
    RETURNING VALUE(rv_subrc)  TYPE i.

ENDINTERFACE.
