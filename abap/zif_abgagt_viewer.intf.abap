*"* Use this source text module for the interface definition
INTERFACE zif_abgagt_viewer PUBLIC.

  " Get detailed information about an ABAP object
  " @parameter iv_name | Object name (e.g., ZCL_MY_CLASS)
  " @parameter rs_info | Object information structure
  METHODS get_info
    IMPORTING iv_name        TYPE string
    RETURNING VALUE(rs_info) TYPE zcl_abgagt_command_view=>ty_view_object.

ENDINTERFACE.
