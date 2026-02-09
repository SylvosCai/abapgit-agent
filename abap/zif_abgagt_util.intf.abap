"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">Utility Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_util PUBLIC.

  "! Parse file name to extract object type and name
  "! @parameter iv_file | File name (e.g., 'zcl_my_class.clas.abap')
  "! @parameter ev_obj_type | Object type (e.g., 'CLAS')
  "! @parameter ev_obj_name | Object name (e.g., 'ZCL_MY_CLASS')
  METHODS parse_file_to_object
    IMPORTING
      iv_file TYPE string
    EXPORTING
      ev_obj_type TYPE string
                ev_obj_name TYPE string.

  "! Check activation log for errors
  "! @return rv_has_error | ABAP_TRUE if errors found
  METHODS check_log_for_errors
    RETURNING
      VALUE(rv_has_error) TYPE abap_bool.

  "! Get detailed log information
  "! @return rv_detail | Detailed log message
  METHODS get_log_detail
    RETURNING
      VALUE(rv_detail) TYPE string.

ENDINTERFACE.
