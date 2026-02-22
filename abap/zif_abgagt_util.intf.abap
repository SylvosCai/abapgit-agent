"* Use this source text module for the interface definition
"! <p class="shorttext synchronized">Utility Interface for ABAP Git Agent</p>
INTERFACE zif_abgagt_util PUBLIC.

  "! Structure for include detection result
  TYPES: BEGIN OF ty_include_info,
           is_source_include TYPE abap_bool,
           obj_type         TYPE tadir-object,
           obj_name         TYPE tadir-obj_name,
           include_type     TYPE string,
           type_text        TYPE string,
         END OF ty_include_info.

  "! Structure for object info from TADIR
  TYPES: BEGIN OF ty_object_info,
           obj_type  TYPE tadir-object,
           obj_name  TYPE tadir-obj_name,
           devclass  TYPE tadir-devclass,
           type_text TYPE string,
         END OF ty_object_info.

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

  "! Detect object type, name, and include type from include name
  "! @parameter iv_name | Full include name (e.g., 'ZCL_CLASS=============CM001')
  "! @return rs_info | Include information (type, name, include_type)
  METHODS detect_include_info
    IMPORTING
      iv_name        TYPE string
    RETURNING
      VALUE(rs_info) TYPE ty_include_info.

  "! Get object info from TADIR by object name
  "! @parameter iv_obj_name | Object name (e.g., 'ZCL_MY_CLASS')
  "! @return rs_info | Object info (obj_type, obj_name, devclass, type_text)
  METHODS get_object_info_from_tadir
    IMPORTING
      iv_obj_name       TYPE tadir-obj_name
    RETURNING
      VALUE(rs_info)   TYPE ty_object_info.

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

  "! Convert method index from base-36 string to integer
  "! @parameter iv_include_name | Include name (e.g., 'CM001', 'CM00F')
  "! @return rv_method_index | Method index as integer (e.g., 1, 15)
  METHODS convert_method_index
    IMPORTING
      iv_include_name TYPE string
    RETURNING
      VALUE(rv_method_index) TYPE i.

  "! Get method name from TMDIR by class name and method index
  "! @parameter iv_classname | Class name (e.g., 'ZCL_MY_CLASS')
  "! @parameter iv_method_index | Method index (e.g., 1 = CM001, 15 = CM00F)
  "! @return rv_method_name | Method name from TMDIR
  METHODS get_method_name
    IMPORTING
      iv_classname     TYPE string
      iv_method_index TYPE i
    RETURNING
      VALUE(rv_method_name) TYPE string.

  "! Get human-readable description of include type
  "! @parameter iv_include_name | Full include name (e.g., 'ZCL_CLASS=============CM001')
  "! @return rv_description | Human-readable description (e.g., 'Class Method')
  METHODS get_include_description
    IMPORTING
      iv_include_name TYPE string
    RETURNING
      VALUE(rv_description) TYPE string.

ENDINTERFACE.
