CLASS zcl_abgagt_util DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_util.
    METHODS parse_file_to_object
      IMPORTING iv_file TYPE string
      EXPORTING ev_obj_type TYPE string ev_obj_name TYPE string.
    METHODS check_log_for_errors RETURNING VALUE(rv_has_error) TYPE abap_bool.
    METHODS get_log_detail RETURNING VALUE(rv_detail) TYPE string.
ENDCLASS.

CLASS zcl_abgagt_util IMPLEMENTATION.
  METHOD parse_file_to_object.
  ENDMETHOD.
  METHOD check_log_for_errors.
    rv_has_error = abap_false.
  ENDMETHOD.
  METHOD get_log_detail.
    rv_detail = 'Not implemented'.
  ENDMETHOD.
ENDCLASS.
