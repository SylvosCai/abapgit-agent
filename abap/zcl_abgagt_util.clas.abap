"! <p class="shorttext synchronized">Utility Class for ABAP Git Agent</p>
"! Provides common utilities for parsing files and checking logs.
"! Uses singleton pattern - get instance via get_instance( ).
CLASS zcl_abgagt_util DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_util.

    "! Get singleton instance
    "! @return ro_util | Utility instance
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_util) TYPE REF TO zcl_abgagt_util.

  PRIVATE SECTION.
    METHODS constructor.

ENDCLASS.

CLASS zcl_abgagt_util IMPLEMENTATION.

  METHOD get_instance.
    DATA lo_util TYPE REF TO zcl_abgagt_util.
    CREATE OBJECT lo_util.
    ro_util = lo_util.
  ENDMETHOD.

  METHOD constructor.
    " Private constructor for singleton
  ENDMETHOD.

  METHOD parse_file_to_object.
    " Extract object type and name from file name
    " e.g., 'zcl_my_class.clas.abap' -> CLAS, ZCL_MY_CLASS

    DATA lv_filename TYPE string.
    lv_filename = iv_file.

    " Find extension position
    DATA lv_ext_pos TYPE i.
    lv_ext_pos = find( val = lv_filename sub = '.abap' case = abap_false ).

    IF lv_ext_pos > 0.
      DATA lv_obj_name TYPE string.
      lv_obj_name = substring( val = lv_filename len = lv_ext_pos ).

      " Handle subdirectory paths
      DATA lv_last_slash TYPE i.
      lv_last_slash = find( val = lv_obj_name sub = '/' ).

      IF lv_last_slash > 0.
        lv_obj_name = substring( val = lv_obj_name off = lv_last_slash + 1 ).
      ENDIF.

      TRANSLATE lv_obj_name TO UPPER CASE.
      ev_obj_name = lv_obj_name.

      DATA lv_ext TYPE string.
      lv_ext = substring( val = lv_filename off = lv_ext_pos + 5 ).

      CASE lv_ext.
        WHEN 'clas' OR 'class'.
          ev_obj_type = 'CLAS'.
        WHEN 'intf'.
          ev_obj_type = 'INTF'.
        WHEN 'prog' OR 'program'.
          ev_obj_type = 'PROG'.
        WHEN OTHERS.
          CLEAR ev_obj_type.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD check_log_for_errors.
    rv_has_error = abap_false.
  ENDMETHOD.

  METHOD get_log_detail.
    rv_detail = 'Not implemented'.
  ENDMETHOD.

ENDCLASS.
