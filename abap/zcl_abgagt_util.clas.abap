"! <p class="shorttext synchronized">Utility Class for ABAP Git Agent</p>
"! Provides common utilities for parsing files, checking logs, and configuring repos.
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
    DATA mo_repo TYPE REF TO zif_abapgit_repo.
    DATA mv_configured TYPE abap_bool.

    METHODS constructor.

ENDCLASS.

CLASS zcl_abgagt_util IMPLEMENTATION.

  METHOD get_instance.
    DATA lo_util TYPE REF TO zcl_abgagt_util.
    CREATE OBJECT lo_util.
    ro_util = lo_util.
  ENDMETHOD.

  METHOD constructor.
    mv_configured = abap_false.
  ENDMETHOD.

  METHOD parse_file_to_object.
    " Extract object type and name from file name
    " e.g., 'zcl_my_class.clas.abap' -> CLAS, ZCL_MY_CLASS
    " e.g., 'zif_my_intf.intf.abap' -> INTF, ZIF_MY_INTTF

    DATA lv_filename TYPE string.
    lv_filename = iv_file.

    " Find extension position
    DATA lv_ext_pos TYPE i.
    lv_ext_pos = find( val = lv_filename sub = '.' occ = 2 case = abap_false ).

    IF lv_ext_pos > 0.
      " Get object name from first part
      DATA lv_obj_name TYPE string.
      lv_obj_name = substring( val = lv_filename len = lv_ext_pos ).

      " Handle subdirectory paths
      DATA lv_last_slash TYPE i.
      lv_last_slash = find( val = lv_obj_name sub = '/' ).

      IF lv_last_slash > 0.
        lv_obj_name = substring( val = lv_obj_name off = lv_last_slash + 1 ).
      ENDIF.

      " Convert to uppercase for ABAP
      TRANSLATE lv_obj_name TO UPPER CASE.
      ev_obj_name = lv_obj_name.

      " Get extension for object type
      DATA lv_ext TYPE string.
      lv_ext = substring( val = lv_filename off = lv_ext_pos + 1 ).

      CASE lv_ext.
        WHEN 'clas.abap' OR 'class.abap'.
          ev_obj_type = 'CLAS'.
        WHEN 'intf.abap'.
          ev_obj_type = 'INTF'.
        WHEN 'prog.abap' OR 'program.abap'.
          ev_obj_type = 'PROG'.
        WHEN 'fugr.abap'.
          ev_obj_type = 'FUGR'.
        WHEN 'tabl.abap'.
          ev_obj_type = 'TABL'.
        WHEN 'ddls.abap'.
          ev_obj_type = 'DDLS'.
        WHEN OTHERS.
          " Try to parse from filename pattern
          CLEAR ev_obj_type.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD check_log_for_errors.
    DATA lo_log TYPE REF TO zcl_abapgit_log.
    lo_log = zcl_abapgit_log=>create_new( ).

    rv_has_error = abap_false.

    " Check if log has errors
    DATA lt_logs TYPE zcl_abapgit_persistence_factory=>ty_logs.
    " Implementation would check actual log entries
  ENDMETHOD.

  METHOD get_log_detail.
    " Get detailed log information
    " Implementation would return formatted log details
    rv_detail = 'Log detail not implemented'.
  ENDMETHOD.

  METHOD configure_credentials.
    " Configure credentials for git access
    " Implementation would set up authentication
    mv_configured = abap_true.
  ENDMETHOD.

  METHOD build_repo.
    " Build abapGit repo instance from URL
    " Implementation would create and return repo reference
    " For now, return dummy reference
    ro_repo = mo_repo.
  ENDMETHOD.

ENDCLASS.
