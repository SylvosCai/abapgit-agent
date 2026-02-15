*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_intf DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_intf IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_obj_name TYPE tadir-obj_name,
          lv_devclass TYPE tadir-devclass.

    SELECT SINGLE obj_name devclass FROM tadir
      INTO (lv_obj_name, lv_devclass)
      WHERE obj_name = iv_name
        AND object = 'INTF'.
    IF sy-subrc = 0.
      rs_info-name = iv_name.
      rs_info-type = 'INTF'.
      rs_info-type_text = 'Interface'.
      rs_info-description = |Interface { iv_name } in { lv_devclass }|.
    ENDIF.

    " Get interface methods using RTTS
    DATA lo_type TYPE REF TO cl_abap_classdescr.
    DATA lo_method TYPE REF TO data.
    DATA lv_method TYPE string.

    lo_type ?= cl_abap_classdescr=>describe_by_name( iv_name ).

    " Loop through methods
    LOOP AT lo_type->methods REFERENCE INTO lo_method.
      ASSIGN lo_method->* TO FIELD-SYMBOL(<ls_method>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_method> TO FIELD-SYMBOL(<lv_name>).
        IF sy-subrc = 0.
          lv_method = |PUBLIC { <lv_name> }|.
          APPEND lv_method TO rs_info-methods.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
