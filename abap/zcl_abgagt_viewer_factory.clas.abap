*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_viewer_factory DEFINITION PUBLIC CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_abgagt_viewer_factory.

    METHODS get_viewer
      IMPORTING iv_obj_type   TYPE string
      RETURNING VALUE(ro_viewer) TYPE REF TO zif_abgagt_viewer
      RAISING   cx_sy_create_object_error.

  PRIVATE SECTION.
    CONSTANTS mc_class_prefix TYPE string VALUE 'ZCL_ABGAGT_VIEWER_'.

    METHODS build_class_name
      IMPORTING iv_obj_type   TYPE string
      RETURNING VALUE(rv_class_name) TYPE seoclsname.

ENDCLASS.

CLASS zcl_abgagt_viewer_factory IMPLEMENTATION.

  METHOD get_instance.
    CREATE OBJECT ro_instance.
  ENDMETHOD.

  METHOD build_class_name.
    " CLAS -> ZCL_ABGAGT_VIEWER_CLAS
    rv_class_name = mc_class_prefix && iv_obj_type.
  ENDMETHOD.

  METHOD get_viewer.
    DATA lv_class_name TYPE seoclsname.
    lv_class_name = build_class_name( iv_obj_type ).
    CREATE OBJECT ro_viewer TYPE (lv_class_name).
  ENDMETHOD.

ENDCLASS.
