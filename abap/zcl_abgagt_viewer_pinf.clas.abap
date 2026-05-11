*"*"use source
*"*"Local Interface:
**********************************************************************
CLASS zcl_abgagt_viewer_pinf DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.

ENDCLASS.

CLASS zcl_abgagt_viewer_pinf IMPLEMENTATION.

  METHOD zif_abgagt_viewer~get_info.
    DATA: lv_name      TYPE scomifnam,
          lv_devclass  TYPE tadir-devclass,
          li_interface TYPE REF TO if_package_interface,
          ls_attrs     TYPE scompidtln,
          lt_elements  TYPE tpak_package_interf_elem_list,
          ls_elem_data TYPE scomeldtln.

    lv_name = iv_name.

    rs_info-name      = iv_name.
    rs_info-type      = 'PINF'.
    rs_info-type_text = 'Package Interface'.

    SELECT SINGLE devclass FROM tadir
      INTO lv_devclass
      WHERE pgmid    = 'R3TR'
        AND obj_name = lv_name
        AND object   = 'PINF'.
    IF sy-subrc <> 0.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.
    rs_info-devclass = lv_devclass.

    cl_package_interface=>load_package_interface(
      EXPORTING
        i_package_interface_name = lv_name
      IMPORTING
        e_package_interface      = li_interface
      EXCEPTIONS
        object_not_existing      = 1
        OTHERS                   = 2 ).

    IF sy-subrc <> 0.
      rs_info-description = |Package Interface { iv_name }|.
      RETURN.
    ENDIF.

    li_interface->get_all_attributes(
      IMPORTING e_package_interface_data = ls_attrs
      EXCEPTIONS object_invalid          = 1
                 OTHERS                  = 2 ).

    rs_info-description = COND #(
      WHEN ls_attrs-descript IS NOT INITIAL THEN ls_attrs-descript
      ELSE |Package Interface { iv_name }|
    ).

    li_interface->get_elements(
      IMPORTING e_elements     = lt_elements
      EXCEPTIONS object_invalid = 1
                 intern_err     = 2
                 OTHERS         = 3 ).

    LOOP AT lt_elements INTO DATA(li_elem).
      li_elem->get_all_attributes(
        IMPORTING e_element_data = ls_elem_data
        EXCEPTIONS object_invalid = 1
                   OTHERS         = 2 ).
      APPEND VALUE #(
        field       = |{ ls_elem_data-elem_type }_{ sy-tabix }|
        key         = abap_false
        type        = 'CHAR'
        length      = 30
        description = |{ ls_elem_data-elem_type }: { ls_elem_data-elem_key }|
      ) TO rs_info-components.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
