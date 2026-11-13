CLASS zcl_abgagt_viewer_ddlx DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.
ENDCLASS.

CLASS zcl_abgagt_viewer_ddlx IMPLEMENTATION.
  METHOD zif_abgagt_viewer~get_info.
    rs_info-name = iv_name.
    rs_info-type = 'DDLX'.
    rs_info-type_text = 'CDS Metadata Extension'.

    SELECT SINGLE devclass FROM tadir
      WHERE pgmid = 'R3TR' AND object = 'DDLX' AND obj_name = @iv_name
      INTO @rs_info-devclass.
    IF sy-subrc <> 0.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.
    rs_info-description = |CDS Metadata Extension { iv_name } in { rs_info-devclass }|.

    DATA lr_data TYPE REF TO data.
    DATA lo_persistence TYPE REF TO if_wb_object_persist.
    DATA lo_model TYPE REF TO if_wb_object_data_model.
    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_source> TYPE any.

    TRY.
        CREATE DATA lr_data TYPE ('CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA').
        ASSIGN lr_data->* TO <ls_data>.
        lo_persistence = NEW cl_ddlx_adt_object_persist( ).
        CREATE OBJECT lo_model TYPE ('CL_DDLX_WB_OBJECT_DATA').
        lo_persistence->get(
          EXPORTING p_object_key = CONV seu_objkey( iv_name ) p_version = 'A'
          CHANGING p_object_data = lo_model ).
        lo_model->get_data( IMPORTING p_data = <ls_data> ).
        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_data> TO <lv_source>.
        IF sy-subrc <> 0.
          rs_info-error = 'Incompatible native DDLX source model'.
          RETURN.
        ENDIF.
        rs_info-source = <lv_source>.
        IF rs_info-source IS INITIAL.
          rs_info-error = 'Active DDLX source is unavailable'.
          RETURN.
        ENDIF.
      CATCH cx_root INTO DATA(lx_error).
        rs_info-error = |Active DDLX source could not be read: { lx_error->get_text( ) }|.
        RETURN.
    ENDTRY.

    IF iv_full = abap_true.
      SPLIT rs_info-source AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_lines).
      APPEND VALUE #( suffix = 'asddlxs' description = 'CDS Metadata Extension Source'
        lines = lt_lines ) TO rs_info-sections.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
