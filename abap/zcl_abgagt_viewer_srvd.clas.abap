CLASS zcl_abgagt_viewer_srvd DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_viewer.
ENDCLASS.

CLASS zcl_abgagt_viewer_srvd IMPLEMENTATION.
  METHOD zif_abgagt_viewer~get_info.
    rs_info-name = iv_name.
    rs_info-type = 'SRVD'.
    rs_info-type_text = 'Service Definition'.

    SELECT SINGLE devclass FROM tadir
      WHERE pgmid = 'R3TR' AND object = 'SRVD' AND obj_name = @iv_name
      INTO @rs_info-devclass.
    IF sy-subrc <> 0.
      rs_info-not_found = abap_true.
      RETURN.
    ENDIF.
    rs_info-description = |Service Definition { iv_name } in { rs_info-devclass }|.

    DATA lr_data TYPE REF TO data.
    DATA lo_operator TYPE REF TO object.
    DATA lo_model TYPE REF TO if_wb_object_data_model.
    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_source> TYPE any.

    TRY.
        CREATE DATA lr_data TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA').
        ASSIGN lr_data->* TO <ls_data>.
        DATA(ls_object_type) = VALUE wbobjtype( objtype_tr = 'SRVD' subtype_wb = 'SRV' ).
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING object_type = ls_object_type object_key = CONV seu_objkey( iv_name )
          RECEIVING result = lo_operator.
        CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING version = 'A' data_selection = 'AL'
          IMPORTING eo_object_data = lo_model.
        CALL METHOD lo_model->('GET_DATA') IMPORTING p_data = <ls_data>.
        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_data> TO <lv_source>.
        IF sy-subrc <> 0.
          rs_info-error = 'Incompatible native SRVD source model'.
          RETURN.
        ENDIF.
        rs_info-source = <lv_source>.
        IF rs_info-source IS INITIAL.
          rs_info-error = 'Active SRVD source is unavailable'.
          RETURN.
        ENDIF.
      CATCH cx_root INTO DATA(lx_error).
        rs_info-error = |Active SRVD source could not be read: { lx_error->get_text( ) }|.
        RETURN.
    ENDTRY.

    IF iv_full = abap_true.
      SPLIT rs_info-source AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_lines).
      APPEND VALUE #( suffix = 'srvdsrv' description = 'Service Definition Source'
        lines = lt_lines ) TO rs_info-sections.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
