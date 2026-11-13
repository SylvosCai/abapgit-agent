CLASS zcl_abgagt_rap_syntax_util DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS check
      IMPORTING iv_type TYPE string iv_name TYPE clike it_source TYPE string_table
      RETURNING VALUE(rs_result) TYPE zif_abgagt_syntax_checker=>ty_result.
  PRIVATE SECTION.
    CLASS-METHODS convert_checklist
      IMPORTING io_log TYPE REF TO cl_wb_checklist
      CHANGING cs_result TYPE zif_abgagt_syntax_checker=>ty_result.
ENDCLASS.

CLASS zcl_abgagt_rap_syntax_util IMPLEMENTATION.
  METHOD check.
    rs_result-object_type = iv_type.
    rs_result-object_name = iv_name.
    rs_result-error_count = 1.
    DATA(lv_source) = concat_lines_of( table = it_source sep = cl_abap_char_utilities=>newline ).
    IF it_source IS INITIAL OR lv_source IS INITIAL.
      rs_result-message = 'No source provided'.
      rs_result-errors = VALUE #( ( line = 1 text = rs_result-message ) ).
      RETURN.
    ENDIF.

    IF iv_type <> 'BDEF' AND iv_type <> 'SRVD' AND iv_type <> 'DDLX'.
      rs_result-message = |Unsupported RAP syntax type: { iv_type }|.
      rs_result-errors = VALUE #( ( text = rs_result-message ) ).
      RETURN.
    ENDIF.
    DATA lr_data TYPE REF TO data.
    DATA lo_operator TYPE REF TO if_wb_object_operator.
    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_field> TYPE any.
    FIELD-SYMBOLS <lv_model_name> TYPE any.
    FIELD-SYMBOLS <lv_model_type> TYPE any.
    FIELD-SYMBOLS <lv_description> TYPE any.
    TRY.
        DATA(lv_data_type) = SWITCH string( iv_type
          WHEN 'BDEF' THEN 'CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA'
          WHEN 'SRVD' THEN 'CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA'
          WHEN 'DDLX' THEN 'CL_DDLX_WB_OBJECT_DATA=>TY_OBJECT_DATA' ).
        DATA ls_type TYPE wbobjtype.
        cl_wb_object_type=>get_global_from_transport_type( EXPORTING p_transport_type = CONV #( iv_type )
          IMPORTING p_global_type = ls_type EXCEPTIONS no_unique_mapping = 1 ).
        IF sy-subrc <> 0.
          rs_result-message = |Native Workbench type mapping unavailable for { iv_type }|.
          rs_result-errors = VALUE #( ( text = rs_result-message ) ).
          RETURN.
        ENDIF.
        CREATE DATA lr_data TYPE (lv_data_type).
        ASSIGN lr_data->* TO <ls_data>.
        ASSIGN COMPONENT 'METADATA-NAME' OF STRUCTURE <ls_data> TO <lv_model_name>.
        ASSIGN COMPONENT 'METADATA-TYPE' OF STRUCTURE <ls_data> TO <lv_model_type>.
        ASSIGN COMPONENT 'METADATA-DESCRIPTION' OF STRUCTURE <ls_data> TO <lv_description>.
        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_data> TO <lv_field>.
        IF <lv_model_name> IS NOT ASSIGNED OR <lv_model_type> IS NOT ASSIGNED
          OR <lv_description> IS NOT ASSIGNED OR <lv_field> IS NOT ASSIGNED.
          rs_result-message = |Incompatible native { iv_type } source model|.
          rs_result-errors = VALUE #( ( text = rs_result-message ) ).
          RETURN.
        ENDIF.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING object_type = ls_type object_key = CONV seu_objkey( iv_name ) do_commits = abap_false
          RECEIVING result = lo_operator.

        SELECT SINGLE FROM tadir FIELDS obj_name
          WHERE pgmid = 'R3TR' AND object = @iv_type AND obj_name = @iv_name
          INTO @DATA(lv_existing).
        IF lv_existing IS NOT INITIAL.
          lo_operator->read( EXPORTING version = 'A' IMPORTING data = <ls_data> ).
        ELSE.
          <lv_model_name> = iv_name.
          <lv_model_type> = |{ ls_type-objtype_tr }/{ ls_type-subtype_wb }|.
          <lv_description> = iv_name.
          ASSIGN COMPONENT 'METADATA-ABAP_LANGU_VERSION' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_language_version>).
          IF <lv_language_version> IS NOT ASSIGNED.
            rs_result-message = |Incompatible native { iv_type } source model|.
            rs_result-errors = VALUE #( ( text = rs_result-message ) ).
            RETURN.
          ENDIF.
          " shortcut: Syntax has no target package; use package defaults when requests supply one.
          <lv_language_version> = cl_blue_wb_utility=>get_default_abap_langu_version(
            i_objtype_tr = CONV #( iv_type ) i_package = CONV #( '' ) ).
        ENDIF.
        <lv_field> = lv_source.
        IF iv_type = 'BDEF' AND lv_existing IS INITIAL.
          " Workbench replaces new-include attributes with empty REPOSRC data; supply virtual attributes instead.
          DATA(lv_include) = cl_bdef_name_service=>get_bd_include( CONV #( iv_name ) ).
          DATA(lt_virtual_source) = CONV rsfb_source( it_source ).
          DATA(ls_attributes) = VALUE trdir( name = lv_include subc = 'B' fixpt = abap_true
            uccheck = CONV #( <lv_language_version> ) ).
          DATA(lo_checker) = cl_seu_adt_checkable_helper=>create_instance(
            res_type = CONV #( if_bdef_adt_ressources=>co_transport_type ) ).
          lo_checker->check_source( EXPORTING main_program = lv_include
            replacing_tab = VALUE #( ( name = lv_include source = REF #( lt_virtual_source )
              trdir = REF #( ls_attributes ) ) )
            IMPORTING check_result_adt = DATA(ls_check) error = DATA(ls_error) subrc = DATA(lv_subrc) ).
          DATA(lo_log) = NEW cl_wb_checklist( ).
          lo_checker->fill_checklist( wb_checklist = lo_log check_result_adt = ls_check ).
          IF lv_subrc <> 0 AND lo_log->has_error_messages( ) = abap_false.
            lo_log->add_error_message( p_message_text = VALUE #( ( CONV string( ls_error-message ) ) )
              p_line = ls_error-line ).
          ENDIF.
        ELSE.
          lo_operator->check( EXPORTING data = <ls_data>
            p_check_reporter = if_adt_check_reporter=>c_syntax_check_runner IMPORTING logger = lo_log ).
        ENDIF.
        IF lo_log IS NOT BOUND.
          rs_result-message = 'Native RAP checker returned no checklist'.
          rs_result-errors = VALUE #( ( text = rs_result-message ) ).
          RETURN.
        ENDIF.

        convert_checklist( EXPORTING io_log = lo_log CHANGING cs_result = rs_result ).
      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        rs_result-message = |Native { iv_type } syntax check failed: { lx_error->get_text( ) }|.
        rs_result-errors = VALUE #( ( text = rs_result-message ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD convert_checklist.
    io_log->get_message_number( IMPORTING p_error_number = DATA(lv_errors) p_warning_number = DATA(lv_warnings) ).
    DO lv_errors TIMES.
      io_log->get_error_message( EXPORTING p_index = sy-index
        IMPORTING p_message_text = DATA(lv_text) p_line = DATA(lv_line) EXCEPTIONS entry_not_found = 1 ).
      IF sy-subrc <> 0.
        lv_text = VALUE #( ( `Native RAP diagnostic could not be read` ) ).
      ENDIF.
      APPEND VALUE #( line = lv_line text = concat_lines_of( table = lv_text sep = cl_abap_char_utilities=>newline ) )
        TO cs_result-errors.
    ENDDO.
    DO lv_warnings TIMES.
      io_log->get_error_message( EXPORTING p_index = sy-index p_message_type = 'W'
        IMPORTING p_message_text = lv_text p_line = lv_line EXCEPTIONS entry_not_found = 1 ).
      IF sy-subrc <> 0.
        lv_text = VALUE #( ( `Native RAP warning could not be read` ) ).
      ENDIF.
      APPEND VALUE #( line = lv_line text = concat_lines_of( table = lv_text sep = cl_abap_char_utilities=>newline )
        severity = 'W' ) TO cs_result-warnings.
    ENDDO.
    cs_result-error_count = lines( cs_result-errors ).
    cs_result-success = xsdbool( cs_result-error_count = 0 ).
    cs_result-message = COND #( WHEN cs_result-success = abap_true THEN 'Syntax check passed'
      ELSE |Syntax check failed with { cs_result-error_count } error(s)| ).
  ENDMETHOD.
ENDCLASS.
