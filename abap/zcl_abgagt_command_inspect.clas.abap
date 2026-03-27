*"*"use source
*"*"Local Interface:
*"**********************************************************************
" INSPECT command implementation - uses SCI/SCIC for code inspection
CLASS zcl_abgagt_command_inspect DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    " Constructor - optionally inject code inspector and DDL handler for testing
    METHODS constructor
      IMPORTING io_inspector TYPE REF TO zif_abgagt_code_inspector OPTIONAL
                io_ddl_handler TYPE REF TO zif_abgagt_ddl_handler OPTIONAL.

    " Error structure for syntax check results
    TYPES: BEGIN OF ty_error,
             line TYPE string,
             column TYPE string,
             text TYPE string,
             word TYPE string,
             sobjname TYPE string,
             method_name TYPE string,
           END OF ty_error.

    " Structure for TMDIR lookup
    TYPES: BEGIN OF ty_tmdir,
             classname TYPE seoclsname,
             methodindx TYPE i,
             methodname TYPE seocmpname,
           END OF ty_tmdir.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

    " Generic warning structure (for DDLS and other objects)
    TYPES: BEGIN OF ty_warning,
             type TYPE string,
             line TYPE string,
             column TYPE string,
             severity TYPE string,
             message TYPE string,
             object_type TYPE string,
             object_name TYPE string,
             sobjname TYPE string,
             method_name TYPE string,
           END OF ty_warning.

    TYPES ty_warnings TYPE STANDARD TABLE OF ty_warning WITH NON-UNIQUE DEFAULT KEY.

    " Info structure (same as warning)
    TYPES: BEGIN OF ty_info,
             type TYPE string,
             line TYPE string,
             column TYPE string,
             severity TYPE string,
             message TYPE string,
             object_type TYPE string,
             object_name TYPE string,
             sobjname TYPE string,
             method_name TYPE string,
           END OF ty_info.

    TYPES ty_infos TYPE STANDARD TABLE OF ty_info WITH NON-UNIQUE DEFAULT KEY.

    " Result for each individual object
    TYPES: BEGIN OF ty_inspect_result,
             object_type TYPE string,
             object_name TYPE string,
             success TYPE abap_bool,
             error_count TYPE i,
             errors TYPE ty_errors,
             warnings TYPE ty_warnings,
             infos TYPE ty_infos,
           END OF ty_inspect_result.

    " Table to hold results for multiple objects
    TYPES ty_inspect_results TYPE STANDARD TABLE OF ty_inspect_result WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_inspect_params,
             files TYPE string_table,
             variant TYPE string,
           END OF ty_inspect_params.

    TYPES ty_object_keys TYPE TABLE OF scir_objs WITH NON-UNIQUE DEFAULT KEY.

    " Type for DDLS object names
    TYPES ty_ddls_names TYPE STANDARD TABLE OF tadir-obj_name WITH NON-UNIQUE DEFAULT KEY.

    " Run inspection for given objects
    METHODS run_inspection
      IMPORTING it_objects TYPE ty_object_keys
                iv_variant TYPE string DEFAULT 'SYNTAX_CHECK'
                io_util    TYPE REF TO zif_abgagt_util
      RETURNING VALUE(rt_results) TYPE ty_inspect_results.

    " Validate DDLS (CDS views)
    METHODS validate_ddls
      IMPORTING it_ddls_names TYPE ty_ddls_names
      RETURNING VALUE(rt_results) TYPE ty_inspect_results
      RAISING cx_dd_ddl_check.

    " Read DDLS source (for testing)
    METHODS read_ddls_source
      IMPORTING iv_ddls_name   TYPE ddlname
      EXPORTING es_ddlsrcv     TYPE any
      RETURNING VALUE(rv_found) TYPE abap_bool
      RAISING   cx_dd_ddl_check.

    " Validate DDLS and build result (for testing)
    METHODS validate_ddls_check
      IMPORTING iv_ddls_name   TYPE ddlname
                is_ddlsrcv     TYPE any
      RETURNING VALUE(rs_result) TYPE ty_inspect_result
      RAISING   cx_dd_ddl_check.

  PRIVATE SECTION.

    " Code inspector - injected for testing
    DATA mo_inspector TYPE REF TO zif_abgagt_code_inspector.

    " DDL handler - injected for testing
    DATA mo_ddl_handler TYPE REF TO zif_abgagt_ddl_handler.

    " Create unique inspection name
    METHODS create_inspection_name
      RETURNING VALUE(rv_name) TYPE sci_objs.

    " Get check variant reference (returns null if not found)
    METHODS get_check_variant
      IMPORTING iv_variant TYPE string
      RETURNING VALUE(ro_variant) TYPE REF TO cl_ci_checkvariant.

    " Create and run inspection
    METHODS create_and_run_inspection
      IMPORTING iv_name     TYPE sci_objs
                io_variant  TYPE REF TO cl_ci_checkvariant
                io_objset   TYPE REF TO cl_ci_objectset
      RETURNING VALUE(ro_inspection) TYPE REF TO cl_ci_inspection.

    " Extract method/include name from SOBJNAME
    METHODS extract_method_name
      IMPORTING iv_classname TYPE string
                iv_sobjname  TYPE string
                io_util      TYPE REF TO zif_abgagt_util
      RETURNING VALUE(rv_method_name) TYPE string.

    " Build result for a single object from inspection list
    METHODS build_object_result
      IMPORTING is_object      TYPE scir_objs
                it_list        TYPE scit_alvlist
                io_util        TYPE REF TO zif_abgagt_util
      RETURNING VALUE(rs_result) TYPE ty_inspect_result.

    " Categorize message into error/warning/info
    METHODS categorize_message
      IMPORTING is_list       TYPE scir_alvlist
                iv_object_type TYPE string
                iv_object_name TYPE string
                iv_method_name TYPE string
      EXPORTING es_error      TYPE ty_error
                es_warning    TYPE ty_warning
                es_info       TYPE ty_info.

    " Cleanup inspection and object set
    METHODS cleanup
      IMPORTING io_inspection TYPE REF TO cl_ci_inspection
                io_objset     TYPE REF TO cl_ci_objectset.

    " Build error result from exception
    METHODS build_error_result
      IMPORTING it_objects   TYPE ty_object_keys
                ix_error     TYPE REF TO cx_root
      RETURNING VALUE(rt_results) TYPE ty_inspect_results.

    " Build variant not found error result
    METHODS build_variant_error
      IMPORTING iv_variant TYPE string
                iv_subrc   TYPE sysubrc
      RETURNING VALUE(rt_results) TYPE ty_inspect_results.

    " Get default DDL handler if not injected
    METHODS get_ddl_handler
      RETURNING VALUE(ro_handler) TYPE REF TO zif_abgagt_ddl_handler.

ENDCLASS.

CLASS zcl_abgagt_command_inspect IMPLEMENTATION.

  METHOD constructor.
    " If no inspector/ddl_handler injected, create default (will use static methods)
    mo_inspector = io_inspector.
    mo_ddl_handler = io_ddl_handler.
  ENDMETHOD.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_inspect.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_inspect_params,
          lv_obj_type TYPE string,
          lv_obj_name TYPE string,
          lt_results TYPE ty_inspect_results,
          lt_objects TYPE ty_object_keys,
          lt_ddls_names TYPE ty_ddls_names,
          ls_obj TYPE scir_objs,
          ls_result TYPE ty_inspect_result.

    " Parse parameters from is_param
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-files IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error_count = 1.
      ls_result-object_name = 'N/A'.
      ls_result-object_type = 'N/A'.
      APPEND ls_result TO lt_results.
      rv_result = /ui2/cl_json=>serialize( data = lt_results ).
      RETURN.
    ENDIF.

    " Parse all files to objects
    DATA(lo_util) = zcl_abgagt_util=>get_instance( ).

    LOOP AT ls_params-files INTO DATA(lv_file).
      CLEAR: lv_obj_type, lv_obj_name.
      lo_util->parse_file_to_object(
        EXPORTING iv_file = lv_file
        IMPORTING ev_obj_type = lv_obj_type
                  ev_obj_name = lv_obj_name ).

      IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
        " Separate DDLS from other objects
        IF lv_obj_type = 'DDLS'.
          APPEND lv_obj_name TO lt_ddls_names.
        ELSE.
          CLEAR ls_obj.
          ls_obj-objtype = lv_obj_type.
          ls_obj-objname = lv_obj_name.
          APPEND ls_obj TO lt_objects.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Run inspection for non-DDLS objects
    IF lt_objects IS NOT INITIAL.
      DATA(lt_sci_results) = run_inspection(
        it_objects = lt_objects
        iv_variant = ls_params-variant
        io_util    = lo_util ).
      INSERT LINES OF lt_sci_results INTO TABLE lt_results.
    ENDIF.

    " Run DDLS validation
    IF lt_ddls_names IS NOT INITIAL.
      TRY.
          DATA(lt_ddls_results) = validate_ddls( lt_ddls_names ).
          INSERT LINES OF lt_ddls_results INTO TABLE lt_results.
        CATCH cx_dd_ddl_check.
          " Ignore - validation handled in method
      ENDTRY.
    ENDIF.

    " If no valid objects found
    IF lt_objects IS INITIAL AND lt_ddls_names IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error_count = 1.
      ls_result-object_name = 'N/A'.
      ls_result-object_type = 'N/A'.
      APPEND ls_result TO lt_results.
    ENDIF.

    rv_result = /ui2/cl_json=>serialize( data = lt_results ).
  ENDMETHOD.

  METHOD validate_ddls.
    " Validate DDLS (CDS views) using DDL handler
    " First checks inactive version ('M'), falls back to active ('A')
    DATA: lv_ddls_name TYPE ddlname,
          ls_ddlsrcv TYPE zif_abgagt_ddl_handler=>ty_ddlsrcv,
          ls_error TYPE ty_error,
          ls_result TYPE ty_inspect_result,
          lv_found TYPE abap_bool.

    rt_results = VALUE #( ).

    " Check each DDLS object
    LOOP AT it_ddls_names INTO lv_ddls_name.
      CLEAR: ls_ddlsrcv, lv_found, ls_result.

      ls_result-object_type = 'DDLS'.
      ls_result-object_name = lv_ddls_name.
      ls_result-success = abap_true.

      " Try to read DDLS source (handles M/A fallback internally)
      TRY.
          lv_found = read_ddls_source(
            EXPORTING
              iv_ddls_name = lv_ddls_name
            IMPORTING
              es_ddlsrcv   = ls_ddlsrcv ).
        CATCH cx_dd_ddl_check.
          " Will report not found below
      ENDTRY.

      " If not found, report error
      IF lv_found = abap_false.
        ls_error-line = '1'.
        ls_error-column = '1'.
        ls_error-text = |DDLS { lv_ddls_name } not found in repository|.
        APPEND ls_error TO ls_result-errors.
        ls_result-success = abap_false.
        ls_result-error_count = lines( ls_result-errors ).
        APPEND ls_result TO rt_results.
        CONTINUE.
      ENDIF.

      " Run validation check using modular method
      ls_result = validate_ddls_check(
        iv_ddls_name = lv_ddls_name
        is_ddlsrcv   = ls_ddlsrcv ).

      APPEND ls_result TO rt_results.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_ddls_source.
    " Read DDLS source - first try inactive, then active
    DATA: ls_ddlsrcv TYPE zif_abgagt_ddl_handler=>ty_ddlsrcv.

    DATA(lo_handler) = get_ddl_handler( ).

    " First try to read inactive version (get_state = 'M')
    TRY.
        lo_handler->read(
          EXPORTING
            iv_name      = iv_ddls_name
            iv_get_state = 'M'
          IMPORTING
            es_ddlsrcv   = ls_ddlsrcv ).

        IF ls_ddlsrcv-source IS NOT INITIAL.
          es_ddlsrcv = ls_ddlsrcv.
          rv_found = abap_true.
          RETURN.
        ENDIF.
      CATCH cx_dd_ddl_check.
        " Ignore - will try active version
    ENDTRY.

    " If no inactive version, try active version
    TRY.
        lo_handler->read(
          EXPORTING
            iv_name      = iv_ddls_name
            iv_get_state = 'A'
          IMPORTING
            es_ddlsrcv   = ls_ddlsrcv ).

        IF ls_ddlsrcv-source IS NOT INITIAL.
          es_ddlsrcv = ls_ddlsrcv.
          rv_found = abap_true.
        ENDIF.
      CATCH cx_dd_ddl_check.
        " Not found in either version
    ENDTRY.
  ENDMETHOD.

  METHOD validate_ddls_check.
    " Validate DDLS and build result
    DATA: ls_ddlsrcv TYPE zif_abgagt_ddl_handler=>ty_ddlsrcv,
          lx_error TYPE REF TO cx_dd_ddl_check,
          ls_warning TYPE ty_warning,
          lv_warn_msg TYPE string,
          lv_err_msg TYPE string.

    ls_ddlsrcv = is_ddlsrcv.
    rs_result-object_type = 'DDLS'.
    rs_result-object_name = iv_ddls_name.
    rs_result-success = abap_true.

    DATA(lo_handler) = get_ddl_handler( ).

    " Run validation check
    TRY.
        lo_handler->check(
          EXPORTING
            iv_name       = iv_ddls_name
          CHANGING
            cs_ddlsrcv    = ls_ddlsrcv ).

        " Get warnings after check
        DATA(lt_warnings) = lo_handler->get_warnings( ).

        " Parse warnings from check method
        LOOP AT lt_warnings INTO DATA(ls_warn_from_check).
          CLEAR ls_warning.
          ls_warning-type = ls_warn_from_check-type.
          ls_warning-line = ls_warn_from_check-line.
          ls_warning-column = ls_warn_from_check-column.
          ls_warning-severity = ls_warn_from_check-severity.
          ls_warning-object_type = 'DDLS'.
          ls_warning-object_name = iv_ddls_name.
          " Use MESSAGE statement to get real warning message
          MESSAGE ID ls_warn_from_check-arbgb TYPE 'E' NUMBER ls_warn_from_check-msgnr
            WITH ls_warn_from_check-var1 ls_warn_from_check-var2 ls_warn_from_check-var3 ls_warn_from_check-var4
            INTO lv_warn_msg.
          ls_warning-message = lv_warn_msg.
          APPEND ls_warning TO rs_result-warnings.
        ENDLOOP.

        " If no errors and no warnings, validation passed
        IF rs_result-warnings IS INITIAL.
          rs_result-success = abap_true.
        ELSE.
          rs_result-success = abap_false.
        ENDIF.

      CATCH cx_dd_ddl_check INTO lx_error.
        " Validation failed - get error details using get_errors method
        DATA(lt_errors) = lx_error->get_errors( ).
        LOOP AT lt_errors INTO DATA(ls_err).
          DATA(ls_error) = VALUE ty_error( ).
          ls_error-line = ls_err-line.
          ls_error-column = ls_err-column.
          " Use MESSAGE statement to get real error message
          MESSAGE ID ls_err-arbgb TYPE 'E' NUMBER ls_err-msgnr
            WITH ls_err-var1 ls_err-var2 ls_err-var3 ls_err-var4
            INTO lv_err_msg.
          ls_error-text = lv_err_msg.
          APPEND ls_error TO rs_result-errors.
        ENDLOOP.

        " Also get warnings if any
        DATA(lt_warn) = lx_error->get_warnings( ).
        LOOP AT lt_warn INTO DATA(ls_warn2).
          CLEAR ls_warning.
          ls_warning-type = ls_warn2-type.
          ls_warning-line = ls_warn2-line.
          ls_warning-column = ls_warn2-column.
          ls_warning-severity = ls_warn2-severity.
          ls_warning-object_type = 'DDLS'.
          ls_warning-object_name = iv_ddls_name.
          MESSAGE ID ls_warn2-arbgb TYPE 'E' NUMBER ls_warn2-msgnr
            WITH ls_warn2-var1 ls_warn2-var2 ls_warn2-var3 ls_warn2-var4
            INTO lv_warn_msg.
          ls_warning-message = lv_warn_msg.
          APPEND ls_warning TO rs_result-warnings.
        ENDLOOP.

        rs_result-success = abap_false.
    ENDTRY.

    rs_result-error_count = lines( rs_result-errors ).
  ENDMETHOD.

  METHOD get_ddl_handler.
    " Return injected handler or create default wrapper
    IF mo_ddl_handler IS BOUND.
      ro_handler = mo_ddl_handler.
    ELSE.
      " Create default wrapper that uses real DDL handler
      ro_handler = NEW lcl_ddl_handler_default( ).
    ENDIF.
  ENDMETHOD.

  METHOD run_inspection.
    DATA: lv_name TYPE sci_objs,
          lo_objset TYPE REF TO cl_ci_objectset,
          lo_variant TYPE REF TO cl_ci_checkvariant,
          lo_inspection TYPE REF TO cl_ci_inspection,
          lt_list TYPE scit_alvlist.

    DATA(lo_util) = io_util.

    rt_results = VALUE #( ).

    " Create inspection name
    lv_name = create_inspection_name( ).

    " Use injected inspector or fall back to static SAP calls
    IF mo_inspector IS BOUND.
      " Use injected inspector (for testing)
      TRY.
          lo_objset = mo_inspector->create_object_set(
            iv_name    = lv_name
            it_objects = it_objects ).

          lo_variant = mo_inspector->get_check_variant( iv_variant ).

          IF lo_variant IS NOT BOUND.
            rt_results = build_variant_error(
              iv_variant = iv_variant
              iv_subrc = 1 ).
            RETURN.
          ENDIF.

          lo_inspection = mo_inspector->create_and_run_inspection(
            iv_name     = lv_name
            io_variant  = lo_variant
            io_objset   = lo_objset ).

          lt_list = mo_inspector->get_results( lo_inspection ).

          mo_inspector->cleanup(
            io_inspection = lo_inspection
            io_objset    = lo_objset ).
        CATCH cx_root INTO DATA(lx_inspector_error).
          rt_results = build_error_result(
            it_objects = it_objects
            ix_error   = lx_inspector_error ).
          RETURN.
      ENDTRY.
    ELSE.
      " Use static SAP methods (production)
      TRY.
          lo_objset = cl_ci_objectset=>save_from_list(
            p_name    = lv_name
            p_objects = it_objects ).

          lo_variant = get_check_variant( iv_variant ).

          IF lo_variant IS NOT BOUND.
            rt_results = build_variant_error(
              iv_variant = iv_variant
              iv_subrc = 1 ).
            RETURN.
          ENDIF.

          lo_inspection = create_and_run_inspection(
            iv_name     = lv_name
            io_variant  = lo_variant
            io_objset   = lo_objset ).

          lo_inspection->plain_list( IMPORTING p_list = lt_list ).

          cleanup(
            io_inspection = lo_inspection
            io_objset     = lo_objset ).
        CATCH cx_root INTO DATA(lx_error).
          " Handle exception - build error result
          rt_results = build_error_result(
            it_objects = it_objects
            ix_error   = lx_error ).
          RETURN.
      ENDTRY.
    ENDIF.

    " Build result for each object
    LOOP AT it_objects INTO DATA(ls_obj).
      DATA(ls_result) = build_object_result(
        is_object = ls_obj
        it_list   = lt_list
        io_util   = lo_util ).
      APPEND ls_result TO rt_results.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_inspection_name.
    CONCATENATE 'SYNT_' sy-uname sy-datum sy-uzeit INTO rv_name.
  ENDMETHOD.

  METHOD get_check_variant.
    " Default to SYNTAX_CHECK if not provided
    DATA lv_variant TYPE sci_chkv.
    lv_variant = iv_variant.
    IF lv_variant IS INITIAL.
      lv_variant = 'SYNTAX_CHECK'.
    ENDIF.

    " Get variant reference
    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user = ''
        p_name = lv_variant
      RECEIVING
        p_ref = ro_variant
      EXCEPTIONS
        chkv_not_exists = 1
        missing_parameter = 2
        broken_variant = 3
        OTHERS = 4 ).

    IF sy-subrc <> 0.
      " Variant not found - return empty (caller will handle)
      CLEAR ro_variant.
    ENDIF.
  ENDMETHOD.

  METHOD create_and_run_inspection.
    " Create inspection
    cl_ci_inspection=>create(
      EXPORTING
        p_user = sy-uname
        p_name = iv_name
      RECEIVING
        p_ref = ro_inspection ).

    " Set inspection with object set and variant
    ro_inspection->set(
      EXPORTING
        p_chkv = io_variant
        p_objs = io_objset ).

    " Save inspection
    ro_inspection->save( ).

    " Run inspection
    ro_inspection->run(
      EXPORTING
        p_howtorun = 'R'
      EXCEPTIONS
        invalid_check_version = 1
        OTHERS = 2 ).
  ENDMETHOD.

  METHOD extract_method_name.
    " Extract include name from SOBJNAME (format: CLASSNAME{multiple====}INCLUDE)
    " Split by '=' to get class name and include name
    DATA: lt_parts TYPE TABLE OF string,
          lv_left TYPE string.

    SPLIT iv_sobjname AT '=' INTO TABLE lt_parts.

    LOOP AT lt_parts INTO DATA(lv_part) WHERE table_line IS NOT INITIAL.
      IF lv_left IS INITIAL.
        lv_left = lv_part.
      ELSE.
        DATA(lv_right) = lv_part.
      ENDIF.
    ENDLOOP.

    DATA(lv_include_name) = lv_right.

    " Only CM### includes need method name from TMDIR
    IF strlen( lv_include_name ) >= 2 AND lv_include_name(2) = 'CM'.
      " Convert method index from base-36
      DATA(lv_method_index) = io_util->convert_method_index( lv_include_name ).
      " Get method name from TMDIR
      rv_method_name = io_util->get_method_name(
        iv_classname   = iv_classname
        iv_method_index = lv_method_index ).
    ENDIF.
  ENDMETHOD.

  METHOD build_object_result.
    DATA: ls_list TYPE scir_alvlist,
          ls_error TYPE ty_error,
          ls_warning TYPE ty_warning,
          ls_info TYPE ty_info.

    rs_result-object_type = is_object-objtype.
    rs_result-object_name = is_object-objname.
    rs_result-success = abap_true.

    " Get errors and warnings for this object
    LOOP AT it_list INTO ls_list WHERE objname = is_object-objname.
      " Extract method name from SOBJNAME
      DATA(lv_method_name) = extract_method_name(
        iv_classname = CONV #( is_object-objname )
        iv_sobjname  = CONV #( ls_list-sobjname )
        io_util      = io_util ).

      " Categorize message
      categorize_message(
        EXPORTING is_list        = ls_list
                  iv_object_type = CONV #( is_object-objtype )
                  iv_object_name = CONV #( is_object-objname )
                  iv_method_name = lv_method_name
        IMPORTING es_error       = ls_error
                  es_warning     = ls_warning
                  es_info        = ls_info ).

      IF ls_error IS NOT INITIAL.
        APPEND ls_error TO rs_result-errors.
      ENDIF.
      IF ls_warning IS NOT INITIAL.
        APPEND ls_warning TO rs_result-warnings.
      ENDIF.
      IF ls_info IS NOT INITIAL.
        APPEND ls_info TO rs_result-infos.
      ENDIF.
    ENDLOOP.

    " Sort results
    SORT rs_result-errors BY method_name line.
    SORT rs_result-warnings BY method_name line.
    SORT rs_result-infos BY method_name line.

    " Set error count and success flag
    rs_result-error_count = lines( rs_result-errors ).
    IF rs_result-error_count > 0.
      rs_result-success = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD categorize_message.
    " Initialize output parameters
    CLEAR es_error.
    CLEAR es_warning.
    CLEAR es_info.

    CASE is_list-kind.
      WHEN 'E'.
        " Error
        es_error-line = is_list-line.
        es_error-column = is_list-col.
        es_error-text = is_list-text.
        es_error-word = is_list-code.
        es_error-sobjname = is_list-sobjname.
        es_error-method_name = iv_method_name.

      WHEN 'W'.
        " Warning
        es_warning-line = is_list-line.
        es_warning-column = is_list-col.
        es_warning-severity = is_list-kind.
        es_warning-message = is_list-text.
        es_warning-object_type = iv_object_type.
        es_warning-object_name = iv_object_name.
        es_warning-sobjname = is_list-sobjname.
        es_warning-method_name = iv_method_name.

      WHEN 'I'.
        " Info
        es_info-line = is_list-line.
        es_info-column = is_list-col.
        es_info-severity = is_list-kind.
        es_info-message = is_list-text.
        es_info-object_type = iv_object_type.
        es_info-object_name = iv_object_name.
        es_info-sobjname = is_list-sobjname.
        es_info-method_name = iv_method_name.
    ENDCASE.
  ENDMETHOD.

  METHOD cleanup.
    io_inspection->delete(
      EXCEPTIONS
        locked = 1
        error_in_enqueue = 2
        not_authorized = 3
        exceptn_appl_exists = 4
        OTHERS = 5 ).

    io_objset->delete(
      EXCEPTIONS
        exists_in_insp = 1
        locked = 2
        error_in_enqueue = 3
        not_authorized = 4
        exists_in_objs = 5
        OTHERS = 6 ).
  ENDMETHOD.

  METHOD build_error_result.
    DATA: ls_result TYPE ty_inspect_result,
          ls_error TYPE ty_error.

    LOOP AT it_objects INTO DATA(ls_obj).
      CLEAR ls_result.
      ls_result-object_type = ls_obj-objtype.
      ls_result-object_name = ls_obj-objname.
      ls_result-success = abap_false.
      ls_error-line = '1'.
      ls_error-column = '1'.
      ls_error-text = ix_error->get_text( ).
      APPEND ls_error TO ls_result-errors.
      ls_result-error_count = 1.
      APPEND ls_result TO rt_results.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_variant_error.
    DATA: ls_result TYPE ty_inspect_result,
          ls_error TYPE ty_error.

    ls_result-object_type = 'VARIANT'.
    ls_result-object_name = iv_variant.
    ls_result-success = abap_false.
    ls_result-error_count = 1.
    ls_error-line = '0'.
    ls_error-column = '0'.
    ls_error-text = |Check variant "{ iv_variant }" not found (rc={ iv_subrc })|.
    APPEND ls_error TO ls_result-errors.
    APPEND ls_result TO rt_results.
  ENDMETHOD.

ENDCLASS.
