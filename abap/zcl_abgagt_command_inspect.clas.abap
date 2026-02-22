*"*"use source
*"*"Local Interface:
*"**********************************************************************
" INSPECT command implementation - uses SCI/SCIC for code inspection
CLASS zcl_abgagt_command_inspect DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    " Constructor - optionally inject code inspector for testing
    METHODS constructor
      IMPORTING io_inspector TYPE REF TO zif_abgagt_code_inspector OPTIONAL.

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
      RETURNING VALUE(rt_results) TYPE ty_inspect_results.

    " Validate DDLS (CDS views)
    METHODS validate_ddls
      IMPORTING it_ddls_names TYPE ty_ddls_names
      RETURNING VALUE(rt_results) TYPE ty_inspect_results
      RAISING cx_dd_ddl_read.

    " Get method name from TMDIR based on class name and include number
    METHODS get_method_name
      IMPORTING iv_classname    TYPE string
                iv_include_num  TYPE i
      RETURNING VALUE(rv_method_name) TYPE string.

  PRIVATE SECTION.

    " Code inspector - injected for testing
    DATA mo_inspector TYPE REF TO zif_abgagt_code_inspector.

    " Create unique inspection name
    METHODS create_inspection_name
      RETURNING VALUE(rv_name) TYPE sci_objs.

    " Get check variant reference
    METHODS get_check_variant
      IMPORTING iv_variant TYPE string
      RETURNING VALUE(ro_variant) TYPE REF TO cl_ci_checkvariant
      RAISING   cx_static_check.

    " Create and run inspection
    METHODS create_and_run_inspection
      IMPORTING iv_name     TYPE sci_objs
                io_variant  TYPE REF TO cl_ci_checkvariant
                it_objects  TYPE ty_object_keys
      RETURNING VALUE(ro_inspection) TYPE REF TO cl_ci_inspection.

    " Extract method/include name from SOBJNAME
    METHODS extract_method_name
      IMPORTING iv_classname TYPE string
                iv_sobjname  TYPE string
      RETURNING VALUE(rv_method_name) TYPE string.

    " Build result for a single object from inspection list
    METHODS build_object_result
      IMPORTING is_object      TYPE scir_objs
                it_list        TYPE scit_alvlist
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

ENDCLASS.

CLASS zcl_abgagt_command_inspect IMPLEMENTATION.

  METHOD constructor.
    " If no inspector injected, create default (will use static methods)
    " For now, we allow empty - actual implementation will check in methods
    mo_inspector = io_inspector.
  ENDMETHOD.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_inspect.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_inspect_params,
          lv_file TYPE string,
          lv_obj_type TYPE string,
          lv_obj_name TYPE string,
          lo_util TYPE REF TO zcl_abgagt_util,
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
    lo_util = zcl_abgagt_util=>get_instance( ).

    LOOP AT ls_params-files INTO lv_file.
      CLEAR: lv_obj_type, lv_obj_name.
      lo_util->zif_abgagt_util~parse_file_to_object(
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
        iv_variant = ls_params-variant ).
      INSERT LINES OF lt_sci_results INTO TABLE lt_results.
    ENDIF.

    " Run DDLS validation
    IF lt_ddls_names IS NOT INITIAL.
      TRY.
          DATA(lt_ddls_results) = validate_ddls( lt_ddls_names ).
          INSERT LINES OF lt_ddls_results INTO TABLE lt_results.
        CATCH cx_dd_ddl_read.
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
    " Validate DDLS (CDS views) using CL_DD_DDL_HANDLER_FACTORY
    " First checks inactive version ('M'), falls back to active ('A')
    DATA: lv_ddls_name TYPE ddlname,
          lo_handler TYPE REF TO if_dd_ddl_handler,
          ls_ddlsrcv TYPE ddddlsrcv,
          ls_error TYPE ty_error,
          ls_warning TYPE ty_warning,
          lt_warnings TYPE ddl2ddicwarnings,
          lx_error TYPE REF TO cx_dd_ddl_check,
          lv_found TYPE abap_bool,
          ls_result TYPE ty_inspect_result,
          lv_err_msg TYPE string,
          lv_warn_msg TYPE string.

    rt_results = VALUE #( ).

    " Create DDL handler
    lo_handler = cl_dd_ddl_handler_factory=>create( ).

    " Check each DDLS object
    LOOP AT it_ddls_names INTO lv_ddls_name.
      CLEAR: ls_ddlsrcv, lt_warnings, lv_found, ls_result.

      ls_result-object_type = 'DDLS'.
      ls_result-object_name = lv_ddls_name.
      ls_result-success = abap_true.

      " First try to read inactive version (get_state = 'M')
      TRY.
          lo_handler->read(
            EXPORTING
              name       = lv_ddls_name
              get_state  = 'M'
            IMPORTING
              ddddlsrcv_wa = ls_ddlsrcv ).

          IF ls_ddlsrcv-source IS NOT INITIAL.
            lv_found = abap_true.
          ENDIF.

        CATCH cx_dd_ddl_check.
          " Ignore - will try active version
      ENDTRY.

      " If no inactive version, try active version
      IF lv_found = abap_false.
        TRY.
            lo_handler->read(
              EXPORTING
                name       = lv_ddls_name
                get_state  = 'A'
              IMPORTING
                ddddlsrcv_wa = ls_ddlsrcv ).

            IF ls_ddlsrcv-source IS NOT INITIAL.
              lv_found = abap_true.
            ENDIF.
          CATCH cx_dd_ddl_check.
            " Ignore
        ENDTRY.
      ENDIF.

      " If still not found, report error
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

      " Run validation check
      TRY.
          " Get warnings from check method
          lo_handler->check(
            EXPORTING
              name = lv_ddls_name
            IMPORTING
              warnings = lt_warnings
            CHANGING
              ddlsrcv_wa = ls_ddlsrcv ).

          " Parse warnings from check method
          LOOP AT lt_warnings INTO DATA(ls_warn_from_check).
            CLEAR ls_warning.
            ls_warning-type = ls_warn_from_check-type.
            ls_warning-line = ls_warn_from_check-line.
            ls_warning-column = ls_warn_from_check-column.
            ls_warning-severity = ls_warn_from_check-severity.
            ls_warning-object_type = 'DDLS'.
            ls_warning-object_name = lv_ddls_name.
            " Use MESSAGE statement to get real warning message
            MESSAGE ID ls_warn_from_check-arbgb TYPE 'E' NUMBER ls_warn_from_check-msgnr
              WITH ls_warn_from_check-var1 ls_warn_from_check-var2 ls_warn_from_check-var3 ls_warn_from_check-var4
              INTO lv_warn_msg.
            ls_warning-message = lv_warn_msg.
            APPEND ls_warning TO ls_result-warnings.
          ENDLOOP.

          " If no errors and no warnings, validation passed
          IF ls_result-warnings IS INITIAL.
            ls_result-success = abap_true.
          ELSE.
            ls_result-success = abap_false.
          ENDIF.

        CATCH cx_dd_ddl_check INTO lx_error.
          " Validation failed - get error details using get_errors method
          DATA(lt_errors) = lx_error->get_errors( ).
          LOOP AT lt_errors INTO DATA(ls_err).
            CLEAR ls_error.
            ls_error-line = ls_err-line.
            ls_error-column = ls_err-column.
            " Use MESSAGE statement to get real error message
            MESSAGE ID ls_err-arbgb TYPE 'E' NUMBER ls_err-msgnr
              WITH ls_err-var1 ls_err-var2 ls_err-var3 ls_err-var4
              INTO lv_err_msg.
            ls_error-text = lv_err_msg.
            APPEND ls_error TO ls_result-errors.
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
            ls_warning-object_name = lv_ddls_name.
            " Use MESSAGE statement to get real warning message
            MESSAGE ID ls_warn2-arbgb TYPE 'E' NUMBER ls_warn2-msgnr
              WITH ls_warn2-var1 ls_warn2-var2 ls_warn2-var3 ls_warn2-var4
              INTO lv_warn_msg.
            ls_warning-message = lv_warn_msg.
            APPEND ls_warning TO ls_result-warnings.
          ENDLOOP.

          ls_result-success = abap_false.
      ENDTRY.

      ls_result-error_count = lines( ls_result-errors ).
      APPEND ls_result TO rt_results.
    ENDLOOP.
  ENDMETHOD.

  METHOD run_inspection.
    DATA: lv_name TYPE sci_objs,
          lo_objset TYPE REF TO cl_ci_objectset,
          lo_variant TYPE REF TO cl_ci_checkvariant,
          lo_inspection TYPE REF TO cl_ci_inspection,
          lt_list TYPE scit_alvlist,
          ls_result TYPE ty_inspect_result.

    rt_results = VALUE #( ).

    " Create inspection name
    lv_name = create_inspection_name( ).

    " Create object set (using static method - could use inspector if injected)
    lo_objset = cl_ci_objectset=>save_from_list(
      p_name    = lv_name
      p_objects = it_objects ).

    " Get check variant
    TRY.
        lo_variant = get_check_variant( iv_variant ).
      CATCH cx_static_check INTO DATA(lx_variant_error).
        " Variant not found - return error
        rt_results = build_variant_error(
          iv_variant = iv_variant
          iv_subrc = sy-subrc ).
        RETURN.
    ENDTRY.

    " Create and run inspection
    lo_inspection = create_and_run_inspection(
      iv_name     = lv_name
      io_variant  = lo_variant
      it_objects  = it_objects ).

    " Get results
    lo_inspection->plain_list( IMPORTING p_list = lt_list ).

    " Build result for each object
    LOOP AT it_objects INTO DATA(ls_obj).
      rs_result = build_object_result(
        is_object = ls_obj
        it_list   = lt_list ).
      APPEND rs_result TO rt_results.
    ENDLOOP.

    " Cleanup
    cleanup(
      io_inspection = lo_inspection
      io_objset     = lo_objset ).

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
      RAISE EXCEPTION TYPE cx_static_check.
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
        p_objs = it_objects ).

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
    " Normalize multiple '=' to single '=' then split
    DATA lv_normalized TYPE string.
    lv_normalized = iv_sobjname.
    REPLACE ALL OCCURRENCES OF REGEX '=+' IN lv_normalized WITH '='.

    " Split by '=' to get class name and include name
    SPLIT lv_normalized AT '=' INTO DATA(lv_part_class) DATA(lv_include_name).

    " Check include type
    CASE lv_include_name.
      WHEN 'CCAU'.
        " Unit test include
        rv_method_name = 'UNIT TEST'.
      WHEN 'CCDEF'.
        " Local definitions
        rv_method_name = 'LOCAL DEFINITIONS'.
      WHEN 'CCIMP'.
        " Local implementations
        rv_method_name = 'LOCAL IMPLEMENTATIONS'.
      WHEN 'CCINC'.
        " Macros
        rv_method_name = 'MACROS'.
      WHEN OTHERS.
        " Check if it's a method include (CM###)
        IF strlen( lv_include_name ) >= 2 AND lv_include_name(2) = 'CM'.
          " Convert CM003 to 3 (remove CM prefix)
          DATA(lv_num_str) = substring( val = lv_include_name off = 2 ).
          DATA(lv_include_num) = CONV i( lv_num_str ).

          " Get method name from TMDIR
          rv_method_name = get_method_name(
            iv_classname   = iv_classname
            iv_include_num = lv_include_num ).
        ENDIF.
    ENDCASE.
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
        iv_classname = is_object-objname
        iv_sobjname  = ls_list-sobjname ).

      " Categorize message
      categorize_message(
        EXPORTING is_list        = ls_list
                  iv_object_type = is_object-objtype
                  iv_object_name = is_object-objname
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

  METHOD get_method_name.
    " Get method name from TMDIR based on class name and include number
    " iv_include_num: 1 = CM001, 2 = CM002, etc.
    rv_method_name = ''.

    SELECT SINGLE methodname
      FROM tmdir
      INTO rv_method_name
      WHERE classname = iv_classname
        AND methodindx = iv_include_num.
  ENDMETHOD.

ENDCLASS.
