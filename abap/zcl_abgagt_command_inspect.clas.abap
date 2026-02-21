*"*"use source
*"*"Local Interface:
*"**********************************************************************
" INSPECT command implementation - uses SCI/SCIC for syntax check
CLASS zcl_abgagt_command_inspect DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

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

    METHODS run_syntax_check
      IMPORTING it_objects TYPE ty_object_keys
                iv_variant TYPE string DEFAULT 'SYNTAX_CHECK'
      RETURNING VALUE(rt_results) TYPE ty_inspect_results.

    " Validate DDLS (CDS views)
    METHODS validate_ddls
      IMPORTING it_ddls_names TYPE ty_ddls_names
      RETURNING VALUE(rt_results) TYPE ty_inspect_results.

    " Get method name from TMDIR based on class name and include number
    METHODS get_method_name
      IMPORTING iv_classname    TYPE string
                iv_include_num  TYPE i
      RETURNING VALUE(rv_method_name) TYPE string.

ENDCLASS.

CLASS zcl_abgagt_command_inspect IMPLEMENTATION.

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

    " Run syntax check for non-DDLS objects
    IF lt_objects IS NOT INITIAL.
      DATA(lt_sci_results) = run_syntax_check(
        it_objects = lt_objects
        iv_variant = ls_params-variant ).
      INSERT LINES OF lt_sci_results INTO TABLE lt_results.
    ENDIF.

    " Run DDLS validation
    IF lt_ddls_names IS NOT INITIAL.
      DATA(lt_ddls_results) = validate_ddls( lt_ddls_names ).
      INSERT LINES OF lt_ddls_results INTO TABLE lt_results.
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

  METHOD run_syntax_check.
    DATA: lv_name TYPE sci_objs,
          lo_objset TYPE REF TO cl_ci_objectset,
          lo_variant TYPE REF TO cl_ci_checkvariant,
          lo_inspection TYPE REF TO cl_ci_inspection,
          lt_list TYPE scit_alvlist,
          ls_error TYPE ty_error,
          ls_warning TYPE ty_warning,
          ls_list TYPE scir_alvlist,
          lx_error TYPE REF TO cx_root,
          ls_result TYPE ty_inspect_result.

    rt_results = VALUE #( ).

    TRY.
        " Create unique name for inspection
        CONCATENATE 'SYNT_' sy-uname sy-datum sy-uzeit INTO lv_name.

        " Create object set
        lo_objset = cl_ci_objectset=>save_from_list(
          p_name    = lv_name
          p_objects = it_objects ).

        " Get check variant (default: SYNTAX_CHECK)
        DATA lv_variant TYPE sci_chkv.
        lv_variant = iv_variant.
        IF lv_variant IS INITIAL.
          lv_variant = 'SYNTAX_CHECK'.
        ENDIF.

        " Try to get the variant using EXCEPTIONS
        cl_ci_checkvariant=>get_ref(
          EXPORTING
            p_user = ''
            p_name = lv_variant
          RECEIVING
            p_ref = lo_variant
          EXCEPTIONS
            chkv_not_exists = 1
            missing_parameter = 2
            broken_variant = 3
            OTHERS = 4 ).

        IF sy-subrc <> 0.
          " Variant not found, return error
          ls_result-object_type = 'VARIANT'.
          ls_result-object_name = lv_variant.
          ls_result-success = abap_false.
          ls_result-error_count = 1.
          APPEND VALUE #( line = '0' column = '0' text = |Check variant "{ lv_variant }" not found (rc={ sy-subrc })| word = '' ) TO ls_result-errors.
          APPEND ls_result TO rt_results.
          RETURN.
        ENDIF.

        " Create inspection
        cl_ci_inspection=>create(
          EXPORTING
            p_user = sy-uname
            p_name = lv_name
          RECEIVING
            p_ref = lo_inspection ).

        " Set inspection with object set and variant
        lo_inspection->set(
          EXPORTING
            p_chkv = lo_variant
            p_objs = lo_objset ).

        " Save inspection
        lo_inspection->save( ).

        " Run inspection
        lo_inspection->run(
          EXPORTING
            p_howtorun = 'R'
          EXCEPTIONS
            invalid_check_version = 1
            OTHERS = 2 ).

        " Get results
        lo_inspection->plain_list( IMPORTING p_list = lt_list ).

        " Build result for each object
        LOOP AT it_objects INTO DATA(ls_obj).
          CLEAR ls_result.
          ls_result-object_type = ls_obj-objtype.
          ls_result-object_name = ls_obj-objname.
          ls_result-success = abap_true.

          " Get errors and warnings for this object
          LOOP AT lt_list INTO ls_list WHERE objname = ls_obj-objname.
            " Extract method name from SOBJNAME (format: CLASSNAME====CM###)
            DATA lv_classname TYPE string.
            lv_classname = ls_obj-objname.
            DATA(lv_include_str) = ls_list-sobjname.
            DATA lv_method_name TYPE string.

            " Get last 3 characters (CM003) and extract numeric part
            DATA(lv_cm_part) = substring(
              val = lv_include_str
              off = strlen( lv_include_str ) - 3
              len = 3 ).
            " Convert CM003 to 3 (remove CM prefix, get last 1 char)
            DATA(lv_num_str) = substring( val = lv_cm_part off = 2 ).
            DATA(lv_include_num) = CONV i( lv_num_str ).

            " Get method name from TMDIR
            lv_method_name = get_method_name(
              iv_classname   = lv_classname
              iv_include_num = lv_include_num ).

            " Check severity - 'E' = Error, 'W' = Warning, 'I' = Info
            IF ls_list-kind = 'E'.
              " Error
              CLEAR ls_error.
              ls_error-line = ls_list-line.
              ls_error-column = ls_list-col.
              ls_error-text = ls_list-text.
              ls_error-word = ls_list-code.
              ls_error-sobjname = ls_list-sobjname.
              ls_error-method_name = lv_method_name.
              APPEND ls_error TO ls_result-errors.
            ELSEIF ls_list-kind = 'W'.
              " Warning
              CLEAR ls_warning.
              ls_warning-line = ls_list-line.
              ls_warning-column = ls_list-col.
              ls_warning-severity = ls_list-kind.
              ls_warning-message = ls_list-text.
              ls_warning-object_type = ls_obj-objtype.
              ls_warning-object_name = ls_obj-objname.
              ls_warning-sobjname = ls_list-sobjname.
              ls_warning-method_name = lv_method_name.
              APPEND ls_warning TO ls_result-warnings.
            ELSEIF ls_list-kind = 'I'.
              " Info
              CLEAR ls_warning.
              ls_warning-line = ls_list-line.
              ls_warning-column = ls_list-col.
              ls_warning-severity = ls_list-kind.
              ls_warning-message = ls_list-text.
              ls_warning-object_type = ls_obj-objtype.
              ls_warning-object_name = ls_obj-objname.
              ls_warning-sobjname = ls_list-sobjname.
              ls_warning-method_name = lv_method_name.
              APPEND ls_warning TO ls_result-infos.
            ENDIF.
          ENDLOOP.

          " Sort errors by method_name and line
          SORT ls_result-errors BY method_name line.
          " Sort warnings by method_name and line
          SORT ls_result-warnings BY method_name line.
          " Sort infos by method_name and line
          SORT ls_result-infos BY method_name line.

          ls_result-error_count = lines( ls_result-errors ).
          IF ls_result-error_count > 0.
            ls_result-success = abap_false.
          ENDIF.

          APPEND ls_result TO rt_results.
        ENDLOOP.

        " Cleanup
        lo_inspection->delete(
          EXCEPTIONS
            locked = 1
            error_in_enqueue = 2
            not_authorized = 3
            exceptn_appl_exists = 4
            OTHERS = 5 ).

        lo_objset->delete(
          EXCEPTIONS
            exists_in_insp = 1
            locked = 2
            error_in_enqueue = 3
            not_authorized = 4
            exists_in_objs = 5
            OTHERS = 6 ).

      CATCH cx_root INTO lx_error.
        " Return error for all objects
        LOOP AT it_objects INTO ls_obj.
          CLEAR ls_result.
          ls_result-object_type = ls_obj-objtype.
          ls_result-object_name = ls_obj-objname.
          ls_result-success = abap_false.
          ls_error-line = '1'.
          ls_error-column = '1'.
          ls_error-text = lx_error->get_text( ).
          APPEND ls_error TO ls_result-errors.
          ls_result-error_count = 1.
          APPEND ls_result TO rt_results.
        ENDLOOP.
    ENDTRY.
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
