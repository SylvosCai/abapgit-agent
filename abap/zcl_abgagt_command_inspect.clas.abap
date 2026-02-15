*"*"use source
*"*"Local Interface:
*"**********************************************************************
" INSPECT command implementation - uses SCI/SCIC for syntax check
" DDLS objects are validated using RUT_DDL_VALIDATE
CLASS zcl_abgagt_command_inspect DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_error,
             line TYPE string,
             column TYPE string,
             text TYPE string,
             word TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_inspect_result,
             success TYPE abap_bool,
             error_count TYPE i,
             errors TYPE ty_errors,
           END OF ty_inspect_result.

    TYPES: BEGIN OF ty_inspect_params,
             files TYPE string_table,
           END OF ty_inspect_params.

    TYPES ty_object_keys TYPE TABLE OF scir_objs WITH NON-UNIQUE DEFAULT KEY.

    " Type for DDLS objects (name only, no objtype needed)
    TYPES ty_ddls_names TYPE TABLE OF ddlname WITH NON-UNIQUE DEFAULT KEY.

    METHODS run_syntax_check
      IMPORTING it_objects TYPE ty_object_keys
      RETURNING VALUE(rs_result) TYPE ty_inspect_result.

    " New method for DDLS validation
    METHODS run_ddls_validation
      IMPORTING it_ddls_names TYPE ty_ddls_names
      RETURNING VALUE(rs_result) TYPE ty_inspect_result.

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
          ls_result TYPE ty_inspect_result,
          lt_objects TYPE ty_object_keys,
          lt_ddls_names TYPE ty_ddls_names,
          ls_obj TYPE scir_objs.

    " Parse parameters from is_param
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-files IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error_count = 1.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
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
        " Separate DDLS from other object types
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

    " Initialize result
    rs_result-success = abap_true.
    rs_result-error_count = 0.

    " Run syntax check for non-DDLS objects
    IF lt_objects IS NOT EMPTY.
      ls_result = run_syntax_check( lt_objects ).
      " Merge results
      INSERT LINES OF ls_result-errors INTO TABLE rs_result-errors.
      rs_result-error_count = rs_result-error_count + ls_result-error_count.
      IF ls_result-success = abap_false.
        rs_result-success = abap_false.
      ENDIF.
    ENDIF.

    " Run validation for DDLS objects
    IF lt_ddls_names IS NOT EMPTY.
      ls_result = run_ddls_validation( lt_ddls_names ).
      " Merge results
      INSERT LINES OF ls_result-errors INTO TABLE rs_result-errors.
      rs_result-error_count = rs_result-error_count + ls_result-error_count.
      IF ls_result-success = abap_false.
        rs_result-success = abap_false.
      ENDIF.
    ENDIF.

    IF lt_objects IS INITIAL AND lt_ddls_names IS INITIAL.
      rs_result-success = abap_false.
      rs_result-error_count = 1.
      rv_result = /ui2/cl_json=>serialize( data = rs_result ).
      RETURN.
    ENDIF.

    rv_result = /ui2/cl_json=>serialize( data = rs_result ).
  ENDMETHOD.

  METHOD run_ddls_validation.
    " Validate DDLS (CDS views/view entities) using DDL validation API
    DATA: lv_ddls_name TYPE ddlname,
          lo_validator TYPE REF TO object,
          lv_source TYPE string,
          lt_messages TYPE STANDARD TABLE OF string,
          ls_error TYPE ty_error.

    rs_result-success = abap_true.

    " Try to use RUT_DDL_VALIDATE for DDL validation
    TRY.
        " Create validator instance
        CALL METHOD cl_dd_validator_factory=>create_validator
          RECEIVING
            r_validator = lo_validator.

        IF lo_validator IS BOUND.
          " Validate each DDLS
          LOOP AT it_ddls_names INTO lv_ddls_name.
            CLEAR: lt_messages, lv_source.

            " Get the DDL source
            SELECT SINGLE ddltext
              FROM dddls
              INTO lv_source
              WHERE ddlname = lv_ddls_name.

            IF sy-subrc = 0 AND lv_source IS NOT INITIAL.
              " Validate the DDL source
              CALL METHOD lo_validator->validate_ddl
                EXPORTING
                  ddl_name  = lv_ddls_name
                  ddl_text  = lv_source
                IMPORTING
                  messages  = lt_messages.

              " Parse messages into errors
              LOOP AT lt_messages INTO DATA(lv_message).
                CLEAR ls_error.
                ls_error-line = '1'.
                ls_error-column = '1'.
                ls_error-text = lv_message.
                APPEND ls_error TO rs_result-errors.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ELSE.
          " Validator not available - try alternative approach
          rs_result-error_count = 1.
          ls_error-line = '1'.
          ls_error-column = '1'.
          ls_error-text = 'DDL validator not available in this system'.
          APPEND ls_error TO rs_result-errors.
          rs_result-success = abap_false.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " Fallback: Try to get error from DDLS runtime
        rs_result-error_count = 1.
        ls_error-line = '1'.
        ls_error-column = '1'.
        ls_error-text = lx_error->get_text( ).
        APPEND ls_error TO rs_result-errors.
        rs_result-success = abap_false.
    ENDTRY.

    " If no errors from validation but we want to check activation
    IF rs_result-success = abap_true AND rs_result-errors IS INITIAL.
      " Try alternative: check if objects are active
      DATA lv_objname TYPE tadir-objname.
      LOOP AT it_ddls_names INTO lv_ddls_name.
        CLEAR lv_objname.
        SELECT SINGLE objname
          FROM tadir
          INTO lv_objname
          WHERE objname = lv_ddls_name
            AND object = 'DDLS'.

        IF sy-subrc <> 0.
          CLEAR ls_error.
          ls_error-line = '1'.
          ls_error-column = '1'.
          ls_error-text = |DDLS { lv_ddls_name } not found in repository|.
          APPEND ls_error TO rs_result-errors.
        ENDIF.
      ENDLOOP.

      IF rs_result-errors IS NOT INITIAL.
        rs_result-success = abap_false.
      ENDIF.
    ENDIF.

    rs_result-error_count = lines( rs_result-errors ).
  ENDMETHOD.

  METHOD run_syntax_check.
    DATA: lv_name TYPE sci_objs,
          lo_objset TYPE REF TO cl_ci_objectset,
          lo_variant TYPE REF TO cl_ci_checkvariant,
          lo_inspection TYPE REF TO cl_ci_inspection,
          lt_list TYPE scit_alvlist,
          ls_error TYPE ty_error,
          lx_error TYPE REF TO cx_root.

    rs_result-success = abap_true.

    TRY.
        " Create unique name for inspection
        CONCATENATE 'SYNT_' sy-uname sy-datum sy-uzeit INTO lv_name.

        " Create object set
        lo_objset = cl_ci_objectset=>save_from_list(
          p_name    = lv_name
          p_objects = it_objects ).

        " Get check variant for syntax check
        lo_variant = cl_ci_checkvariant=>get_ref(
          p_user = ''
          p_name = 'SYNTAX_CHECK' ).

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

        " Parse results - aggregate all errors
        LOOP AT lt_list INTO DATA(ls_list).
          CLEAR ls_error.
          ls_error-line = ls_list-line.
          ls_error-column = ls_list-col.
          ls_error-text = ls_list-text.
          ls_error-word = ls_list-code.
          APPEND ls_error TO rs_result-errors.
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

        rs_result-error_count = lines( rs_result-errors ).
        IF rs_result-error_count > 0.
          rs_result-success = abap_false.
        ENDIF.

      CATCH cx_root INTO lx_error.
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        ls_error-line = '1'.
        ls_error-column = '1'.
        ls_error-text = lx_error->get_text( ).
        ls_error-word = ''.
        APPEND ls_error TO rs_result-errors.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
