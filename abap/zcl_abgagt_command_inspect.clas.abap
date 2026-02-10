*"*"use source
*"*"Local Interface:
*"**********************************************************************
" INSPECT command implementation - uses SCI/SCIC for syntax check
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

    METHODS run_syntax_check
      IMPORTING it_objects TYPE ty_object_keys
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
        CLEAR ls_obj.
        ls_obj-objtype = lv_obj_type.
        ls_obj-objname = lv_obj_name.
        APPEND ls_obj TO lt_objects.
      ENDIF.
    ENDLOOP.

    IF lt_objects IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error_count = 1.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    " Run syntax check for all objects together
    ls_result = run_syntax_check( lt_objects ).

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
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
