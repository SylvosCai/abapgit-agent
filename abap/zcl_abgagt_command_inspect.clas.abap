*"*"use source
*"*"Local Interface:
*"**********************************************************************
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
             object_type TYPE string,
             object_name TYPE string,
             error_count TYPE i,
             errors TYPE ty_errors,
           END OF ty_inspect_result.

    TYPES: BEGIN OF ty_inspect_params,
             source_name TYPE string,
           END OF ty_inspect_params.

ENDCLASS.

CLASS zcl_abgagt_command_inspect IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_inspect.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_inspect_params,
          lv_json TYPE string.

    " Parse parameters from JSON (it_files is passed as JSON string)
    IF lines( it_files ) = 1.
      READ TABLE it_files INDEX 1 INTO lv_json.
      IF lv_json CP '*{*' OR lv_json CP '*"*'.
        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_json
          CHANGING data = ls_params ).
      ELSE.
        ls_params-source_name = lv_json.
      ENDIF.
    ELSEIF lines( it_files ) > 0.
      READ TABLE it_files INDEX 1 INTO ls_params-source_name.
    ENDIF.

    " Execute syntax check using SAP standard code
    DATA(ls_result) = run_syntax_check( ls_params-source_name ).

    " Convert result to JSON
    DATA: BEGIN OF ls_response,
            success TYPE string,
            object_type TYPE string,
            object_name TYPE string,
            error_count TYPE i,
            errors TYPE ty_errors,
          END OF ls_response.

    ls_response-success = COND string( WHEN ls_result-success = abap_true THEN 'X' ELSE '' ).
    ls_response-object_type = ls_result-object_type.
    ls_response-object_name = ls_result-object_name.
    ls_response-error_count = ls_result-error_count.
    ls_response-errors = ls_result-errors.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.

  METHOD run_syntax_check.
    " Parse file to object type and name
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.
    parse_file_to_object(
      EXPORTING iv_file = iv_file
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    rs_result-success = abap_true.
    rs_result-object_type = lv_obj_type.
    rs_result-object_name = lv_obj_name.

    TRY.
        " Check if object exists in TADIR
        DATA lv_devclass TYPE devclass.
        SELECT SINGLE devclass FROM tadir
          INTO lv_devclass
          WHERE pgmid = 'R3TR'
            AND object = lv_obj_type
            AND obj_name = lv_obj_name.

        IF lv_devclass IS INITIAL.
          rs_result-success = abap_false.
          rs_result-error_count = 1.
          DATA(ls_error) = VALUE ty_error( line = '1' column = '1' text = |Object { lv_obj_type } { lv_obj_name } does not exist| word = '' ).
          APPEND ls_error TO rs_result-errors.
          RETURN.
        ENDIF.

        " Create object structure
        DATA ls_obj TYPE scir_objs.
        ls_obj-objtype = lv_obj_type.
        ls_obj-objname = lv_obj_name.

        DATA lt_objects TYPE scit_objs.
        APPEND ls_obj TO lt_objects.

        " Create unique name for inspection
        DATA lv_name TYPE sci_objs.
        CONCATENATE 'SYNT_' sy-uname sy-datum sy-uzeit INTO lv_name.

        " Create object set
        DATA(lo_objset) = cl_ci_objectset=>save_from_list(
          p_name    = lv_name
          p_objects = lt_objects ).

        " Get check variant
        DATA(lo_variant) = cl_ci_checkvariant=>get_ref(
          p_user = ''
          p_name = 'SYNTAX_CHECK' ).

        " Create inspection
        cl_ci_inspection=>create(
          EXPORTING
            p_user = sy-uname
            p_name = lv_name
          RECEIVING
            p_ref = DATA(lo_inspection) ).

        " Set inspection
        lo_inspection->set(
          EXPORTING
            p_chkv = lo_variant
            p_objs = lo_objset ).

        " Save and run
        lo_inspection->save( ).
        lo_inspection->run(
          EXPORTING
            p_howtorun = 'R'
          EXCEPTIONS
            invalid_check_version = 1
            OTHERS = 2 ).

        " Get results
        DATA lt_list TYPE scit_alvlist.
        lo_inspection->plain_list( IMPORTING p_list = lt_list ).

        " Parse results
        LOOP AT lt_list INTO DATA(ls_list).
          IF ls_list-objtype = lv_obj_type AND ls_list-objname = lv_obj_name.
            ls_error = VALUE #( line = ls_list-line column = ls_list-col text = ls_list-text word = ls_list-code ).
            APPEND ls_error TO rs_result-errors.
          ENDIF.
        ENDLOOP.

        " Cleanup
        lo_inspection->delete( EXCEPTIONS locked = 1 error_in_enqueue = 2 not_authorized = 3 exceptn_appl_exists = 4 OTHERS = 5 ).
        lo_objset->delete( EXCEPTIONS exists_in_insp = 1 locked = 2 error_in_enqueue = 3 not_authorized = 4 exists_in_objs = 5 OTHERS = 6 ).

        rs_result-error_count = lines( rs_result-errors ).
        IF rs_result-error_count > 0.
          rs_result-success = abap_false.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-error_count = 1.
        ls_error = VALUE #( line = '1' column = '1' text = lx_error->get_text( ) word = '' ).
        APPEND ls_error TO rs_result-errors.
    ENDTRY.
  ENDMETHOD.

  METHOD parse_file_to_object.
    DATA lv_upper TYPE string.
    lv_upper = iv_file.
    TRANSLATE lv_upper TO UPPER CASE.

    DATA lt_parts TYPE TABLE OF string.
    SPLIT lv_upper AT '.' INTO TABLE lt_parts.
    DATA lv_part_count TYPE i.
    lv_part_count = lines( lt_parts ).

    IF lv_part_count < 3.
      RETURN.
    ENDIF.

    READ TABLE lt_parts INDEX lv_part_count INTO DATA(lv_last).
    IF lv_last <> 'ABAP'.
      RETURN.
    ENDIF.

    DATA lv_obj_name TYPE string.
    DATA lv_obj_type_raw TYPE string.
    READ TABLE lt_parts INDEX 1 INTO lv_obj_name.
    READ TABLE lt_parts INDEX 2 INTO lv_obj_type_raw.

    CASE lv_obj_type_raw.
      WHEN 'CLAS' OR 'CLASS'.
        ev_obj_type = 'CLAS'.
      WHEN 'INTF' OR 'INTERFACE'.
        ev_obj_type = 'INTF'.
      WHEN 'PROG' OR 'PROGRAM'.
        ev_obj_type = 'PROG'.
      WHEN 'FUGR' OR 'FUGROUP'.
        ev_obj_type = 'FUGR'.
      WHEN 'TABL' OR 'TABLE'.
        ev_obj_type = 'TABL'.
      WHEN 'DDLS'.
        ev_obj_type = 'DDLS'.
      WHEN OTHERS.
        ev_obj_type = lv_obj_type_raw.
    ENDCASE.

    DATA lv_len TYPE i.
    lv_len = strlen( lv_obj_name ).
    DATA lv_offs TYPE i.
    lv_offs = find( val = reverse( lv_obj_name ) sub = '/' ).
    IF lv_offs > 0.
      lv_offs = lv_len - lv_offs - 1.
      lv_obj_name = lv_obj_name+lv_offs.
    ENDIF.

    IF lv_obj_name(1) = '/'.
      lv_obj_name = lv_obj_name+1.
    ENDIF.

    ev_obj_name = lv_obj_name.
  ENDMETHOD.

ENDCLASS.
