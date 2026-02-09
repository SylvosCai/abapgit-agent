*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abapgit_agent_syntax_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_error,
             line TYPE string,
             column TYPE string,
             text TYPE string,
             word TYPE string,
           END OF ty_error.

    TYPES ty_errors TYPE STANDARD TABLE OF ty_error WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             success TYPE abap_bool,
             object_type TYPE string,
             object_name TYPE string,
             error_count TYPE i,
             errors TYPE ty_errors,
           END OF ty_result.

    METHODS syntax_check
      IMPORTING
        iv_object_type TYPE string
        iv_object_name TYPE string
      RETURNING
        VALUE(rs_result) TYPE ty_result.

    METHODS parse_file_to_object
      IMPORTING
        iv_file TYPE string
      EXPORTING
        ev_obj_type TYPE string
        ev_obj_name TYPE string.

ENDCLASS.

CLASS zcl_abapgit_agent_syntax_agent IMPLEMENTATION.

  METHOD parse_file_to_object.
    " Parse file path to extract obj_type and obj_name
    " Example: "zcl_ai_math.clas.abap" -> CLAS, ZCL_AI_MATH
    " Example: "src/zcl_my_class.clas.abap" -> CLAS, ZCL_MY_CLASS

    DATA lv_upper TYPE string.
    lv_upper = iv_file.
    TRANSLATE lv_upper TO UPPER CASE.

    " Split filename by '.' to get parts
    DATA lt_parts TYPE TABLE OF string.
    SPLIT lv_upper AT '.' INTO TABLE lt_parts.
    DATA lv_part_count TYPE i.
    lv_part_count = lines( lt_parts ).

    IF lv_part_count < 3.
      RETURN.
    ENDIF.

    " Last part should be 'ABAP' for verification
    READ TABLE lt_parts INDEX lv_part_count INTO DATA(lv_last).
    IF lv_last <> 'ABAP'.
      RETURN.
    ENDIF.

    " First part is obj_name (may contain path), second part is obj_type
    DATA lv_obj_name TYPE string.
    READ TABLE lt_parts INDEX 1 INTO lv_obj_name.
    READ TABLE lt_parts INDEX 2 INTO ev_obj_type.

    " Extract file name from obj_name (remove path prefix)
    DATA lv_len TYPE i.
    lv_len = strlen( lv_obj_name ).
    DATA lv_offs TYPE i.
    lv_offs = find( val = reverse( lv_obj_name ) sub = '/' ).
    IF lv_offs > 0.
      DATA lv_start TYPE i.
      lv_start = lv_len - lv_offs - 1.
      lv_obj_name = lv_obj_name+lv_start.
    ENDIF.

    " Remove leading '/' if present
    IF lv_obj_name(1) = '/'.
      lv_obj_name = lv_obj_name+1.
    ENDIF.

    ev_obj_name = lv_obj_name.
  ENDMETHOD.

  METHOD syntax_check.
    DATA ls_error LIKE LINE OF rs_result-errors.

    rs_result-success = abap_true.
    rs_result-object_type = iv_object_type.
    rs_result-object_name = iv_object_name.

    TRY.
        " Check if object exists in TADIR
        DATA lv_devclass TYPE devclass.
        SELECT SINGLE devclass FROM tadir
          INTO lv_devclass
          WHERE pgmid = 'R3TR'
            AND object = iv_object_type
            AND obj_name = iv_object_name.

        IF lv_devclass IS INITIAL.
          rs_result-success = abap_false.
          rs_result-error_count = 1.
          ls_error-line = '1'.
          ls_error-column = '1'.
          ls_error-text = |Object { iv_object_type } { iv_object_name } does not exist|.
          ls_error-word = ''.
          APPEND ls_error TO rs_result-errors.
          RETURN.
        ENDIF.

        " Create object structure for the specific object
        DATA ls_obj TYPE scir_objs.
        ls_obj-objtype = iv_object_type.
        ls_obj-objname = iv_object_name.

        DATA lt_objects TYPE scit_objs.
        APPEND ls_obj TO lt_objects.

        " Create unique name for inspection
        DATA lv_name TYPE sci_objs.
        CONCATENATE 'SYNT_' sy-uname sy-datum sy-uzeit INTO lv_name.

        " Create object set
        DATA(lo_objset) = cl_ci_objectset=>save_from_list(
          p_name    = lv_name
          p_objects = lt_objects ).

        " Get check variant for syntax check
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

        " Set inspection with object set and variant
        lo_inspection->set(
          EXPORTING
            p_chkv = lo_variant
            p_objs = lo_objset ).

        " Save inspection
        lo_inspection->save( ).

        " Run inspection directly
        lo_inspection->run(
          EXPORTING
            p_howtorun = 'D'
          EXCEPTIONS
            invalid_check_version = 1
            OTHERS = 2 ).

        " Get results
        DATA lt_list TYPE scit_alvlist.
        lo_inspection->plain_list( IMPORTING p_list = lt_list ).

        " Parse results
        LOOP AT lt_list INTO DATA(ls_list).
          " Only include errors for the requested object
          IF ls_list-objtype = iv_object_type AND ls_list-objname = iv_object_name.
            CLEAR ls_error.
            ls_error-line = ls_list-line.
            ls_error-column = ls_list-col.
            ls_error-text = ls_list-text.
            ls_error-word = ls_list-code.
            APPEND ls_error TO rs_result-errors.
          ENDIF.
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

      CATCH cx_root INTO DATA(lx_error).
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
