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

ENDCLASS.

CLASS zcl_abapgit_agent_syntax_agent IMPLEMENTATION.

  METHOD syntax_check.
    DATA ls_error LIKE LINE OF rs_result-errors.

    rs_result-success = abap_true.
    rs_result-object_type = iv_object_type.
    rs_result-object_name = iv_object_name.

    TRY.
        " Build object structure
        DATA: BEGIN OF ls_obj,
                objtype TYPE trobjtype,
                objname TYPE sobj_name,
              END OF ls_obj.
        ls_obj-objtype = iv_object_type.
        ls_obj-objname = iv_object_name.

        DATA lt_objects TYPE scit_objs.
        APPEND ls_obj TO lt_objects.

        " Create object set
        DATA lv_name TYPE sci_objs.
        CONCATENATE 'SYNT_' sy-uname sy-datum sy-uzeit INTO lv_name.

        DATA(lo_objset) = cl_ci_objectset=>save_from_list(
          p_name    = lv_name
          p_objects = lt_objects ).

        " Get check variant for syntax check
        DATA(lo_variant) = cl_ci_checkvariant=>get_ref(
          p_user = ''
          p_name = 'SYNTAX' ). " Use SYNTHETIC_CHECK variant for syntax

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
        lo_inspection->save(
          EXCEPTIONS
            missing_information = 1
            insp_no_name = 2
            not_enqueued = 3
            OTHERS = 4 ).

        " Run inspection
        lo_inspection->run(
          EXPORTING
            p_howtorun = 'D' " Direct run
          EXCEPTIONS
            invalid_check_version = 1
            OTHERS = 2 ).

        " Get results
        DATA lt_list TYPE scit_alvlist.
        lo_inspection->plain_list( IMPORTING p_list = lt_list ).

        " Parse results
        LOOP AT lt_list INTO DATA(ls_list).
          CLEAR ls_error.
          ls_error-line = ls_list-line.
          ls_error-column = ls_list-col.
          ls_error-text = ls_list-message.
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
