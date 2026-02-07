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
        " Use /SDF/CL_CODE_INSPECTOR to run syntax check
        DATA lt_objects TYPE scit_objs.
        DATA ls_object LIKE LINE OF lt_objects.
        ls_object-objtype = iv_object_type.
        ls_object-objname = iv_object_name.
        APPEND ls_object TO lt_objects.

        DATA lo_inspection TYPE REF TO cl_ci_inspection.
        DATA lv_result TYPE scit_rest.

        " Use the SELECTION method with objects
        DATA: BEGIN OF ls_tadir,
                pgmid    TYPE tadir-pgmid,
                object   TYPE tadir-object,
                obj_name TYPE tadir-obj_name,
                devclass TYPE tadir-devclass,
              END OF ls_tadir.

        ls_tadir-pgmid = 'R3TR'.
        ls_tadir-object = iv_object_type.
        ls_tadir-obj_name = iv_object_name.
        ls_tadir-devclass = '$ABAP_AI_BRIDGE'.

        DATA: BEGIN OF l_sel_flags,
                class TYPE c VALUE 'X',
                fugrs TYPE c VALUE 'X',
                repos TYPE c VALUE 'X',
                wdyns TYPE c VALUE 'X',
                ddics TYPE c VALUE 'X',
                typps TYPE c VALUE 'X',
              END OF l_sel_flags.

        DATA: lv_ok TYPE sychar01.

        " Call the static method
        CALL METHOD /sdf/cl_code_inspector=>selection
          EXPORTING
            p_variant_user = sy-uname
            p_variant      = 'SYNTAX'
            p_tadir        = ls_tadir
            p_function_groups = VALUE scisfugrs( )
            p_reports     = VALUE scisrepos( )
            p_classes     = VALUE scisclass( )
            p_ddic_types  = VALUE scisdics( )
            p_type_pools  = VALUE scistypps( )
          IMPORTING
            p_ok          = lv_ok
            p_result       = lv_result
            p_inspection   = lo_inspection.

        IF lv_ok = 'X' AND lo_inspection IS BOUND.
          " Get results from the inspection
          DATA lt_results TYPE scit_rest.
          CALL METHOD lo_inspection->get_results
            IMPORTING
              p_scirest_ps = lt_results.

          " Parse results
          DATA lv_msg TYPE string.
          LOOP AT lt_results INTO DATA(ls_res).
            CLEAR ls_error.
            " Extract line and column from message
            IF ls_res-message CP 'Line *'.
              FIND REGEX 'Line ([0-9]+), Column ([0-9]+): (.*)' IN ls_res-message
                SUBMATCHINGS DATA(lv_line) DATA(lv_col) DATA(lv_msg).
              IF sy-subrc = 0.
                ls_error-line = lv_line.
                ls_error-column = lv_col.
                ls_error-text = lv_msg.
              ELSE.
                ls_error-line = '1'.
                ls_error-column = '1'.
                ls_error-text = ls_res-message.
              ENDIF.
            ELSE.
              ls_error-line = '1'.
              ls_error-column = '1'.
              ls_error-text = ls_res-message.
            ENDIF.
            ls_error-word = ls_res-code.
            APPEND ls_error TO rs_result-errors.
          ENDLOOP.
        ENDIF.

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
