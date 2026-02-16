*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_preview DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

ENDCLASS.

CLASS zcl_abgagt_command_preview IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_preview.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA lv_json TYPE string.
    DATA lv_tabname TYPE string.
    lv_tabname = 'TADIR'.

    TRY.
        DATA(lv_valid_tab) = cl_abap_dyn_prg=>check_table_name_str(
          val      = to_upper( lv_tabname )
          packages = '$ABAP_AI_BRIDGE' ).

        SELECT SINGLE * FROM (lv_valid_tab) INTO NEW @DATA(ls_data).

        lv_json = '{"success":"X","command":"PREVIEW","message":"OK","data":"' && ls_data->obj_name && '"}'.

      CATCH cx_table cx_abap_not_in_abap_not_a_package INTO DATA(lx_error).
        lv_json = '{"success":"","command":"PREVIEW","message":"' && lx_error->get_text( ) && '"}'.
    ENDTRY.

    rv_result = lv_json.
  ENDMETHOD.

ENDCLASS.
