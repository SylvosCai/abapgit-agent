*"*"use source
*"*"Local Interface:
*"**********************************************************************
" INSPECT command implementation - delegates to agent for syntax check
CLASS zcl_abgagt_command_inspect DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_inspect_params,
             files TYPE string_table,
           END OF ty_inspect_params.

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
          lo_agent TYPE REF TO zcl_abgagt_agent,
          ls_result TYPE zif_abgagt_agent=>ty_inspect_result,
          ls_response TYPE zif_abgagt_agent=>ty_inspect_result,
          lt_objects TYPE zif_abgagt_agent=>ty_object_keys.

    " Parse parameters from is_param
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    IF ls_params-files IS INITIAL.
      ls_response-success = abap_false.
      ls_response-error_count = 1.
      rv_result = /ui2/cl_json=>serialize( data = ls_response ).
      RETURN.
    ENDIF.

    " Parse files to objects
    lo_util = zcl_abgagt_util=>get_instance( ).
    LOOP AT ls_params-files INTO lv_file.
      CLEAR: lv_obj_type, lv_obj_name.
      lo_util->zif_abgagt_util~parse_file_to_object(
        EXPORTING iv_file = lv_file
        IMPORTING ev_obj_type = lv_obj_type
                  ev_obj_name = lv_obj_name ).
      IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
        <ls_obj>-object_type = lv_obj_type.
        <ls_obj>-object_name = lv_obj_name.
      ENDIF.
    ENDLOOP.

    lo_agent = NEW zcl_abgagt_agent( ).
    ls_result = lo_agent->zif_abgagt_agent~inspect(
      it_files = ls_params-files
      it_objects = lt_objects ).

    ls_response-success = ls_result-success.
    ls_response-error_count = ls_result-error_count.
    ls_response-errors = ls_result-errors.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.

ENDCLASS.
