*"*"use source
*"*"Local Interface:
*"**********************************************************************
" UNIT command implementation - delegates to agent for AUnit tests
CLASS zcl_abgagt_command_unit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_unit_params,
             package TYPE string,
             files TYPE string_table,
           END OF ty_unit_params.

ENDCLASS.

CLASS zcl_abgagt_command_unit IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_unit.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_unit_params,
          lv_file TYPE string,
          lv_obj_type TYPE string,
          lv_obj_name TYPE string,
          lo_util TYPE REF TO zcl_abgagt_util,
          lo_agent TYPE REF TO zcl_abgagt_agent,
          ls_result TYPE zif_abgagt_agent=>ty_unit_result,
          ls_response TYPE zif_abgagt_agent=>ty_unit_result,
          lt_objects TYPE zif_abgagt_agent=>ty_object_keys.

    " Parse parameters from is_param
    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Parse files to objects
    IF ls_params-files IS NOT INITIAL.
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
    ENDIF.

    lo_agent = NEW zcl_abgagt_agent( ).
    ls_result = lo_agent->zif_abgagt_agent~run_tests(
      iv_package = ls_params-package
      it_objects = lt_objects ).

    ls_response-success = ls_result-success.
    ls_response-message = ls_result-message.
    ls_response-test_count = ls_result-test_count.
    ls_response-passed_count = ls_result-passed_count.
    ls_response-failed_count = ls_result-failed_count.
    ls_response-results = ls_result-results.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.

ENDCLASS.
