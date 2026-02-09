*"*"use source
*"*"Local Interface:
*"**********************************************************************
" UNIT command implementation - runs unit tests
CLASS zcl_abgagt_unit DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_object,
             object_type TYPE string,
             object_name TYPE string,
           END OF ty_object.

    TYPES ty_object_list TYPE STANDARD TABLE OF ty_object WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_unit_params,
             package TYPE string,
             objects TYPE ty_object_list,
             files TYPE string_table,
           END OF ty_unit_params.

ENDCLASS.

CLASS zcl_abgagt_unit IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = 'UNIT'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_unit_params,
          lv_json TYPE string,
          lv_file TYPE string,
          lv_obj_type TYPE string,
          lv_obj_name TYPE string,
          lo_parser TYPE REF TO zcl_abgagt_agent,
          lo_agent TYPE REF TO zcl_abgagt_agent.

    " Parse parameters from JSON (it_files is passed as JSON string)
    IF lines( it_files ) = 1.
      " Single item passed - could be JSON params
      READ TABLE it_files INDEX 1 INTO lv_json.
      IF lv_json CP '*{*' OR lv_json CP '*"*'.
        " Looks like JSON, deserialize
        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_json
          CHANGING data = ls_params ).
      ENDIF.
    ELSEIF lines( it_files ) > 0.
      " Files passed as list - parse to objects
      LOOP AT it_files INTO lv_file.
        " Use agent's parse_file_to_object method
        lo_parser = NEW zcl_abgagt_agent( ).
        lo_parser->parse_file_to_object(
          EXPORTING iv_file = lv_file
          IMPORTING ev_obj_type = lv_obj_type
                    ev_obj_name = lv_obj_name ).
        IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
          APPEND INITIAL LINE TO ls_params-objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
          <ls_obj>-object_type = lv_obj_type.
          <ls_obj>-object_name = lv_obj_name.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Get agent instance and execute unit tests
    lo_agent = NEW zcl_abgagt_agent( ).

    " Convert package if provided
    DATA(lv_package) = COND devclass( WHEN ls_params-package IS NOT INITIAL THEN ls_params-package ).

    " Convert objects to agent format
    DATA lt_objects TYPE zif_abgagt_agent=>ty_object_keys.
    LOOP AT ls_params-objects ASSIGNING FIELD-SYMBOL(<ls_param_obj>).
      APPEND INITIAL LINE TO lt_objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
      <ls_obj>-object_type = <ls_param_obj>-object_type.
      <ls_obj>-object_name = <ls_param_obj>-object_name.
    ENDLOOP.

    DATA(ls_result) = lo_agent->zif_abgagt_agent~run_tests(
      iv_package = lv_package
      it_objects = lt_objects ).

    " Convert result to JSON string using /ui2/cl_json
    DATA: BEGIN OF ls_response,
            success TYPE string,
            message TYPE string,
            test_count TYPE i,
            passed_count TYPE i,
            failed_count TYPE i,
            results TYPE zif_abgagt_agent=>ty_test_results,
          END OF ls_response.

    ls_response-success = COND string( WHEN ls_result-success = abap_true THEN 'X' ELSE '' ).
    ls_response-message = ls_result-message.
    ls_response-test_count = ls_result-test_count.
    ls_response-passed_count = ls_result-passed_count.
    ls_response-failed_count = ls_result-failed_count.
    ls_response-results = ls_result-results.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.

ENDCLASS.
