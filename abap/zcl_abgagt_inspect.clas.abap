*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_inspect DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_inspect_params,
             source_name TYPE string,
           END OF ty_inspect_params.

ENDCLASS.

CLASS zcl_abgagt_inspect IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = 'INSPECT'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_inspect_params.

    " Parse parameters from JSON (it_files is passed as JSON string)
    IF lines( it_files ) = 1.
      " Single file passed as string - could be JSON params
      READ TABLE it_files INDEX 1 INTO DATA(lv_json).
      IF lv_json CP '*{*' OR lv_json CP '*"*'.
        " Looks like JSON, deserialize
        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_json
          CHANGING data = ls_params ).
      ELSE.
        " Treat as file list with first element as source_name
        ls_params-source_name = lv_json.
      ENDIF.
    ELSEIF lines( it_files ) > 0.
      " Use first file as source_name
      READ TABLE it_files INDEX 1 INTO ls_params-source_name.
    ENDIF.

    " Get agent instance and execute inspect
    DATA(lo_agent) = NEW zcl_abgagt_agent( ).

    DATA(ls_result) = lo_agent->zif_abgagt_agent~inspect(
      iv_file = ls_params-source_name ).

    " Convert result to JSON string using /ui2/cl_json
    DATA: BEGIN OF ls_response,
            success TYPE string,
            object_type TYPE string,
            object_name TYPE string,
            error_count TYPE i,
            errors TYPE zif_abgagt_agent=>ty_errors,
          END OF ls_response.

    ls_response-success = COND string( WHEN ls_result-success = abap_true THEN 'X' ELSE '' ).
    ls_response-object_type = ls_result-object_type.
    ls_response-object_name = ls_result-object_name.
    ls_response-error_count = ls_result-error_count.
    ls_response-errors = ls_result-errors.

    rv_result = /ui2/cl_json=>serialize( data = ls_response ).
  ENDMETHOD.

ENDCLASS.
