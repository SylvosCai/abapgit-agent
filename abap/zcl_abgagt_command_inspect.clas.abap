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
             files TYPE string_table,
           END OF ty_inspect_params.
ENDCLASS.

CLASS zcl_abgagt_command_inspect IMPLEMENTATION.
  METHOD zif_abgagt_command~get_name.
    rv_name = zif_abgagt_command=>gc_inspect.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_inspect_params,
          lv_json TYPE string,
          lv_file TYPE string,
          lv_obj_type TYPE string,
          lv_obj_name TYPE string,
          lo_util TYPE REF TO zcl_abgagt_util,
          lo_agent TYPE REF TO zcl_abgagt_agent,
          ls_result TYPE ty_inspect_result,
          ls_file_result TYPE ty_inspect_result,
          lt_all_errors TYPE ty_errors.

    " Parse parameters from JSON (it_files is passed as JSON string)
    IF lines( it_files ) = 1.
      READ TABLE it_files INDEX 1 INTO lv_json.
      IF lv_json CP '*{*' OR lv_json CP '*"*'.
        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_json
          CHANGING data = ls_params ).
      ELSE.
        ls_params-files = it_files.
      ENDIF.
    ELSEIF lines( it_files ) > 0.
      ls_params-files = it_files.
    ENDIF.

    IF ls_params-files IS INITIAL.
      ls_result-success = abap_false.
      ls_result-error_count = 1.
      rv_result = /ui2/cl_json=>serialize( data = ls_result ).
      RETURN.
    ENDIF.

    lo_util = zcl_abgagt_util=>get_instance( ).
    lo_agent = NEW zcl_abgagt_agent( ).

    LOOP AT ls_params-files INTO lv_file.
      CLEAR: lv_obj_type, lv_obj_name.
      lo_util->zif_abgagt_util~parse_file_to_object(
        EXPORTING iv_file = lv_file
        IMPORTING ev_obj_type = lv_obj_type
                  ev_obj_name = lv_obj_name ).

      IF lv_obj_type IS NOT INITIAL AND lv_obj_name IS NOT INITIAL.
        ls_file_result = lo_agent->zif_abgagt_agent~inspect( lv_file ).

        IF ls_result-success = abap_true.
          ls_result-success = ls_file_result-success.
        ENDIF.

        APPEND LINES OF ls_file_result-errors TO lt_all_errors.
      ENDIF.
    ENDLOOP.

    ls_result-error_count = lines( lt_all_errors ).
    ls_result-errors = lt_all_errors.
    IF ls_result-error_count > 0.
      ls_result-success = abap_false.
    ENDIF.

    rv_result = /ui2/cl_json=>serialize( data = ls_result ).
  ENDMETHOD.
ENDCLASS.
