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

    DATA(ls_result) = run_syntax_check( ls_params-source_name ).
    rv_result = 'test'.
  ENDMETHOD.

  METHOD run_syntax_check RETURNING VALUE(rs_result) TYPE ty_inspect_result.
    RETURN.
  ENDMETHOD.
ENDCLASS.
