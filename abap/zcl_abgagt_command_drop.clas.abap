*"* use source
*"* Local Interface:
**********************************************************************
CLASS zcl_abgagt_command_drop DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_drop_params,
             file              TYPE string,
             transport_request TYPE string,
           END OF ty_drop_params.

    METHODS constructor
      IMPORTING
        io_agent TYPE REF TO zif_abgagt_agent OPTIONAL
        io_util  TYPE REF TO zif_abgagt_util  OPTIONAL.

  PRIVATE SECTION.
    DATA mo_agent TYPE REF TO zif_abgagt_agent.

ENDCLASS.

CLASS zcl_abgagt_command_drop IMPLEMENTATION.

  METHOD constructor.
    IF io_agent IS BOUND.
      mo_agent = io_agent.
    ELSE.
      mo_agent = NEW zcl_abgagt_agent( io_util = io_util ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_command~get_name.
    rv_name = 'DROP'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA ls_params TYPE ty_drop_params.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    DATA(ls_result) = mo_agent->drop(
      iv_file              = ls_params-file
      iv_transport_request = ls_params-transport_request ).

    IF ls_result-success = abap_true.
      rv_result = |\{"success":"X","object":"{ ls_result-obj_name }","type":"{ ls_result-obj_type }","message":"{ ls_result-message }"\}|.
    ELSE.
      rv_result = |\{"success":"","error":"{ ls_result-error }"\}|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
