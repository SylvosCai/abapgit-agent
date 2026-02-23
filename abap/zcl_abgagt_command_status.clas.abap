*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_status DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    TYPES: BEGIN OF ty_status_params,
             url TYPE string,
           END OF ty_status_params.

ENDCLASS.

CLASS zcl_abgagt_command_status IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = 'STATUS'.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_status_params.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Validate URL
    IF ls_params-url IS INITIAL.
      rv_result = '{"success":false,"error":"URL is required"}'.
      RETURN.
    ENDIF.

    " Check repo status using agent
    DATA lo_agent TYPE REF TO zcl_abgagt_agent.
    CREATE OBJECT lo_agent.

    DATA(lv_status) = lo_agent->zif_abgagt_agent~get_repo_status( ls_params-url ).

    " Build response
    IF lv_status = 'Found'.
      " Get repo details
      DATA li_repo TYPE REF TO zif_abapgit_repo.
      TRY.
          zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
            EXPORTING iv_url = ls_params-url
            IMPORTING ei_repo = li_repo ).

          DATA(lv_repo_key) = li_repo->get_key( ).
          DATA(lv_package) = li_repo->get_package( ).

          rv_result = '{"success":true,"url":"' && ls_params-url && '","status":"Found",' &&
                      '"repo_key":"' && lv_repo_key && '","package":"' && lv_package && '"}'.
        CATCH zcx_abapgit_exception.
          rv_result = '{"success":true,"url":"' && ls_params-url && '","status":"Found"}'.
      ENDTRY.
    ELSE.
      rv_result = '{"success":true,"url":"' && ls_params-url && '","status":"Not found"}'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
