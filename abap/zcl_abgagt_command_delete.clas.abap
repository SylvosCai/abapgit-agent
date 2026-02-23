*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_command_delete DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.

    METHODS constructor
      IMPORTING
        io_repo_srv TYPE REF TO zif_abapgit_repo_srv OPTIONAL.

    TYPES: BEGIN OF ty_delete_params,
             url      TYPE string,
             repo_key TYPE string,
           END OF ty_delete_params.

  PRIVATE SECTION.
    DATA mo_repo_srv TYPE REF TO zif_abapgit_repo_srv.

    METHODS get_repo_srv
      RETURNING VALUE(ro_srv) TYPE REF TO zif_abapgit_repo_srv.
    METHODS find_repo
      IMPORTING
        is_params TYPE ty_delete_params
      RETURNING VALUE(ri_repo) TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abgagt_command_delete IMPLEMENTATION.

  METHOD zif_abgagt_command~get_name.
    rv_name = 'DELETE'.
  ENDMETHOD.

  METHOD constructor.
    mo_repo_srv = io_repo_srv.
  ENDMETHOD.

  METHOD get_repo_srv.
    IF mo_repo_srv IS NOT BOUND.
      mo_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).
    ENDIF.
    ro_srv = mo_repo_srv.
  ENDMETHOD.

  METHOD find_repo.
    " Find repository by URL or key
    IF is_params-url IS NOT INITIAL.
      ri_repo = get_repo_srv( )->get_by_url( is_params-url ).
    ELSEIF is_params-repo_key IS NOT INITIAL.
      ri_repo = get_repo_srv( )->get( is_params-repo_key ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_abapgit_exception
        EXPORTING
          text = 'URL or repo_key is required'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_delete_params,
          li_repo TYPE REF TO zif_abapgit_repo.

    IF is_param IS SUPPLIED.
      ls_params = CORRESPONDING #( is_param ).
    ENDIF.

    " Validate that either url or repo_key is provided
    IF ls_params-url IS INITIAL AND ls_params-repo_key IS INITIAL.
      rv_result = '{"success":"","error":"URL or repo_key is required"}'.
      RETURN.
    ENDIF.

    " Find the repository
    TRY.
        li_repo = find_repo( ls_params ).
      CATCH zcx_abapgit_exception INTO DATA(lx_not_found).
        rv_result = '{"success":"","error":"' && lx_not_found->get_text( ) && '"}'.
        RETURN.
    ENDTRY.

    " Delete the repository
    TRY.
        li_repo->delete( ).
        COMMIT WORK AND WAIT.
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        rv_result = '{"success":"","error":"' && lx_error->get_text( ) && '"}'.
        RETURN.
    ENDTRY.

    " Success response
    DATA lv_response TYPE string.
    lv_response = '{"success":"X",'.
    lv_response = lv_response && '"repo_key":"' && li_repo->get_key( ) && '",'.
    lv_response = lv_response && '"message":"Repository deleted successfully"}'.

    rv_result = lv_response.
  ENDMETHOD.

ENDCLASS.
