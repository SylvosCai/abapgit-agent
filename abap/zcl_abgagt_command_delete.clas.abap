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
    " Find repository by URL
    IF is_params-url IS NOT INITIAL.
      get_repo_srv( )->get_repo_from_url(
        EXPORTING iv_url = is_params-url
        IMPORTING ei_repo = ri_repo ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abgagt_command~execute.
    DATA: ls_params TYPE ty_delete_params,
          li_repo TYPE REF TO zif_abapgit_repo.

    IF is_param IS SUPPLIED.
      MOVE-CORRESPONDING is_param TO ls_params.
    ENDIF.

    " Validate that URL is provided
    IF ls_params-url IS INITIAL.
      rv_result = '{"success":"","error":"URL is required"}'.
      RETURN.
    ENDIF.

    " Find the repository
    TRY.
        li_repo = find_repo( ls_params ).
      CATCH zcx_abapgit_exception INTO DATA(lx_not_found).
        rv_result = '{"success":"","error":"' && lx_not_found->get_text( ) && '"}'.
        RETURN.
    ENDTRY.

    IF li_repo IS NOT BOUND.
      rv_result = '{"success":"","error":"Repository not found"}'.
      RETURN.
    ENDIF.

    " Delete the repository
    TRY.
        get_repo_srv( )->delete( ii_repo = li_repo ).
        COMMIT WORK AND WAIT.
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        rv_result = '{"success":"","error":"' && lx_error->get_text( ) && '"}'.
        RETURN.
    ENDTRY.

    " Success response
    DATA(lv_key) = condense( val = CONV string( li_repo->get_key( ) ) ).
    rv_result = |\{"success":"X","repo_key":"{ lv_key }","message":"Repository deleted successfully"\}|.
  ENDMETHOD.

ENDCLASS.
