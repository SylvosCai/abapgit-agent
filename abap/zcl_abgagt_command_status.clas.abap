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

    " Look up the repository directly — no need to instantiate ZCL_ABGAGT_AGENT
    DATA li_repo TYPE REF TO zif_abapgit_repo.
    TRY.
        zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
          EXPORTING iv_url = ls_params-url
          IMPORTING ei_repo = li_repo ).
      CATCH zcx_abapgit_exception.
        " li_repo stays unbound on error
    ENDTRY.

    IF li_repo IS NOT BOUND.
      rv_result = '{"success":true,"url":"' && ls_params-url && '","status":"Not found"}'.
      RETURN.
    ENDIF.

    " Repo found — collect details
    TRY.
        DATA(lv_repo_key) = li_repo->get_key( ).
        DATA(lv_package)  = li_repo->get_package( ).

        " Check if package requires transport (TDEVC-KORRFLAG = 'X')
        DATA(lv_transport_required) = abap_false.
        IF lv_package IS NOT INITIAL.
          SELECT SINGLE korrflag FROM tdevc
            WHERE devclass = @lv_package
            INTO @DATA(lv_korrflag).
          IF sy-subrc = 0 AND lv_korrflag = abap_true.
            lv_transport_required = abap_true.
          ENDIF.
        ENDIF.

        DATA(lv_tr_str) = COND string(
          WHEN lv_transport_required = abap_true THEN 'true'
          ELSE 'false' ).

        rv_result = '{"success":true,"url":"' && ls_params-url && '","status":"Found",' &&
                    '"repo_key":"' && lv_repo_key && '","package":"' && lv_package && '",' &&
                    '"transport_required":' && lv_tr_str && '}'.
      CATCH zcx_abapgit_exception.
        rv_result = '{"success":true,"url":"' && ls_params-url && '","status":"Found"}'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
