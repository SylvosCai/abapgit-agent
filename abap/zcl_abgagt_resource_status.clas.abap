*"*"use source
*"*"Local Interface:
*"**********************************************************************
CLASS zcl_abgagt_resource_status DEFINITION PUBLIC FINAL
                             INHERITING FROM cl_rest_resource
                             CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~post REDEFINITION.

ENDCLASS.

CLASS zcl_abgagt_resource_status IMPLEMENTATION.

  METHOD if_rest_resource~post.
    DATA lv_json TYPE string.

    " Parse request body
    DATA(lo_entity) = mo_request->get_entity( ).
    DATA(lv_body) = lo_entity->get_string_data( ).

    " Parse JSON to get URL
    DATA lv_url TYPE string.
    IF lv_body IS NOT INITIAL.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json = lv_body
        CHANGING
          data = VALUE #( url = lv_url ) ).
    ENDIF.

    " If no URL in body, try to get from query parameter
    DATA lv_url_param TYPE string.
    lv_url_param = mo_request->get_uri_parameter( 'url' ).
    IF lv_url_param IS NOT INITIAL.
      lv_url = lv_url_param.
    ENDIF.

    " Check repo status
    DATA lo_agent TYPE REF TO zcl_abgagt_agent.
    CREATE OBJECT lo_agent.

    DATA(lv_status) = lo_agent->get_repo_status( lv_url ).

    " Build response
    IF lv_status = 'Found'.
      " Get repo details
      DATA li_repo TYPE REF TO zif_abapgit_repo.
      TRY.
          zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
            EXPORTING iv_url = lv_url
            IMPORTING ei_repo = li_repo ).

          DATA(lv_repo_key) = li_repo->get_key( ).
          DATA(lv_package) = li_repo->get_package( ).

          lv_json = '{"success":true,"url":"' && lv_url && '","status":"Found",'.
          lv_json = lv_json && '"repo_key":"' && lv_repo_key && '","package":"' && lv_package && '"}'.
        CATCH zcx_abapgit_exception.
          lv_json = '{"success":true,"url":"' && lv_url && '","status":"Found"}'.
      ENDTRY.
    ELSE.
      lv_json = '{"success":true,"url":"' && lv_url && '","status":"Not found"}'.
    ENDIF.

    " Set response
    DATA(lo_response_entity) = mo_response->create_entity( ).
    lo_response_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_response_entity->set_string_data( lv_json ).
    mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
  ENDMETHOD.

ENDCLASS.
