FUNCTION zabapgagent_activate.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_URL) TYPE STRING OPTIONAL
*"    VALUE(IV_PACKAGE) TYPE STRING OPTIONAL
*"  EXPORTING
*"    VALUE(EV_SUCCESS) TYPE CHAR1
*"    VALUE(EV_MESSAGE) TYPE STRING
*"  RAISING
*"    ERROR
*"    ZCX_ABAPGIT_EXCEPTION
*"----------------------------------------------------------------------
  DATA: lv_success TYPE char1.
  DATA: lv_message TYPE string.
  DATA: li_repo TYPE REF TO zif_abapgit_repo.
  DATA: lv_reason TYPE string.
  DATA: lt_tadir TYPE STANDARD TABLE OF tadir.
  DATA: ls_tadir TYPE tadir.
  DATA: ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
  DATA: lv_count TYPE i.
  DATA: lv_act TYPE i.
  DATA: lv_devclass TYPE devclass.

  ev_success = ' '.
  ev_message = 'Starting activation...'.

  WRITE: / 'Activation started.'.
  IF iv_url IS NOT INITIAL.
    WRITE: / 'URL:', iv_url.
  ENDIF.
  IF iv_package IS NOT INITIAL.
    WRITE: / 'Package:', iv_package.
  ENDIF.
  ULINE.

  " Find repo by URL or package
  IF iv_url IS NOT INITIAL.
    WRITE: / 'Finding repository by URL...'.
    zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
      EXPORTING
        iv_url    = iv_url
      IMPORTING
        ei_repo   = li_repo
        ev_reason = lv_reason ).

    IF li_repo IS BOUND.
      lv_devclass = li_repo->get_package( ).
    ENDIF.
  ELSEIF iv_package IS NOT INITIAL.
    lv_devclass = iv_package.
    WRITE: / 'Finding repository by package...'.
    " Try to find repo by package
    TRY.
        DATA(li_repo_list) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
      CATCH zcx_abapgit_exception.
        li_repo_list = VALUE #( ).
    ENDTRY.

    LOOP AT li_repo_list INTO DATA(li_repo_tmp).
      IF li_repo_tmp->get_package( ) = lv_devclass.
        li_repo = li_repo_tmp.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF li_repo IS NOT BOUND.
    ev_success = ' '.
    ev_message = |Repository not found for package { lv_devclass }|.
    WRITE: / 'ERROR:', ev_message.
    RETURN.
  ENDIF.

  WRITE: / 'Repository found.'.
  WRITE: / 'Package:', li_repo->get_package( ).

  " Get objects to activate
  WRITE: / 'Getting objects to activate...'.

  " Refresh repo to get latest
  li_repo->refresh( ).

  " Get deserialize checks (includes objects)
  ls_checks = li_repo->deserialize_checks( ).

  " Get list of objects
  DATA(lt_objects) = ls_checks-overwrite.
  lv_count = lines( lt_objects ).

  IF lv_count = 0.
    ev_success = 'X'.
    ev_message = 'No objects to activate'.
    WRITE: / 'No objects to activate.'.
    ev_message = |No objects to activate|.
    RETURN.
  ENDIF.

  WRITE: / 'Found', lv_count, 'objects to activate.'.

  " Enable activate without popup
  DATA(lo_settings) = zcl_abapgit_persist_factory=>get_settings( )->read( ).
  DATA(lv_activation_setting) = lo_settings->get_activate_wo_popup( ).
  lo_settings->set_activate_wo_popup( abap_true ).

  " Activate objects
  WRITE: / 'Activating objects...'.
  lv_act = 0.

  LOOP AT lt_objects INTO DATA(ls_object).
    DATA(ls_object_key) = ls_object-obj_type.
    DATA(ls_object_name) = ls_object-obj_name.

    WRITE: / 'Activating:', ls_object_key, ls_object_name.

    TRY.
        zcl_abapgit_objects_activation=>activate(
          iv_object = ls_object_key
          iv_object_name = ls_object_name ).
        lv_act = lv_act + 1.
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        WRITE: / 'ERROR activating:', lx_error->get_text( ).
      CATCH cx_root INTO DATA(lx_root).
        WRITE: / 'ERROR:', lx_root->get_text( ).
    ENDTRY.
  ENDLOOP.

  " Restore setting
  lo_settings->set_activate_wo_popup( lv_activation_setting ).

  ULINE.
  WRITE: / 'Activation completed.'.
  WRITE: / 'Activated:', lv_act, 'of', lv_count, 'objects.'.

  IF lv_act = lv_count.
    ev_success = 'X'.
    ev_message = |Activation completed: { lv_act } of { lv_count } objects|.
  ELSE.
    ev_success = ' '.
    ev_message = |Activation partial: { lv_act } of { lv_count } objects failed|.
  ENDIF.

ENDFUNCTION.
