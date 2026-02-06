FUNCTION zabapgagent_activate.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"    VALUE(IV_URL) TYPE STRING OPTIONAL
*"    VALUE(IV_PACKAGE) TYPE STRING OPTIONAL
*"  EXPORTING
*"    VALUE(EV_SUCCESS) TYPE CHAR1
*"    VALUE(EV_MESSAGE) TYPE STRING
*"----------------------------------------------------------------------
  DATA: lv_success TYPE char1.
  DATA: lv_message TYPE string.
  DATA: lv_devclass TYPE devclass.
  DATA: li_repo TYPE REF TO zif_abapgit_repo.
  DATA: lv_reason TYPE string.
  DATA: li_repo_tmp TYPE REF TO zif_abapgit_repo.
  DATA: lt_objects TYPE STANDARD TABLE OF tadir.
  DATA: ls_object TYPE tadir.
  DATA: lv_count TYPE i.
  DATA: lv_act TYPE i.
  DATA: lo_settings TYPE REF TO zcl_abapgit_settings.
  DATA: lv_activation_setting TYPE zif_abapgit_persist_user=>ty_s_user_settings-activate_wo_popup.

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
    TRY.
        zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url(
          EXPORTING
            iv_url    = iv_url
          IMPORTING
            ei_repo   = li_repo
            ev_reason = lv_reason ).
      CATCH zcx_abapgit_exception.
        li_repo = VALUE #( ).
    ENDTRY.

    IF li_repo IS BOUND.
      lv_devclass = li_repo->get_package( ).
    ENDIF.
  ELSEIF iv_package IS NOT INITIAL.
    lv_devclass = iv_package.
    WRITE: / 'Finding repository by package...'.

    " Get all repos and find the one with matching package
    DATA(lt_all_repos) = zcl_abapgit_repo_srv=>get_instance( )->get_repo_list( ).

    LOOP AT lt_all_repos INTO li_repo_tmp.
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
  ULINE.

  " Refresh repo
  TRY.
      li_repo->refresh( ).
    CATCH zcx_abapgit_exception.
      WRITE: / 'Warning: Could not refresh repo'.
  ENDTRY.

  " Get objects from TADIR for this package
  TRY.
      SELECT * FROM tadir
        INTO TABLE lt_objects
        WHERE devclass = lv_devclass
        AND object NOT IN ('DEVC', 'PACK', 'TABL', 'VIEW', 'DOMA', 'DTEL', 'TTYP').
    CATCH zcx_abapgit_exception.
      ev_success = ' '.
      ev_message = 'Error reading objects from TADIR'.
      WRITE: / 'ERROR:', ev_message.
      RETURN.
  ENDTRY.

  lv_count = lines( lt_objects ).

  IF lv_count = 0.
    ev_success = 'X'.
    ev_message = 'No objects to activate'.
    WRITE: / 'No objects to activate.'.
    RETURN.
  ENDIF.

  WRITE: / 'Found', lv_count, 'objects to activate.'.

  " Enable activate without popup
  TRY.
      lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
      lv_activation_setting = lo_settings->get_activate_wo_popup( ).
      lo_settings->set_activate_wo_popup( abap_true ).
    CATCH zcx_abapgit_exception.
      WRITE: / 'Warning: Could not change activation setting'.
  ENDTRY.

  " Activate objects
  WRITE: / 'Activating objects...'.
  lv_act = 0.

  LOOP AT lt_objects INTO ls_object.
    WRITE: / 'Activating:', ls_object-object, ls_object-obj_name.

    CALL FUNCTION 'RS_OBJECT_ACTIVE'
      EXPORTING
        objecttype = ls_object-object
        objectname = ls_object-obj_name
        devclass   = ls_object-devclass.
    lv_act = lv_act + 1.
  ENDLOOP.

  " Restore setting
  TRY.
      lo_settings->set_activate_wo_popup( lv_activation_setting ).
    CATCH zcx_abapgit_exception.
      WRITE: / 'Warning: Could not restore activation setting'.
  ENDTRY.

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
