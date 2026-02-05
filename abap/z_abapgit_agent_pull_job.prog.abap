REPORT z_abapgit_agent_pull_job.
PARAMETERS: pv_url TYPE string LOWER CASE.
PARAMETERS: pv_branch TYPE string DEFAULT 'main'.
PARAMETERS: pv_job_id TYPE string.

START-OF-SELECTION.
  DATA: lo_repo TYPE REF TO zcl_abapgit_repo.
  DATA: lt_objects TYPE zif_abapgit_agent=>ty_object_table.
  DATA: lt_error_log TYPE string_table.
  DATA: lv_success TYPE abap_bool.
  DATA: lv_activated TYPE i.
  DATA: lv_failed TYPE i.

  DATA(ls_result) = VALUE z_abapg_res(
    job_id = pv_job_id
    status = 'RUNNING'
    started_at = |{ sy-datum }{ sy-uzeit }|
  ).
  MODIFY z_abapg_res FROM ls_result.

  TRY.
      lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get_by_url( pv_url ).
      MODIFY z_abapg_log FROM VALUE #( job_id = pv_job_id timestamp = |{ sy-datum }{ sy-uzeit }| type = 'INFO' message = |Repository found: { pv_url }| ).
      lo_repo->refresh( ).
      lo_repo->pull( iv_branch = pv_branch iv_strategy = zif_abapgit_definitions=>c_git_strategy-branch ).
      DATA(lt_objects_raw) = lo_repo->get_objects_serialized( ).
      LOOP AT lt_objects_raw ASSIGNING FIELD-SYMBOL(<ls_obj>).
        APPEND VALUE #( object_type = <ls_obj>-obj_type object_name = <ls_obj>-obj_name devclass = <ls_obj>-devclass ) TO lt_objects.
      ENDLOOP.
      MODIFY z_abapg_log FROM VALUE #( job_id = pv_job_id timestamp = |{ sy-datum }{ sy-uzeit }| type = 'INFO' message = |{ lines( lt_objects ) } objects to activate| ).
      IF lt_objects IS NOT INITIAL.
        PERFORM activate_objects TABLES lt_objects lt_error_log USING lv_activated lv_failed.
      ELSE.
        lv_success = abap_true.
        lv_activated = 0.
        lv_failed = 0.
      ENDIF.
      ls_result-success = lv_success.
      ls_result-message = COND #( WHEN lv_success = abap_true THEN 'Activation completed' ELSE 'Activation failed' ).
      ls_result-activated_count = lv_activated.
      ls_result-failed_count = lv_failed.
      ls_result-finished_at = |{ sy-datum }{ sy-uzeit }|.
      ls_result-status = COND #( WHEN lv_success = abap_true THEN 'COMPLETED' ELSE 'FAILED' ).
    MODIFY z_abapg_res FROM ls_result.
    MODIFY z_abapg_log FROM VALUE #( job_id = pv_job_id timestamp = |{ sy-datum }{ sy-uzeit }| type = COND #( WHEN lv_success = abap_true THEN 'SUCCESS' ELSE 'ERROR' ) message = ls_result-message ).
    LOOP AT lt_error_log ASSIGNING FIELD-SYMBOL(<lv_error>).
      MODIFY z_abapg_err FROM VALUE #( job_id = pv_job_id message = <lv_error> ).
    ENDLOOP.
  CATCH cx_root INTO DATA(lx_exception).
    MODIFY z_abapg_log FROM VALUE #( job_id = pv_job_id timestamp = |{ sy-datum }{ sy-uzeit }| type = 'ERROR' message = lx_exception->get_text( ) ).
    ls_result-success = abap_false.
    ls_result-message = lx_exception->get_text( ).
    ls_result-status = 'FAILED'.
    MODIFY z_abapg_res FROM ls_result.
ENDTRY.

FORM activate_objects TABLES it_objects TYPE zif_abapgit_agent=>ty_object_table pt_error_log TYPE string_table USING ev_activated TYPE i ev_failed TYPE i.
  DATA: lt_src TYPE TABLE OF string.
  CLEAR: ev_activated, ev_failed, pt_error_log[].
  LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
    TRY.
        CASE <ls_obj>-object_type.
          WHEN 'PROG' OR 'CLAS' OR 'FUGR' OR 'INTF'.
            CALL FUNCTION 'RS_INCL_OBJ' EXPORTING object = <ls_obj>-object_type obj_name = <ls_obj>-object_name devclass = <ls_obj>-devclass TABLES source = lt_src EXCEPTIONS not_done = 1 OTHERS = 2.
            IF sy-subrc = 0. ev_activated = ev_activated + 1. ELSE. ev_failed = ev_failed + 1. APPEND |Failed to activate { <ls_obj>-object_type } { <ls_obj>-object_name }| TO pt_error_log. ENDIF.
          WHEN 'DOMA' OR 'DTEL' OR 'TABL'.
            CALL FUNCTION 'RS_DD_DOMACT' EXPORTING name = <ls_obj>-object_name obj_type = <ls_obj>-object_type EXCEPTIONS illegal_input = 1 not_activation = 2 OTHERS = 3.
            IF sy-subrc = 0. ev_activated = ev_activated + 1. ELSE. ev_failed = ev_failed + 1. APPEND |Failed to activate { <ls_obj>-object_type } { <ls_obj>-object_name }| TO pt_error_log. ENDIF.
          WHEN OTHERS.
            ev_failed = ev_failed + 1.
            APPEND |Unknown object type: { <ls_obj>-object_type } { <ls_obj>-object_name }| TO pt_error_log.
        ENDCASE.
      CATCH cx_root INTO DATA(lx_error).
        ev_failed = ev_failed + 1.
        APPEND |Exception: { lx_error->get_text( ) } | TO pt_error_log.
    ENDTRY.
  ENDLOOP.
ENDFORM.
