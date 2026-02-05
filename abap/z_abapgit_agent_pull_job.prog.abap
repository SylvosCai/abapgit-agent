"&lt;report Z_ABAPGIT_AGENT_PULL_JOB
"&gt; Purpose: Background job to pull git and activate objects
"&gt; Created by: ABAP AI Bridge
"&gt; Date: 2026-02-05

REPORT z_abapgit_agent_pull_job.
TABLES: sscrfields.

PARAMETERS: pv_url TYPE string LOWER CASE.
PARAMETERS: pv_branch TYPE string DEFAULT 'main'.
PARAMETERS: pv_job_id TYPE string.

INITIALIZATION.
  " Job started
DATA(lv_started) = |{ sy-datum }{ sy-uzeit }|.

START-OF-SELECTION.
  DATA: lo_repo TYPE REF TO zcl_abapgit_repo.
  DATA: lv_message TYPE string.
  DATA: lt_objects TYPE zif_abapgit_agent=>ty_object_table.
  DATA: lt_error_log TYPE string_table.
  DATA: lv_success TYPE abap_bool.
  DATA: lv_activated TYPE i.
  DATA: lv_failed TYPE i.

  " Initialize result record
  DATA(ls_result) = VALUE z_abapgit_agent_res(
    job_id = pv_job_id
    status = 'RUNNING'
    started_at = |{ sy-datum }{ sy-uzeit }|
  ).

  MODIFY z_abapgit_agent_res FROM ls_result.

  TRY.
      " Find repository by URL
      lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get_by_url( pv_url ).

      " Log: Repository found
      MODIFY z_abapgit_agent_log FROM VALUE #( job_id = pv_job_id timestamp = |{ sy-datum }{ sy-uzeit }| type = 'INFO' message = |Repository found: { pv_url }| ).

      " Pull changes
      lo_repo->refresh( ).
      lo_repo->pull(
        iv_branch = pv_branch
        iv_strategy = zif_abapgit_definitions=>c_git_strategy-branch
      ).

      " Get changed objects
      DATA(lt_objects_raw) = lo_repo->get_objects_serialized( ).

      " Extract object names
      LOOP AT lt_objects_raw ASSIGNING FIELD-SYMBOL(<ls_obj>).
        APPEND VALUE #( object_type = <ls_obj>-obj_type
                        object_name = <ls_obj>-obj_name
                        devclass = <ls_obj>-devclass ) TO lt_objects.
      ENDLOOP.

      " Log: Objects to activate
      MODIFY z_abapgit_agent_log FROM VALUE #( job_id = pv_job_id timestamp = |{ sy-datum }{ sy-uzeit }| type = 'INFO' message = |{ lines( lt_objects ) } objects to activate| ).

      " Activate objects
      IF lt_objects IS NOT INITIAL.
        lv_success = activate_objects(
          EXPORTING it_objects = lt_objects
          IMPORTING ev_activated = lv_activated
                    ev_failed = lv_failed
                    et_error_log = lt_error_log ).
      ELSE.
        lv_success = abap_true.
        lv_activated = 0.
        lv_failed = 0.
      ENDIF.

      " Update result
      ls_result-success = lv_success.
      ls_result-message = COND #( WHEN lv_success = abap_true THEN 'Activation completed' ELSE 'Activation failed' ).
      ls_result-activated_count = lv_activated.
      ls_result-failed_count = lv_failed.
      ls_result-finished_at = |{ sy-datum }{ sy-uzeit }|.
      ls_result-status = COND #( WHEN lv_success = abap_true THEN 'COMPLETED' ELSE 'FAILED' ).

    MODIFY z_abapgit_agent_res FROM ls_result.

    " Log result
    MODIFY z_abapgit_agent_log FROM VALUE #( job_id = pv_job_id timestamp = |{ sy-datum }{ sy-uzeit }| type = COND #( WHEN lv_success = abap_true THEN 'SUCCESS' ELSE 'ERROR' ) message = ls_result-message ).

    " Log errors
    LOOP AT lt_error_log ASSIGNING FIELD-SYMBOL(<lv_error>).
      MODIFY z_abapgit_agent_err FROM VALUE #( job_id = pv_job_id message = <lv_error> ).
    ENDLOOP.

  CATCH cx_root INTO DATA(lx_exception).
    " Log exception
    MODIFY z_abapgit_agent_log FROM VALUE #( job_id = pv_job_id timestamp = |{ sy-datum }{ sy-uzeit }| type = 'ERROR' message = lx_exception->get_text( ) ).

    ls_result-success = abap_false.
    ls_result-message = lx_exception->get_text( ).
    ls_result-status = 'FAILED'.

    MODIFY z_abapgit_agent_res FROM ls_result.
ENDTRY.

"&amp;---------------------------------------------------------------------
"&amp;      FORM activate_objects
"&amp;---------------------------------------------------------------------
FORM activate_objects
      IMPORTING it_objects TYPE zif_abapgit_agent=>ty_object_table
      EXPORTING ev_activated TYPE i
                ev_failed TYPE i
                et_error_log TYPE string_table.

  DATA: lv_obj TYPE string.
  DATA: lt_ddic TYPE TABLE OF rsddevobj.
  DATA: ls_ddic TYPE rsddevobj.
  DATA: lt_src TYPE TABLE OF string.

  CLEAR: ev_activated, ev_failed, et_error_log.

  LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<ls_obj>).
    lv_obj = |<ls_obj>-object_type> <ls_obj>-object_name>|.

    TRY.
        CASE <ls_obj>-object_type.
          WHEN 'PROG' OR 'CLAS' OR 'FUGR' OR 'INTF' OR 'TYPE'.
            " Source code object - use INCLUDE
            CALL FUNCTION 'RS_INCL_OBJ'
              EXPORTING
                object   = <ls_obj>-object_type
                obj_name = <ls_obj>-object_name
                devclass = <ls_obj>-devclass
              TABLES
                source   = lt_src
              EXCEPTIONS
                not_done = 1
                OTHERS   = 2.

            IF sy-subrc = 0.
              ev_activated = ev_activated + 1.
            ELSE.
              ev_failed = ev_failed + 1.
              APPEND |Failed to activate { <ls_obj>-object_type } { <ls_obj>-object_name }| TO et_error_log.
            ENDIF.

          WHEN 'DOMA' OR 'DTEL' OR 'TABL' OR 'VIEW' OR 'MARG' OR 'STRU'.
            " DDIC object
            CALL FUNCTION 'RS_DD_DOMACT'
              EXPORTING
                name          = <ls_obj>-object_name
                obj_type      = <ls_obj>-object_type
              EXCEPTIONS
                illegal_input = 1
                not_activation = 2
                OTHERS         = 3.

            IF sy-subrc = 0.
              ev_activated = ev_activated + 1.
            ELSE.
              ev_failed = ev_failed + 1.
              APPEND |Failed to activate { <ls_obj>-object_type } { <ls_obj>-object_name }| TO et_error_log.
            ENDIF.

          WHEN OTHERS.
            " Other objects - try generic activation
            CALL FUNCTION 'SCT_LIBRARY_ACTIVATE_OBJECT'
              EXPORTING
                obj_type      = <ls_obj>-object_type
                obj_name      = <ls_obj>-object_name
              IMPORTING
                act_state     = DATA(lv_state)
              EXCEPTIONS
                illegal_input = 1
                not_found     = 2
                OTHERS        = 3.

            IF sy-subrc = 0 AND lv_state = '1'.  " Active
              ev_activated = ev_activated + 1.
            ELSE.
              ev_failed = ev_failed + 1.
              APPEND |Failed to activate { <ls_obj>-object_type } { <ls_obj>-object_name }| TO et_error_log.
            ENDIF.
        ENDCASE.

      CATCH cx_root INTO DATA(lx_error).
        ev_failed = ev_failed + 1.
        APPEND |Exception for { <ls_obj>-object_type } { <ls_obj>-object_name }: { lx_error->get_text( ) }| TO et_error_log.
    ENDTRY.
  ENDLOOP.

ENDFORM.
