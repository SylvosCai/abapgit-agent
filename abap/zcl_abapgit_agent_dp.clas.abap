"&lt;class ZCL_ABAPGIT_AGENT_DP
"&gt; Purpose: OData Data Provider for ABAP Git Agent
"&gt; Created by: ABAP AI Bridge
"&gt; Date: 2026-02-05

CLASS zcl_abapgit_agent_dp DEFINITION
  PUBLIC
  INHERITING FROM cl_sadl_gtx_standard_dpc
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      pullcommandset_create
        REDEFINITION,
      jobstatusset_get_entity
        REDEFINITION,
      jobstatusset_get_entityset
        REDEFINITION,
      logentryset_get_entityset
        REDEFINITION,
      healthcheckset_get_entity
        REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      pullcommandset_update
        REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      create_job_id
        RETURNING VALUE(rv_job_id) TYPE string.
ENDCLASS.

CLASS zcl_abapgit_agent_dp IMPLEMENTATION.

  METHOD pullcommandset_create.
    " Create pull job
    DATA(lv_job_id) = create_job_id( ).

    " Read request data
    DATA(ls_request) = CORRESPONDING zif_abapgit_agent=>ty_pull_request( it_key_tab ).

    " Start background job
    DATA(lv_jobcount) = start_pull_job(
      iv_url    = ls_request-Url
      iv-B_branch = ls_requestranch
      iv_job_id = lv_job_id
    ).

    " Return job info
    er_entity = VALUE #(
      JobId      = lv_job_id
      Status     = zif_abapgit_agent=>gc_status_running
      Message    = |Pull job started: { lv_jobcount }|
      StartedAt  = |{ sy-datum }{ sy-uzeit }|
    ).
  ENDMETHOD.

  METHOD jobstatusset_get_entity.
    " Get job status from database
    SELECT SINGLE * FROM z_abapgit_agent_res
      WHERE job_id = @iv_key_jobid
      INTO @DATA(ls_result).

    IF sy-subrc = 0.
      er_entity = VALUE #(
        JobId          = ls_result-job_id
        Status         = ls_result-status
        Success        = ls_result-success
        Message        = ls_result-message
        ActivatedCount = ls_result-activated_count
        FailedCount    = ls_result-failed_count
        StartedAt      = ls_result-started_at
        FinishedAt     = ls_result-finished_at
      ).
    ELSE.
      mo_response->set_status_code( '404' ).
      RAISE EXCEPTION TYPE cx_sadl_entity_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD jobstatusset_get_entityset.
    " Get all jobs or filter by status
    DATA(lv_status) = VALUE #( it_filter_select_options[ property = 'Status' ]-select_options[ 1 ]-low OPTIONAL ).

    SELECT * FROM z_abapgit_agent_res
      INTO TABLE @DATA(lt_results)
      WHERE status = @lv_status
      ORDER BY started_at DESC.

    LOOP AT lt_results ASSIGNING FIELD-SYMBOL(<ls>).
      APPEND VALUE #(
        JobId          = <ls>-job_id
        Status         = <ls>-status
        Success        = <ls>-success
        Message        = <ls>-message
        ActivatedCount = <ls>-activated_count
        FailedCount    = <ls>-failed_count
        StartedAt      = <ls>-started_at
        FinishedAt     = <ls>-finished_at
      ) TO et_entityset.
    ENDLOOP.
  ENDMETHOD.

  METHOD logentryset_get_entityset.
    " Get log entries for job
    DATA(lv_job_id) = VALUE #( it_filter_select_options[ property = 'JobId' ]-select_options[ 1 ]-low ).

    SELECT * FROM z_abapgit_agent_log
      WHERE job_id = @lv_job_id
      ORDER BY timestamp
      INTO TABLE @DATA(lt_logs).

    LOOP AT lt_logs ASSIGNING FIELD-SYMBOL(<ls>).
      APPEND VALUE #(
        JobId      = <ls>-job_id
        Timestamp  = <ls>-timestamp
        Type       = <ls>-type
        Message    = <ls>-message
        ObjectType = <ls>-object_type
        ObjectName = <ls>-object_name
      ) TO et_entityset.
    ENDLOOP.
  ENDMETHOD.

  METHOD healthcheckset_get_entity.
    " Simple health check
    er_entity = VALUE #(
      Status = 'OK'
      Version = '1.0'
      Timestamp = |{ sy-datum }{ sy-uzeit }|
    ).
  ENDMETHOD.

  METHOD pullcommandset_update.
    " Not supported
    RAISE EXCEPTION TYPE cx_sadl_not_implemented.
  ENDMETHOD.

  METHOD create_job_id.
    rv_job_id = |{ sy-uname }_{ sy-datetime }_{ sy-uzeit }|.
  ENDMETHOD.

  METHOD start_pull_job.
    DATA: lv_jobname TYPE btcjob,
          lv_jobcount TYPE btcjobcount.

    lv_jobname = |ABAPGIT_PULL_{ iv_job_id }|.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname = lv_jobname
      IMPORTING
        jobcount = lv_jobcount.

    SUBMIT z_abapgit_agent_pull_job VIA JOB lv_jobcount
      WITH pv_url = iv_url
      WITH pv_branch = iv_branch
      WITH pv_job_id = iv_job_id
      AND RETURN.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount  = lv_jobcount
        jobname   = lv_jobname
        sdlstrtdt = sy-datum
        sdlstrttm = sy-uzeit.

  ENDMETHOD.

ENDCLASS.
