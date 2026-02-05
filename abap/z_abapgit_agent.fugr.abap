"&lt;report Z_ABAPGIT_AGENT_PULL
"&gt; Purpose: Function group for ABAP Git Agent RFC functions
"&gt; Created by: ABAP AI Bridge
"&gt; Date: 2026-02-05

FUNCTION-POOL Z_ABAPGIT_AGENT.             " Global Function-Pool

" Global data for function group
DATA: gv_job_id TYPE string.
DATA: gt_job_log TYPE zif_abapgit_agent=>ty_log_table.
DATA: gs_job_result TYPE zif_abapgit_agent=>ty_result.
