"&lt;interface ZIF_ABAPGIT_AGENT_ODATA
"&gt; Purpose: OData types for ABAP Git Agent
"&gt; Created by: ABAP AI Bridge
"&gt; Date: 2026-02-05

INTERFACE zif_abapgit_agent_odata
  PUBLIC .

  TYPES:
    BEGIN OF ty_pull_request,
      Url    TYPE string,
      Branch TYPE string,
    END OF ty_pull_request,

    BEGIN OF ty_pull_response,
      JobId      TYPE string,
      Status     TYPE string,
      Message    TYPE string,
      StartedAt  TYPE timestampl,
    END OF ty_pull_response,

    BEGIN OF ty_job_response,
      JobId          TYPE string,
      Status         TYPE string,
      Success        TYPE abap_bool,
      Message        TYPE string,
      ActivatedCount TYPE i,
      FailedCount    TYPE i,
      StartedAt      TYPE timestampl,
      FinishedAt     TYPE timestampl,
    END OF ty_job_response,

    BEGIN OF ty_log_response,
      JobId      TYPE string,
      Timestamp  TYPE timestampl,
      Type       TYPE string,
      Message    TYPE string,
      ObjectType TYPE trobjtype,
      ObjectName TYPE sobj_name,
    END OF ty_log_response,

    BEGIN OF ty_health_response,
      Status    TYPE string,
      Version   TYPE string,
      Timestamp TYPE timestampl,
    END OF ty_health_response.

ENDINTERFACE.
