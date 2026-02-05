# ABAP Git Bridge - Implementation Plan

## Overview
Create an automated workflow where Claude generates ABAP code, pushes to git, and a local agent pulls/activates the code in the ABAP system with proper error handling.

## Architecture

```
┌─────────────┐     ┌─────────────────┐     ┌────────────────┐
│   Claude     │────▶│  Local Agent    │────▶│   ABAP System  │
│  (VS Code)   │     │  (Node.js)      │     │  (abapGit)     │
└─────────────┘     └─────────────────┘     └────────────────┘
                            │                        │
                            │                        ▼
                     ┌─────────────┐         ┌────────────────┐
                     │ Result Cache│◀───────│ Activation Log │
                     └─────────────┘         └────────────────┘
```

## Components

### 1. ABAP System Components

#### 1.1 Main Class: ZCL_ABAPGit_AGENT
```abap
CLASS zcl_abapgit_agent DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      pull_repo
        IMPORTING iv_url TYPE string
                  iv_branch TYPE string DEFAULT 'main'
        RETURNING VALUE(rs_result) TYPE zif_abapgit_agent=>ty_result,
      activate_objects
        IMPORTING it_objects TYPE zif_abapgit_agent=>ty_object_table
        RETURNING VALUE(rs_result) TYPE zif_abapgit_agent=>ty_result,
      get_status
        IMPORTING iv_repo_url TYPE string
        RETURNING VALUE(rs_status) TYPE zif_abapgit_agent=>ty_repo_status,
      get_activation_log
        IMPORTING iv_job_id TYPE string
        RETURNING VALUE(rt_log) TYPE zif_abapgit_agent=>ty_log_table.
ENDCLASS.
```

#### 1.2 Interface: ZIF_ABAPGIT_AGENT
```abap
INTERFACE zif_abapgit_agent
  PUBLIC .

  TYPES:
    BEGIN OF ty_result,
      success      TYPE abap_bool,
      job_id       TYPE string,
      message      TYPE string,
      error_log    TYPE string_table,
      activated_count TYPE i,
      failed_count   TYPE i,
    END OF ty_result,

    BEGIN OF ty_repo_status,
      url          TYPE string,
      branch       TYPE string,
      commit_sha   TYPE string,
      last_pull    TYPE timestampl,
      is_active    TYPE abap_bool,
    END OF ty_repo_status,

    ty_object_table TYPE TABLE OF string,

    BEGIN OF ty_log_entry,
      timestamp   TYPE timestampl,
      type        TYPE string,
      message     TYPE string,
      object      TYPE string,
    END OF ty_log_entry,

    ty_log_table TYPE TABLE OF ty_log_entry.

ENDINTERFACE.
```

#### 1.3 RFC-Enabled Function Modules
- `Z_ABAPGIT_AGENT_PULL` - Trigger git pull
- `Z_ABAPGIT_AGENT_ACTIVATE` - Activate objects
- `Z_ABAPGIT_AGENT_GET_LOG` - Get activation log
- `Z_ABAPGIT_AGENT_CHECK_STATUS` - Check repo status

#### 1.4 Background Job Class
```abap
CLASS zcl_abapgit_job DEFINITION
  PUBLIC.
  METHODS:
    constructor
      IMPORTING iv_job_id TYPE string.
    METHODS run
      IMPORTING iv_repo_url TYPE string
                iv_branch TYPE string.
    METHODS get_log
      RETURNING VALUE(rt_log) TYPE zif_abapgit_agent=>ty_log_table.
ENDCLASS.
```

### 2. Local Agent (Node.js)

#### 2.1 Project Structure
```
abap-ai-bridge/
├── package.json
├── src/
│   ├── agent.js          # Main agent class
│   ├── abap-client.js    # ABAP connection (RFC/OData)
│   ├── server.js         # HTTP server for Claude
│   ├── config.js         # Configuration
│   └── logger.js         # Logging
├── abap/
│   ├── zcl_abapgit_agent.prog.abap
│   ├── zif_abapgit_agent.intf.abap
│   └── z_abapgit_agent_pull.fugr.abap
└── README.md
```

#### 2.2 Agent API Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | /api/pull | Pull and activate repo |
| POST | /api/activate | Activate specific objects |
| GET | /api/status/:repoUrl | Get repo status |
| GET | /api/log/:jobId | Get activation log |
| GET | /api/health | Health check |

### 3. Implementation Steps

#### Phase 1: ABAP Backend
- [ ] Create interface `ZIF_ABAPGIT_AGENT`
- [ ] Create class `ZCL_ABAPGIT_AGENT`
- [ ] Create function modules (RFC-enabled)
- [ ] Create background job logic
- [ ] Create activation log table

#### Phase 2: Local Agent
- [ ] Initialize Node.js project
- [ ] Create ABAP client (using `node-rfc` or OData)
- [ ] Create HTTP server
- [ ] Implement retry logic
- [ ] Add result caching

#### Phase 3: Integration
- [ ] Claude integration script
- [ ] Error handling workflow
- [ ] Test with real ABAP system

### 4. Communication Flow

```
1. Claude pushes code to git
2. Claude calls: POST /api/pull { url: "...", branch: "main" }
3. Local Agent:
   a. Calls RFC function Z_ABAPGIT_AGENT_PULL
   b. Starts async job in ABAP
   c. Polls for job completion every 5 seconds
   d. Returns job_id immediately
4. Claude polls: GET /api/log/:jobId
5. When job completes, returns full log with errors if any
6. If errors, Claude fixes and repeats
```

### 5. ABAP Code Examples

#### 5.1 Pull and Activate Main Logic
```abap
METHOD pull_and_activate.

  DATA: lo_repo TYPE REF TO zcl_abapgit_repo,
        lv_url  TYPE string,
        ls_log  TYPE zif_abapgit_agent=>ty_log_entry.

  " 1. Find repo by URL
  lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get_by_url( iv_url ).

  " 2. Pull changes
  lo_repo->refresh( ).
  lo_repo->pull( iv_branch = iv_branch ).

  " 3. Get changed objects
  DATA(lt_strict) = lo_repo->get_objects_serialized( ).
  DATA(lt_objects) = extract_object_names( lt_strict ).

  " 4. Activate in background
  DATA(lv_job_id) = create_background_job( it_objects = lt_objects ).

  " 5. Return result
  rs_result-job_id = lv_job_id.
  rs_result-success = abap_true.
ENDMETHOD.
```

#### 5.2 Background Activation
```abap
METHOD create_background_job.

  DATA: lv_jobname TYPE btcjob,
        lv_jobcount TYPE btcjobcount.

  lv_jobname = |ABAPGIT_AGENT_{ sy-uname }|_{ sy-datetime }|.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname = lv_jobname
    IMPORTING
      jobcount = lv_jobcount.

  SUBMIT z_abapgit_activate_objects VIA JOB lv_jobcount
    WITH pv_job_id = lv_jobcount
    WITH pt_objects = it_objects
    AND RETURN.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount  = lv_jobcount
      jobname   = lv_jobname
      sdlstrtdt = sy-datum
      sdlstrttm = sy-uzeit.
ENDMETHOD.
```

### 6. Configuration

#### ABAP System
- RFC destination: `ABAPGIT_AGENT`
- User with developer rights
- Access to abapGit repositories

#### Local Agent (config.json)
```json
{
  "abap": {
    "ashost": "your.sap-system.com",
    "sysnr": "00",
    "client": "100",
    "user": "TECH_USER",
    "password": "secret",
    "language": "EN"
  },
  "agent": {
    "port": 3000,
    "pollInterval": 5000,
    "maxRetries": 3
  }
}
```

### 7. Error Handling

| Error Type | Handling |
|------------|----------|
| Syntax Error | Parse error log, return specific object |
| Activation Error | Log with object name, suggest fix |
| Network Timeout | Retry with backoff |
| Git Not Found | Clear error message |

### 8. Testing

- [ ] Pull existing repo without changes
- [ ] Pull with syntax errors
- [ ] Pull with successful activation
- [ ] Concurrent requests
- [ ] Large object activation

### 9. Security Considerations

- Use technical user (not personal credentials)
- Network encryption (HTTPS/SNC)
- Input validation
- Audit logging

### 10. Future Enhancements

- WebSocket for real-time updates
- OAuth authentication
- Multi-system support
- Repository management UI
