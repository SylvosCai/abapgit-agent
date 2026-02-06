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

#### 1.1 Main Class: ZCL_ABAPGIT_AGENT (DONE)
```abap
CLASS zcl_abapgit_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES: zif_abapgit_agent.

  PRIVATE SECTION.
    METHODS:
      configure_credentials,
      prepare_deserialize_checks,
      check_inactive_objects,
      handle_exception.
ENDCLASS.
```
**Features:**
- Pull repository and activate objects in one operation
- Check for inactive objects after activation
- Detailed error reporting with exception chaining

#### 1.2 Interface: ZIF_ABAPGIT_AGENT (DONE)
```abap
INTERFACE zif_abapgit_agent PUBLIC.
  TYPES: BEGIN OF ty_result,
    success TYPE abap_bool,
    job_id TYPE string,
    message TYPE string,
    error_detail TYPE string,
    activated_count TYPE i,
    failed_count TYPE i,
    started_at TYPE timestampl,
    finished_at TYPE timestampl,
  END OF ty_result.

  TYPES: BEGIN OF ty_pull_params,
    url TYPE string,
    branch TYPE string,
    username TYPE string,
    password TYPE string,
    package TYPE devclass,
    folder_logic TYPE string,
    create_new TYPE abap_bool,
  END OF ty_pull_params.

  METHODS pull
    IMPORTING iv_url TYPE string
              iv_branch TYPE string DEFAULT 'main'
              iv_username TYPE string OPTIONAL
              iv_password TYPE string OPTIONAL
    RETURNING VALUE(rs_result) TYPE ty_result
    RAISING zcx_abapgit_exception.

  METHODS get_repo_status
    IMPORTING iv_url TYPE string
    RETURNING VALUE(rv_status) TYPE string.
ENDINTERFACE.
```

#### 1.3 REST API Handlers (DONE)
- `ZCL_ABAPGIT_AGENT_HANDLER` - Router for all endpoints
- `ZCL_ABAPGIT_AGENT_PULL` - POST /pull endpoint
- `ZCL_ABAPGIT_AGENT_STATUS` - GET /status endpoint
- `ZCL_ABAPGIT_AGENT_HEALTH` - GET /health endpoint

### 2. Local Agent (Node.js)

#### 2.1 Project Structure (DONE)
```
abap-ai-bridge/
├── package.json
├── src/
│   ├── agent.js          # Main agent class
│   ├── abap-client.js    # REST client for ABAP
│   ├── server.js         # HTTP server for Claude
│   ├── config.js         # Configuration
│   └── logger.js         # Logging
├── abap/
│   ├── zcl_abapgit_agent.clas.abap
│   ├── zif_abapgit_agent.intf.abap
│   ├── zcl_abapgit_agent_*.clas.abap
└── README.md
```

#### 2.2 REST API Endpoints (DONE)

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/pull` | Pull and activate repo |
| GET | `/status?job_id=<id>` | Get job status |
| GET | `/health` | Health check |

## Implementation Status

### Phase 1: ABAP Backend
- [x] Create interface `ZIF_ABAPGIT_AGENT`
- [x] Create class `ZCL_ABAPGIT_AGENT` with OO implementation
- [x] Create REST handlers
- [x] Create function modules
- [x] Create database tables

### Phase 2: Local Agent
- [x] Initialize Node.js project
- [x] Create REST client for ABAP communication
- [x] Create HTTP server
- [x] Add configuration management

### Phase 3: Integration (DONE)

#### Claude Integration Script (DONE)
- [x] Basic CLI structure
- [x] Load configuration from `.abapGitAgent`
- [x] Parse `error_detail` from response
- [x] Display activation results with proper formatting
- [x] Error handling for network failures

#### Node.js Agent Server (DONE)
- [x] HTTP server (`src/server.js`)
- [x] Load configuration from `.abapGitAgent`
- [x] REST client to call ABAP endpoints
- [x] Return formatted response to Claude

#### Testing (DONE)
- [x] Test response parsing logic
- [x] Test URL construction
- [x] Test error detail formatting

## Communication Flow

```
1. Claude pushes code to git
2. Claude calls agent: POST /pull { url: "...", branch: "main", username, password }
3. Agent calls ABAP: POST /sap/bc/z_abapgit_agent/pull
4. ABAP executes pull synchronously
5. ABAP returns: { success, job_id, message, error_detail }
6. Agent returns response to Claude
7. If errors, Claude fixes and repeats
```

**Note:** Unlike original plan, execution is synchronous - no polling needed. The ABAP API returns immediately with the result.

## ABAP Code Flow

```abap
" Pull flow in ZCL_ABAPGIT_AGENT
METHOD pull.

  " Configure credentials if provided
  configure_credentials( ).

  " Find repository
  zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url( ).

  " Refresh and deserialize
  mo_repo->refresh( ).
  mo_repo->deserialize( is_checks = ls_checks ii_log = mo_repo->get_log( ) ).

  " Check for inactive objects
  check_inactive_objects( IMPORTING iv_package = mo_repo->get_package( ) ).

  " Return result
  rs_result-success = abap_true.
ENDMETHOD.
```

## Error Handling

| Error Type | Handling |
|------------|----------|
| Syntax Error | Parse TADIR for inactive objects, return object names |
| Activation Error | Return detailed error_message with object list |
| Network Timeout | Handled by local agent retry logic |
| Git Not Found | Clear error message in response |

## Configuration

#### ABAP System
- REST handler ICF path: `sap/bc/z_abapgit_agent`
- Handler class: `ZCL_ABAPGIT_AGENT_HANDLER`

#### Local Agent (.abapGitAgent)
```json
{
  "host": "your-sap-system.com",
  "sapport": 44300,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "agent": {
    "port": 3000,
    "pollInterval": 5000
  }
}
```

## Key Differences from Original Plan

1. **Simplified Architecture**: Pull and activate combined into single `pull` method
2. **REST API**: Uses direct ICF REST handlers instead of RFC
3. **No Background Job**: Synchronous execution with immediate response
4. **Error Detection**: Uses TADIR query for inactive objects instead of separate log table
5. **No Separate Activate API**: Pull handles activation automatically via deserialize

## Future Enhancements

- [ ] Async job processing for large repositories
- [ ] WebSocket for real-time updates
- [ ] OAuth authentication
- [ ] Multi-system support
- [ ] Repository management UI
- [ ] **Syntax Error Detail Parsing**: Extract line numbers, error codes from syntax errors. Query SEPSA/TRINT_OBJECT_LOG for detailed error info (line, column, fix suggestions)
