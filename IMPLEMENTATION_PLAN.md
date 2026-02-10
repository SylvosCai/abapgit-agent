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

#### 1.1 Main Class: ZCL_ABGAGT_AGENT (DONE)
```abap
CLASS zcl_abgagt_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES: zif_abgagt_agent.
ENDCLASS.
```
**Features:**
- Pull repository and activate objects in one operation
- Check for inactive objects after activation
- Detailed error reporting with exception chaining

#### 1.2 Interface: ZIF_ABGAGT_AGENT (DONE)
```abap
INTERFACE zif_abgagt_agent PUBLIC.
  TYPES: BEGIN OF ty_result,
    success TYPE abap_bool,
    job_id TYPE string,
    message TYPE string,
    error_detail TYPE string,
    activated_count TYPE i,
    failed_count TYPE i,
    started_at TYPE timestampl,
    finished_at TYPE timestampl,
    log_messages TYPE ty_object_list,
    activated_objects TYPE ty_object_list,
    failed_objects TYPE ty_object_list,
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
              it_files TYPE string_table OPTIONAL
    RETURNING VALUE(rs_result) TYPE ty_result
    RAISING zcx_abapgit_exception.

  METHODS inspect
    IMPORTING iv_file TYPE string
    RETURNING VALUE(rs_result) TYPE ty_inspect_result.

  METHODS run_tests
    IMPORTING iv_package TYPE devclass OPTIONAL
              it_objects TYPE ty_object_keys OPTIONAL
    RETURNING VALUE(rs_result) TYPE ty_unit_result.
ENDINTERFACE.
```

#### 1.3 Command Factory Pattern (DONE)
```
ZCL_ABGAGT_CMD_FACTORY → ZCL_ABGAGT_COMMAND_PULL
                      → ZCL_ABGAGT_COMMAND_INSPECT
                      → ZCL_ABGAGT_COMMAND_UNIT
```

#### 1.4 REST API Handlers (DONE)
- `ZCL_ABGAGT_REST_HANDLER` - Router for all endpoints
- `ZCL_ABGAGT_RESOURCE_PULL` - POST /pull endpoint
- `ZCL_ABGAGT_RESOURCE_INSPECT` - POST /inspect endpoint (uses Code Inspector)
- `ZCL_ABGAGT_RESOURCE_UNIT` - POST /unit endpoint (uses CL_SUT_AUNIT_RUNNER)

### 2. Local Agent (Node.js)

#### 2.1 Project Structure (DONE)
```
abapgit-agent/
├── package.json
├── bin/
│   └── abapgit-agent     # CLI entry point
├── src/
│   ├── agent.js          # Main agent class
│   ├── abap-client.js    # REST client for ABAP
│   ├── command-client.js # Command-based API client
│   ├── server.js         # HTTP server for Claude
│   ├── config.js         # Configuration
│   └── logger.js         # Logging
├── abap/
│   ├── zcl_abgagt_agent.clas.abap
│   ├── zif_abgagt_agent.intf.abap
│   ├── zcl_abgagt_cmd_factory.clas.abap
│   ├── zcl_abgagt_command_*.clas.abap
│   └── zif_abgagt_command.intf.abap
├── tests/
│   ├── agent.test.js
│   ├── abap-client.test.js
│   ├── config.test.js
│   └── server.test.js
├── API.md
├── README.md
└── CLAUDE.md
```

#### 2.2 REST API Endpoints (DONE)

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check (also fetches CSRF token) |
| POST | `/pull` | Pull and activate repository |
| POST | `/inspect` | Syntax check via Code Inspector (SCI) |
| POST | `/unit` | Execute AUnit tests using CL_SUT_AUNIT_RUNNER |

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
- Handler class: `ZCL_ABGAGT_REST_HANDLER`

#### Local Agent (.abapGitAgent)
```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "gitUsername": "git-username",
  "gitPassword": "git-token",
  "useCommandApi": false
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
- [x] **Syntax Error Detail Parsing**: Extract line numbers, error codes from syntax errors (DONE via `/inspect`)
- [x] **Unit Test Execution**: Execute AUnit tests for test classes (DONE via `/unit`)

## DONE: Unit Test Endpoint (`/unit`)

### Overview
Implemented AUnit test execution using `CL_SUT_AUNIT_RUNNER` for test classes.

### API Design

**POST /unit**

Request body:
```json
{
  "files": ["zcl_my_test.clas.testclasses.abap", "zcl_other.clas.testclasses.abap"]
}
```

Response:
```json
{
  "success": "X",
  "test_count": 10,
  "passed_count": 8,
  "failed_count": 2,
  "message": "2 of 10 tests failed",
  "errors": [
    {
      "class_name": "ZCL_MY_TEST",
      "method_name": "TEST_METHOD_1",
      "error_kind": "ERROR",
      "error_text": "Expected X but got Y"
    }
  ]
}
```

### Implementation (COMPLETED)

#### ABAP Side
- [x] Create `ZCL_ABGAGT_COMMAND_UNIT` - Unit test command using CL_SUT_AUNIT_RUNNER
- [x] Error extraction from nested structure: TAB_OBJECTS → TAB_TESTCLASSES → TAB_METHODS → STR_ERROR
- [x] `run_syntax_check()` method for running AUnit tests

#### CLI Side
- [x] Add `unitTest()` method to `src/command-client.js`
- [x] Add `unitCheck()` method to `src/agent.js`
- [x] Add `unit` command to `bin/abapgit-agent`
  - `--files <file1>,<file2>,...` - Run tests for test class files

### ABAP APIs Used
- `CL_SUT_AUNIT_RUNNER` - Main AUnit runner class
  - `S_CREATE()` - Create runner instance
  - `RUN()` - Execute tests
  - `STR_RESULTS` - Test statistics (cnt_testmethods, cnt_ok_methods, cnt_error_methods)
  - `TAB_OBJECTS` - Detailed results with nested structure

### CLI Commands
```bash
# Run unit tests for test class files
abapgit-agent unit --files abap/zcl_my_test.clas.testclasses.abap

# Run tests for multiple test class files
abapgit-agent unit --files abap/zcl_test1.clas.testclasses.abap,abap/zcl_test2.clas.testclasses.abap
```
