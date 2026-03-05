---
layout: default
title: Background Job Architecture
parent: Architecture
grand_parent: Reference
nav_order: 1
---

# Background Job Architecture

Complete guide to the background job infrastructure enabling asynchronous command execution with real-time progress reporting.

## Overview

Long-running ABAP operations (e.g., import command processing 3,700+ files) are executed asynchronously as background jobs with real-time progress updates via polling, preventing HTTP timeouts and providing better user experience.

### Key Features

- ✅ **No Timeouts**: HTTP requests return immediately with job ID
- ✅ **Progress Tracking**: Real-time progress updates via polling
- ✅ **Resource Efficient**: HTTP work processes released immediately
- ✅ **Automatic Detection**: Commands implementing `zif_abgagt_progressable` run in background automatically
- ✅ **Generic Infrastructure**: Single executor for all async commands
- ✅ **Backward Compatible**: Non-progressable commands run synchronously

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│ CLI (Node.js)                                           │
│ - POST /import (data)                                   │
│ - Receive HTTP 202 + jobNumber                          │
│ - Poll GET /import?jobNumber=X every 2s                 │
│ - Display progress bar                                  │
└────────────┬────────────────────────────────────────────┘
             │
    ┌────────▼─────────────────────────────────────────┐
    │ REST Resource (ZCL_ABGAGT_RESOURCE_IMPORT)       │
    │ POST: Start job → Return 202 Accepted            │
    │ GET:  Read status → Return progress JSON         │
    └────────┬─────────────────────────────────────────┘
             │
    ┌────────▼─────────────────────────────────────────┐
    │ Decision Engine (ZCL_ABGAGT_BG_DECISION)         │
    │ Priority 1: force_background config              │
    │ Priority 2: force_sync config                    │
    │ Priority 3: Implements progressable? ← IMPORT    │
    │ Priority 4: Request parameter                    │
    │ Priority 5: Heuristics                           │
    │ Default: Synchronous                             │
    └────────┬────────────┬────────────────────────────┘
             │            │
      YES ◄──┘            └──► NO
       │                       │
       │                  ┌────▼──────────────────────┐
       │                  │ Synchronous Execution     │
       │                  │ execute() → return 200 OK │
       │                  └───────────────────────────┘
       │
  ┌────▼─────────────────────────────────────────────┐
  │ Scheduler (ZCL_ABGAGT_BG_SCHEDULER)              │
  │ - Serialize data to JSON                         │
  │ - JOB_OPEN → SUBMIT → JOB_CLOSE                 │
  │ - Return job_name + job_number                   │
  └────────┬─────────────────────────────────────────┘
           │
  ┌────────▼─────────────────────────────────────────┐
  │ Generic Executor (Z_ABGAGT_BG_EXECUTOR)          │
  │ - Get job number from runtime                    │
  │ - Get command from factory                       │
  │ - Check if progressable → register logger        │
  │ - execute(p_data)                                │
  │ - Update final status                            │
  └────────┬─────────────────────────────────────────┘
           │
  ┌────────▼─────────────────────────────────────────┐
  │ Command (ZCL_ABGAGT_COMMAND_IMPORT)              │
  │ implements zif_abgagt_progressable               │
  │                                                  │
  │ RAISE EVENT progress_update (10%) Parse params  │
  │ RAISE EVENT progress_update (20%) Find repo     │
  │ RAISE EVENT progress_update (40%) Refresh       │
  │ RAISE EVENT progress_update (60%) Stage files   │
  │ RAISE EVENT progress_update (75%) Prepare       │
  │ RAISE EVENT progress_update (90%) Push          │
  │ RAISE EVENT progress_update (100%) Complete     │
  └────────┬─────────────────────────────────────────┘
           │
  ┌────────▼─────────────────────────────────────────┐
  │ Logger (ZCL_ABGAGT_BG_LOGGER)                    │
  │ - Listens to progress_update events              │
  │ - Updates status via status manager              │
  └────────┬─────────────────────────────────────────┘
           │
  ┌────────▼─────────────────────────────────────────┐
  │ Status Manager (ZCL_ABGAGT_BG_STATUS_MGR)        │
  │ implements zif_abgagt_job_status_mgr             │
  │ - update_status: Store to SHARED BUFFER          │
  │ - get_status: Read from SHARED BUFFER            │
  │ - delete_status: Cleanup after completion        │
  └──────────────────────────────────────────────────┘
```

## Components

### 1. Decision Engine (`ZCL_ABGAGT_BG_DECISION`)

Determines whether a command should run synchronously or asynchronously using 5-priority logic.

**Priority Order:**
1. **force_background** - Resource config forces background (highest priority)
2. **force_sync** - Resource config forces synchronous
3. **Progressable Interface** - Command implements `zif_abgagt_progressable` ← Import uses this
4. **Request Parameter** - Explicit `background=X` in request
5. **Heuristics** - Smart detection (file count, package size)
6. **Default** - Synchronous execution (lowest priority)

**Example - Import Command:**
```abap
CLASS zcl_abgagt_command_import DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    INTERFACES zif_abgagt_progressable.  " ← Detected by decision engine
ENDCLASS.

" Decision engine checks:
TRY.
    lo_progressable ?= lo_command.
    " Success → Run in background (Priority 3)
  CATCH cx_sy_move_cast_error.
    " Not progressable → Run synchronously
ENDTRY.
```

### 2. Scheduler (`ZCL_ABGAGT_BG_SCHEDULER`)

Creates and submits background jobs.

**Responsibilities:**
- Serialize command data to JSON
- Create background job (`JOB_OPEN`)
- Submit generic executor program
- Start job immediately (`JOB_CLOSE` with `strtimmed='X'`)
- Return job name and number

**Job Naming:** `ABGAGT_<COMMAND>_<TIMESTAMP>`
Example: `ABGAGT_IMPORT_20260305055407`

### 3. Generic Executor (`Z_ABGAGT_BG_EXECUTOR`)

Universal background job program that works with any command.

**Flow:**
1. Get job number from runtime (`GET_JOB_RUNTIME_INFO`)
2. Initialize status manager
3. Get command from factory using command type
4. Check if progressable → register logger for progress events
5. Execute command with JSON data
6. Update final status (completed/error)

**Parameters:**
- `p_cmd` - Command type constant (e.g., 'IMPORT')
- `p_data` - Serialized command data (JSON string)

### 4. Logger (`ZCL_ABGAGT_BG_LOGGER`)

Captures progress events and updates job status.

**Event Handler:**
```abap
METHOD on_progress.
  ls_status = VALUE #(
    job_number    = mv_job_number
    status        = 'running'
    stage         = iv_stage
    message       = iv_message
    progress      = iv_progress
    current       = iv_current
    total         = iv_total
    updated_at    = <timestamp>
  ).

  mo_status_mgr->update_status( ls_status ).
ENDMETHOD.
```

### 5. Status Manager (`ZCL_ABGAGT_BG_STATUS_MGR`)

Manages job status using SAP SHARED BUFFER.

**Interface:** `zif_abgagt_job_status_mgr`

**Storage:** SHARED BUFFER indx(zz)
- **RELID** = 'ZZ' (customer namespace)
- **SRTFD** = 'JOBSTATUS_<job_number>'
- **Cross-process**: Works across different work processes
- **Cross-server**: Works across application servers (database-backed)

**Methods:**
- `update_status` - Store/update status
- `get_status` - Retrieve by job number
- `delete_status` - Cleanup after completion

## Request Flow

### 1. Client Sends Request

```bash
POST /sap/bc/z_abapgit_agent/import
Content-Type: application/json

{
  "url": "https://github.com/user/repo.git",
  "message": "Initial import",
  "username": "user",
  "password": "token"
}
```

### 2. Resource Base Processing

```abap
METHOD if_rest_resource~post.
  " Parse request
  parse_request( ).

  " Get command from factory
  lo_command = lo_factory->get_command( lv_constant ).

  " Decision: background or sync?
  lo_decision = NEW zcl_abgagt_bg_decision( ).
  lv_run_in_bg = lo_decision->should_run_in_background(
    io_command = lo_command
    is_config  = get_bg_config( )
  ).

  IF lv_run_in_bg = abap_true.
    " Background execution
    lo_scheduler = NEW zcl_abgagt_bg_scheduler( ).
    ls_job_info = lo_scheduler->schedule_command(
      iv_command_type = lv_constant
      is_command_data = <ls_request>
    ).

    " Return HTTP 202 Accepted
    return_job_scheduled( ls_job_info ).
  ELSE.
    " Synchronous execution
    lv_result = lo_command->execute( is_param = <ls_request> ).

    " Return HTTP 200 OK
    return_success( lv_result ).
  ENDIF.
ENDMETHOD.
```

### 3. Response - HTTP 202 Accepted

```json
{
  "success": "X",
  "jobName": "ABGAGT_IMPORT_20260305055407",
  "jobNumber": "06231400",
  "status": "scheduled",
  "message": "Import job started"
}
```

### 4. Background Job Execution

```abap
REPORT z_abgagt_bg_executor.

PARAMETERS: p_cmd  TYPE string,  " 'IMPORT'
            p_data TYPE string.  " JSON data

START-OF-SELECTION.
  " Get job number
  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING jobcount = lv_job_number.

  " Initialize status
  lo_status_mgr = NEW zcl_abgagt_bg_status_mgr( ).
  lo_status_mgr->update_status( VALUE #(
    job_number = lv_job_number
    status     = 'running'
    progress   = 0
  ) ).

  " Get command
  lo_command = lo_factory->get_command( p_cmd ).

  " Register logger if progressable
  TRY.
      lo_progressable ?= lo_command.
      lo_logger = NEW zcl_abgagt_bg_logger(
        io_status_mgr = lo_status_mgr
        iv_job_number = lv_job_number
      ).
      SET HANDLER lo_logger->on_progress FOR lo_progressable.
    CATCH cx_sy_move_cast_error.
      " Not progressable - continue without logger
  ENDTRY.

  " Execute command
  lv_result = lo_command->execute( is_param = p_data ).

  " Update final status
  lo_status_mgr->update_status( VALUE #(
    job_number   = lv_job_number
    status       = 'completed'
    progress     = 100
    result       = lv_result
    completed_at = <timestamp>
  ) ).
```

### 5. Client Polls Status

```bash
GET /sap/bc/z_abapgit_agent/import?jobNumber=06231400
```

**Response (Running):**
```json
{
  "jobNumber": "06231400",
  "status": "running",
  "stage": "STAGE_FILES",
  "message": "Staging files (1250 of 3701)",
  "progress": 65,
  "current": 1250,
  "total": 3701,
  "startedAt": "20260305055407",
  "updatedAt": "20260305055420"
}
```

**Response (Completed):**
```json
{
  "jobNumber": "06231400",
  "status": "completed",
  "stage": "COMPLETED",
  "message": "Import completed successfully",
  "progress": 100,
  "result": "{\"success\":\"X\",\"filesStaged\":3701}",
  "startedAt": "20260305055407",
  "completedAt": "20260305055635"
}
```

## Implementing Progressable Commands

### Step 1: Implement Interface

```abap
CLASS zcl_my_command DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
    INTERFACES zif_abgagt_progressable.  " ← Add this
ENDCLASS.
```

### Step 2: Raise Progress Events

```abap
METHOD zif_abgagt_command~execute.
  TRY.
      " Stage 1: Initialize (10%)
      RAISE EVENT zif_abgagt_progressable~progress_update
        EXPORTING
          iv_stage    = 'INITIALIZE'
          iv_message  = 'Initializing...'
          iv_progress = 10.

      " Do work...

      " Stage 2: Processing (50%)
      RAISE EVENT zif_abgagt_progressable~progress_update
        EXPORTING
          iv_stage    = 'PROCESS'
          iv_message  = 'Processing data...'
          iv_progress = 50
          iv_current  = <current_item>
          iv_total    = <total_items>.

      " Do more work...

      " Stage 3: Complete (100%)
      RAISE EVENT zif_abgagt_progressable~progress_update
        EXPORTING
          iv_stage    = 'COMPLETE'
          iv_message  = 'Completed successfully'
          iv_progress = 100.

      rv_result = '{"success":"X"}'.

    CATCH cx_root INTO DATA(lx_error).
      rv_result = '{"error":"' && lx_error->get_text( ) && '"}'.
  ENDTRY.
ENDMETHOD.
```

### Step 3: That's It!

The command will automatically:
- ✅ Be detected as progressable (Priority 3)
- ✅ Run as background job when executed
- ✅ Report progress via events
- ✅ Return HTTP 202 Accepted
- ✅ Support status polling via GET

## CLI Integration

```javascript
// Generic utility: src/utils/backgroundJobPoller.js

// 1. Start job
const jobInfo = await startBackgroundJob(http, endpoint, data, csrfToken);
console.log(`✅ Job started: ${jobInfo.jobNumber}`);

// 2. Poll for completion
const finalResult = await pollForCompletion(http, endpoint, jobInfo.jobNumber, {
  pollInterval: 2000,
  maxAttempts: 300,
  onProgress: (progress, message) => {
    displayProgress(progress, message);
  }
});

// 3. Display result
if (finalResult.status === 'completed') {
  console.log('✅ Import completed successfully!');
  console.log(`   Files staged: ${finalResult.result.filesStaged}`);

  const timeSpent = calculateTimeSpent(finalResult.startedAt, finalResult.completedAt);
  console.log(`⏱️  Time spent: ${timeSpent}`);
}
```

## HTTP Status Codes

| Code | Meaning | When |
|------|---------|------|
| 200 OK | Success (synchronous) | Non-progressable commands complete immediately |
| 202 Accepted | Job scheduled | Progressable commands scheduled for background |
| 400 Bad Request | Validation failed | Invalid request parameters |
| 404 Not Found | Job not found | GET with invalid/expired jobNumber |
| 500 Internal Server Error | Execution failed | Exception during processing |

## Best Practices

### Progress Updates

**DO:**
- ✅ Update at meaningful milestones (not in tight loops)
- ✅ Calculate progress as percentage 0-100
- ✅ Provide descriptive messages
- ✅ Include current/total for loops

**DON'T:**
- ❌ Update too frequently (creates overhead)
- ❌ Leave progress at 0% for long periods
- ❌ Jump progress backwards

### Error Handling

Always update status on error:

```abap
CATCH cx_root INTO DATA(lx_error).
  RAISE EVENT zif_abgagt_progressable~progress_update
    EXPORTING
      iv_stage    = 'ERROR'
      iv_message  = 'Processing failed'
      iv_progress = <last_progress>.

  rv_result = '{"error":"' && lx_error->get_text( ) && '"}'.
```

### Polling Intervals

| Operation Duration | Recommended Interval |
|-------------------|---------------------|
| < 30 seconds | 1 second |
| 30s - 5 minutes | 2 seconds |
| > 5 minutes | 5 seconds |

### Cleanup

Status is automatically deleted when:
- Client receives completed status (final GET)
- Client receives error status (final GET)

No manual cleanup needed in most cases.

## Testing

### Unit Test - Status Manager

```abap
METHOD test_status_roundtrip.
  DATA: lo_mgr TYPE REF TO zif_abgagt_job_status_mgr,
        ls_status TYPE zif_abgagt_job_status_mgr=>ty_job_status.

  lo_mgr = NEW zcl_abgagt_bg_status_mgr( ).

  " Write status
  ls_status-job_number = '99999999'.
  ls_status-status = 'running'.
  ls_status-progress = 50.
  lo_mgr->update_status( ls_status ).

  " Read status
  DATA(ls_read) = lo_mgr->get_status( '99999999' ).
  cl_abap_unit_assert=>assert_equals(
    act = ls_read-progress
    exp = 50
  ).

  " Cleanup
  lo_mgr->delete_status( '99999999' ).
ENDMETHOD.
```

### Integration Test - Command Detection

```abap
METHOD test_import_cmd_progressable.
  DATA: lo_decision TYPE REF TO zif_abgagt_bg_decision,
        lo_command  TYPE REF TO zif_abgagt_command.

  lo_decision = NEW zcl_abgagt_bg_decision( ).
  lo_command = NEW zcl_abgagt_command_import( ).

  " Import implements progressable → should run in background
  DATA(lv_result) = lo_decision->should_run_in_background(
    io_command = lo_command
  ).

  cl_abap_unit_assert=>assert_true(
    act = lv_result
    msg = 'Import should run in background'
  ).
ENDMETHOD.
```

## Limitations

1. **SHARED BUFFER**: Ephemeral storage, entries may expire automatically
2. **No Cancellation**: Once started, job runs to completion
3. **No Job Listing**: Cannot list all active jobs (query by job number only)
4. **Polling Overhead**: Many clients polling increases load

## Future Enhancements

Consider for production environments:
- Database table instead of SHARED BUFFER for persistence
- Job cancellation API
- WebSocket alternative for push updates
- Job history and audit logging
- Concurrent job limits and queue management

---

**Status: Fully Implemented and Tested** ✅
