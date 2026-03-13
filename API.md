---
layout: default
title: REST API Reference
nav_order: 2
parent: Reference
---

# REST API Reference

The ABAP system exposes these endpoints via SICF handler: `sap/bc/z_abapgit_agent`

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check (also fetches CSRF token) |
| POST | `/pull` | Pull and activate repository |
| POST | `/create` | Create abapGit online repository |
| POST | `/delete` | Delete abapGit repository from ABAP |
| POST | `/import` | **Import objects from package to git (async)** |
| GET | `/import?jobNumber=X` | **Poll import job status** |
| POST | `/status` | Check if repo exists in ABAP system |
| POST | `/syntax` | **Pre-commit syntax check (CLAS, INTF, PROG, DDLS)** |
| POST | `/inspect` | Inspect source file for issues (syntax check, CDS validation) |
| POST | `/unit` | Execute unit tests (AUnit) |
| POST | `/tree` | Display package hierarchy tree |
| POST | `/list` | List ABAP objects in a package with filtering |
| POST | `/view` | View ABAP object definitions |
| POST | `/preview` | Preview table/CDS view data |
| POST | `/where` | Find where-used list for ABAP objects |

## GET /health

Health check endpoint - also used to fetch CSRF token for POST requests.

```bash
curl "https://your-system:44300/sap/bc/z_abapgit_agent/health" \
  -u USER:PASSWORD \
  -H "sap-client: 100"
```

Response:
```json
{"status":"OK","version":"1.0.0"}
```

## POST /pull

Pull and activate repository from git.

### Request

Requires CSRF token. First fetch from `/health`:

```bash
# 1. Get CSRF token and cookies
curl -c cookies.txt -D headers.txt -X GET "https://your-system:44300/sap/bc/z_abapgit_agent/health" \
  -u USER:PASSWORD \
  -H "sap-client: 100" \
  -H "X-CSRF-Token: fetch"

# 2. Extract CSRF token
CSRF=$(grep -i "x-csrf-token" headers.txt | awk '{print $2}' | tr -d '\r')

# 3. Pull repository
curl -X POST "https://your-system:44300/sap/bc/z_abapgit_agent/pull" \
  -H "Content-Type: application/json" \
  -H "sap-client: 100" \
  -H "X-CSRF-Token: $CSRF" \
  -b cookies.txt \
  -u USER:PASSWORD \
  -d '{"url": "https://github.com/user/repo.git", "branch": "main"}'
```

### Request Body

```json
{
  "url": "https://github.com/user/repo.git",
  "branch": "main",
  "username": "git-username",
  "password": "git-token",
  "transport_request": "DEVK900001",
  "files": ["zcl_my_class.clas.abap", "zcl_other.clas.abap"]
}
```

### File Format

Files are parsed to extract `(obj_type, obj_name)`:
- `zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS
- `src/zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS (subdirectory support)

### Transport Request

The optional `transport_request` field specifies a transport request number to use for activation:
- If provided, objects are activated in the specified transport
- If omitted, abapGit creates/uses a default transport

### Response (success)

```json
{
  "success": "X",
  "message": "Pull completed successfully",
  "transport_request": "DEVK900001",
  "activated_count": 10,
  "failed_count": 0,
  "activated_objects": [...],
  "failed_objects": [...]
}
```

### Response (with activation errors)

```json
{
  "success": "",
  "message": "Pull completed with errors",
  "error_detail": "CLAS ZCL_MY_CLASS: Syntax error\nException: The statement...",
  "transport_request": "DEVK900001",
  "activated_count": 9,
  "failed_count": 2,
  "activated_objects": [...],
  "failed_objects": [
    {
      "type": "E",
      "text": "The statement METHOD is unexpected",
      "obj_type": "CLAS",
      "obj_name": "ZCL_MY_CLASS",
      "exception": "The statement METHOD is unexpected"
    }
  ]
}
```

## POST /create

Create an abapGit online repository in the ABAP system.

### Request Body

```json
{
  "url": "https://github.com/user/repo.git",
  "branch": "main",
  "package": "$MY_PACKAGE",
  "name": "my-repo",
  "display_name": "My Repository",
  "folder_logic": "PREFIX",
  "folder": "/src/",
  "username": "git-username",
  "password": "git-token"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `url` | String | Git repository URL (required) |
| `branch` | String | Branch name (default: main) |
| `package` | String | ABAP package (required) |
| `name` | String | Repository name (optional) |
| `display_name` | String | Display name (optional) |
| `folder_logic` | String | Folder logic: PREFIX or FULL (default: PREFIX) |
| `folder` | String | Starting folder (optional) |
| `username` | String | Git username/token (optional) |
| `password` | String | Git password/token (optional) |

### Response (success)

```json
{
  "success": "X",
  "repo_key": "abc123",
  "repo_name": "my-repo",
  "message": "Repository created successfully"
}
```

### Response (error)

```json
{
  "success": "",
  "error": "Repository already exists"
}
```

## POST /delete

Delete an abapGit online repository from the ABAP system.

### Request Body

```json
{
  "url": "https://github.com/user/repo.git"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `url` | String | Git repository URL (required) |

### Response (success)

```json
{
  "success": "X",
  "repo_key": "abc123",
  "message": "Repository deleted successfully"
}
```

### Response (not found)

```json
{
  "success": "",
  "error": "No suitable repository found"
}
```

## POST /import (Async)

Import objects from an ABAP package into git. This endpoint runs **asynchronously** as a background job to handle large packages without HTTP timeouts.

### Workflow

```
1. POST /import → Returns HTTP 202 Accepted with jobNumber
2. GET /import?jobNumber=X → Poll every 2 seconds for status
3. Job completes → Final status with result
```

### Request

```bash
# 1. Get CSRF token
curl -c cookies.txt -D headers.txt -X GET "https://your-system:44300/sap/bc/z_abapgit_agent/health" \
  -u USER:PASSWORD \
  -H "sap-client: 100" \
  -H "X-CSRF-Token: fetch"

CSRF=$(grep -i "x-csrf-token" headers.txt | awk '{print $2}' | tr -d '\r')

# 2. Start import job
curl -X POST "https://your-system:44300/sap/bc/z_abapgit_agent/import" \
  -H "Content-Type: application/json" \
  -H "sap-client: 100" \
  -H "X-CSRF-Token: $CSRF" \
  -b cookies.txt \
  -u USER:PASSWORD \
  -d '{
    "url": "https://github.com/user/repo.git",
    "branch": "main",
    "package": "$MY_PACKAGE",
    "message": "feat: initial import from ABAP",
    "username": "git-username",
    "password": "ghp_token"
  }'

# 3. Poll for status (repeat until status is "completed" or "error")
JOB_NUMBER="12345678"
curl -X GET "https://your-system:44300/sap/bc/z_abapgit_agent/import?jobNumber=$JOB_NUMBER" \
  -H "sap-client: 100" \
  -b cookies.txt \
  -u USER:PASSWORD
```

### Request Body

```json
{
  "url": "https://github.com/user/repo.git",
  "branch": "main",
  "package": "$MY_PACKAGE",
  "message": "feat: initial import from ABAP",
  "username": "git-username",
  "password": "ghp_token"
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `url` | String | Yes | Git repository URL |
| `branch` | String | No | Branch name (default: main) |
| `package` | String | No | ABAP package (auto-detected from repo if omitted) |
| `message` | String | No | Commit message (default: "feat: initial import from ABAP package \<package\>") |
| `username` | String | No | Git username (can also use credentials from `.abapGitAgent`) |
| `password` | String | No | Git token/password |

### Response: Job Scheduled (HTTP 202 Accepted)

```json
{
  "success": true,
  "command": "import",
  "status": "accepted",
  "jobName": "IMPORT_20260305103045",
  "jobNumber": "12345678",
  "message": "Command scheduled for background execution"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `success` | Boolean | Always `true` when job is scheduled |
| `command` | String | Command type ("import") |
| `status` | String | Always "accepted" for initial response |
| `jobName` | String | Background job name |
| `jobNumber` | String | Job number for polling (8 digits with leading zeros) |
| `message` | String | Status message |

---

## GET /import?jobNumber=X

Poll the status of a running import job.

### Request

```bash
curl -X GET "https://your-system:44300/sap/bc/z_abapgit_agent/import?jobNumber=12345678" \
  -H "sap-client: 100" \
  -b cookies.txt \
  -u USER:PASSWORD
```

### Response: Job Running

```json
{
  "job_name": "IMPORT_20260305103045",
  "job_number": "12345678",
  "status": "running",
  "stage": "STAGE_FILES",
  "message": "Staging files (1250 of 3701)",
  "progress": 65,
  "current": 1250,
  "total": 3701,
  "started_at": "20260305103045",
  "updated_at": "20260305103112"
}
```

### Response: Job Completed (Success)

```json
{
  "job_name": "IMPORT_20260305103045",
  "job_number": "12345678",
  "status": "completed",
  "stage": "FINISHED",
  "message": "Import completed successfully",
  "progress": 100,
  "result": "{\"success\":\"X\",\"filesStaged\":\"3701\",\"commitMessage\":\"feat: initial import from ABAP package $MY_PACKAGE\"}",
  "started_at": "20260305103045",
  "updated_at": "20260305103520",
  "completed_at": "20260305103520"
}
```

### Response: Job Failed

```json
{
  "job_name": "IMPORT_20260305103045",
  "job_number": "12345678",
  "status": "error",
  "stage": "FAILED",
  "message": "Error during import",
  "error_message": "Repository not found",
  "progress": 30,
  "started_at": "20260305103045",
  "updated_at": "20260305103112",
  "completed_at": "20260305103112"
}
```

### Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `job_name` | String | Background job name |
| `job_number` | String | Job number (8 digits) |
| `status` | String | `scheduled`, `running`, `completed`, or `error` |
| `stage` | String | Current stage: `INITIALIZATION`, `EXECUTION`, `FIND_REPOSITORY`, `REFRESH_REPOSITORY`, `STAGE_FILES`, `PREPARE_COMMIT`, `PUSH`, `FINISHED`, `FAILED` |
| `message` | String | Human-readable status message |
| `progress` | Integer | Progress percentage (0-100) |
| `current` | Integer | Current item (for staging files) |
| `total` | Integer | Total items (for staging files) |
| `result` | String | JSON string with final result (when status is "completed") |
| `error_message` | String | Error details (when status is "error") |
| `started_at` | String | Timestamp when job started (YYYYMMDDHHmmss) |
| `updated_at` | String | Timestamp of last status update |
| `completed_at` | String | Timestamp when job finished (present when status is "completed" or "error") |

### Import Stages

The import job progresses through these stages:

| Stage | Progress | Description |
|-------|----------|-------------|
| `INITIALIZATION` | 0% | Job scheduled, waiting to start |
| `FIND_REPOSITORY` | 10% | Locating abapGit repository by URL |
| `REFRESH_REPOSITORY` | 30% | Refreshing repository state from ABAP |
| `STAGE_FILES` | 50-70% | Staging local files from package (shows file count) |
| `PREPARE_COMMIT` | 70% | Building commit message and metadata |
| `PUSH` | 90% | Committing and pushing to remote repository |
| `FINISHED` | 100% | Import completed successfully |
| `FAILED` | - | Error occurred (check `error_message`) |

### Polling Recommendations

- **Poll interval**: 2 seconds
- **Timeout**: 10 minutes (for very large packages)
- **Error handling**: If GET request fails, retry up to 3 times before giving up
- **Status check**: Continue polling while `status` is `scheduled` or `running`
- **Completion**: Stop polling when `status` is `completed` or `error`

### Example Polling Script

```bash
#!/bin/bash

JOB_NUMBER="$1"
MAX_ATTEMPTS=300  # 10 minutes with 2-second intervals

for ((i=1; i<=MAX_ATTEMPTS; i++)); do
  RESPONSE=$(curl -s -X GET "https://your-system:44300/sap/bc/z_abapgit_agent/import?jobNumber=$JOB_NUMBER" \
    -H "sap-client: 100" \
    -b cookies.txt \
    -u USER:PASSWORD)

  STATUS=$(echo "$RESPONSE" | jq -r '.status')
  MESSAGE=$(echo "$RESPONSE" | jq -r '.message')
  PROGRESS=$(echo "$RESPONSE" | jq -r '.progress')

  echo "[$i] Status: $STATUS | Progress: $PROGRESS% | $MESSAGE"

  if [ "$STATUS" = "completed" ]; then
    echo "✅ Import completed successfully"
    exit 0
  elif [ "$STATUS" = "error" ]; then
    ERROR=$(echo "$RESPONSE" | jq -r '.error_message')
    echo "❌ Import failed: $ERROR"
    exit 1
  fi

  sleep 2
done

echo "⏱️ Timeout: Import did not complete within 10 minutes"
exit 1
```

## POST /status

Check if an abapGit online repository exists in the ABAP system for a given URL.

### Request Body

```json
{
  "url": "https://github.com/user/repo.git"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `url` | String | Git repository URL (required) |

### Response (found)

```json
{
  "success": true,
  "url": "https://github.com/user/repo.git",
  "status": "Found",
  "repo_key": "abc123",
  "package": "$MY_PACKAGE"
}
```

### Response (not found)

```json
{
  "success": true,
  "url": "https://github.com/user/repo.git",
  "status": "Not found"
}
```

## POST /syntax

**Pre-commit syntax check** for ABAP source code without requiring pull/activation. Checks CLAS, INTF, PROG, and DDLS files directly from local filesystem.

**Key Features:**
- Check syntax before committing to git
- Auto-detection of companion files (locals_def, locals_imp, testclasses)
- FIXPT flag support from XML metadata
- Exact line numbers and filenames in errors

### Request Body

```json
{
  "objects": [
    {
      "type": "CLAS",
      "name": "ZCL_MY_CLASS",
      "source": "CLASS zcl_my_class DEFINITION PUBLIC.\n...",
      "locals_def": "CLASS lcl_helper DEFINITION.\n...",
      "locals_imp": "CLASS lcl_helper IMPLEMENTATION.\n...",
      "testclasses": "CLASS ltcl_test DEFINITION FOR TESTING.\n...",
      "fixpt": "X"
    }
  ],
  "uccheck": "X"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `objects` | Array | List of objects to check (required) |
| `uccheck` | String | Unicode check mode: 'X' (Standard) or '5' (Cloud). Default: 'X' |

### Object Fields

| Field | Type | Description |
|-------|------|-------------|
| `type` | String | Object type: CLAS, INTF, PROG, DDLS (required) |
| `name` | String | Object name (required) |
| `source` | String | Main source code with `\n` line separators (required) |
| `locals_def` | String | Local class definitions (CLAS only, optional) |
| `locals_imp` | String | Local class implementations (CLAS only, optional) |
| `testclasses` | String | Test classes (CLAS only, optional) |
| `fixpt` | String | FIXPT flag from XML: 'X' or '' (optional) |

### Supported Object Types

| Type | Description | Supports |
|------|-------------|----------|
| CLAS | Class | locals_def, locals_imp, testclasses, fixpt |
| INTF | Interface | fixpt |
| PROG | Program | uccheck, fixpt |
| DDLS | CDS View/Entity | Annotations required |

### Response (success)

```json
{
  "success": true,
  "command": "SYNTAX",
  "message": "All 1 object(s) passed syntax check",
  "results": [
    {
      "object_type": "CLAS",
      "object_name": "ZCL_MY_CLASS",
      "success": true,
      "error_count": 0,
      "warning_count": 0,
      "errors": [],
      "warnings": [],
      "message": "Syntax check passed"
    }
  ]
}
```

### Response (with errors)

```json
{
  "success": false,
  "command": "SYNTAX",
  "message": "1 of 1 object(s) have syntax errors",
  "results": [
    {
      "object_type": "CLAS",
      "object_name": "ZCL_MY_CLASS",
      "success": false,
      "error_count": 2,
      "warning_count": 0,
      "errors": [
        {
          "line": 15,
          "text": "The statement METHOD is unexpected",
          "include": "Main class"
        },
        {
          "line": 42,
          "text": "Variable LV_TEST not declared",
          "include": "Local implementation"
        }
      ],
      "warnings": [],
      "message": "Syntax check failed with 2 error(s)"
    }
  ]
}
```

### Response (unsupported type)

```json
{
  "success": false,
  "command": "SYNTAX",
  "message": "1 of 1 object(s) have syntax errors",
  "results": [
    {
      "object_type": "FUGR",
      "object_name": "Z_MY_FUGR",
      "success": false,
      "error_count": 1,
      "errors": [
        {
          "line": 1,
          "text": "Unsupported object type: FUGR. Syntax command only supports CLAS, INTF, PROG, DDLS. Use 'pull' command for other object types."
        }
      ],
      "message": "Unsupported object type: FUGR. Use 'pull' command instead."
    }
  ]
}
```

### Error Fields

| Field | Type | Description |
|-------|------|-------------|
| `line` | Integer | Line number in source (1-based) |
| `text` | String | Error message |
| `include` | String | Source location: "Main class", "Local definitions", "Local implementation", "Test classes" |

### Notes

- **Line numbers are exact** - they match the source code strings provided
- **FIXPT support** - Reads from XML metadata, defaults to blank if not specified
- **Auto-detection** - CLI automatically detects and includes companion files
- **No activation required** - Checks happen before git commit

## POST /inspect

Inspect source files for issues (runs syntax check via Code Inspector, validates CDS views).

### Request Body

```json
{
  "files": ["zcl_my_class.clas.abap", "zc_my_view.ddls.asddls"]
}
```

The endpoint parses file names to extract `obj_type` and `obj_name`:
- `zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS
- `src/zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS
- `zc_my_view.ddls.asddls` → DDLS, ZC_MY_VIEW

### Supported Object Types

| Type | Description | Validation Method |
|------|-------------|------------------|
| CLAS | Class | Code Inspector (SCI) |
| INTF | Interface | Code Inspector (SCI) |
| PROG | Program | Code Inspector (SCI) |
| FUGR | Function Group | Code Inspector (SCI) |
| DDLS | CDS View/Entity | DDL Handler (CL_DD_DDL_HANDLER_FACTORY) |

### Response (success)

```json
{
  "success": "X",
  "object_type": "CLAS",
  "object_name": "ZCL_MY_CLASS",
  "error_count": 0,
  "errors": []
}
```

### Response (with errors)

```json
{
  "success": "",
  "object_type": "CLAS",
  "object_name": "ZCL_MY_CLASS",
  "error_count": 2,
  "errors": [
    {
      "line": "15",
      "column": "12",
      "text": "\"MESSAGE\" is not a declaration"
    },
    {
      "line": "20",
      "column": "5",
      "text": "Variable \"LV_TEST\" not found"
    }
  ]
}
```

## POST /unit

Execute unit tests (AUnit) for test class files.

### Request Body

```json
{
  "files": ["zcl_my_test.clas.testclasses.abap", "zcl_other_test.clas.testclasses.abap"]
}
```

The endpoint parses file names to extract `obj_type` and `obj_name`, then runs AUnit tests using `CL_SUT_AUNIT_RUNNER`.

### Response (success)

```json
{
  "success": "X",
  "test_count": 10,
  "passed_count": 10,
  "failed_count": 0,
  "message": "All 10 tests passed",
  "errors": []
}
```

### Response (with failures)

```json
{
  "success": "",
  "test_count": 5,
  "passed_count": 3,
  "failed_count": 2,
  "message": "2 of 5 tests failed",
  "errors": [
    {
      "class_name": "ZCL_MY_TEST",
      "method_name": "TEST_METHOD_1",
      "error_kind": "ERROR",
      "error_text": "Expected X but got Y"
    },
    {
      "class_name": "ZCL_MY_TEST",
      "method_name": "TEST_METHOD_2",
      "error_kind": "FAILURE",
      "error_text": "Reference is initial"
    }
  ]
}
```

## POST /tree

Display package hierarchy tree from ABAP system.

### Request Body

```json
{
  "package": "$MY_PACKAGE",
  "depth": 3,
  "include_objects": true
}
```

| Field | Type | Description |
|-------|------|-------------|
| `package` | String | Package name (required) |
| `depth` | Integer | Maximum depth (default: 3, max: 10) |
| `include_objects` | Boolean | Include object counts by type |

### Response (success)

```json
{
  "success": true,
  "command": "TREE",
  "package": "$MY_PACKAGE",
  "message": "Tree retrieved successfully",
  "parent_package": "$ZSAP_BASE",
  "nodes": [
    {
      "package": "$MY_PACKAGE",
      "parent": "",
      "description": "$MY_PACKAGE",
      "depth": 0,
      "object_count": 10
    },
    {
      "package": "$MY_SUBPACKAGE",
      "parent": "$MY_PACKAGE",
      "description": "$MY_SUBPACKAGE",
      "depth": 1,
      "object_count": 5
    }
  ],
  "total_packages": 2,
  "total_objects": 15,
  "objects": [
    { "object": "CLAS", "count": 8 },
    { "object": "INTF", "count": 2 },
    { "object": "TABL", "count": 5 }
  ],
  "error": ""
}
```

### Response (error)

```json
{
  "success": false,
  "command": "TREE",
  "package": "$NONEXISTENT",
  "error": "Package $NONEXISTENT does not exist"
}
```

## POST /list

List ABAP objects in a package with filtering and pagination.

### Request Body

```json
{
  "package": "$MY_PACKAGE",
  "type": ["CLAS", "INTF"],
  "name_pattern": "ZCL_*",
  "limit": 50,
  "offset": 0
}
```

| Field | Type | Description |
|-------|------|-------------|
| `package` | String | Package name (required) |
| `type` | Array | Object types to filter (e.g., ["CLAS", "INTF"]). Optional |
| `name_pattern` | String | Name pattern with wildcards (e.g., "ZCL_*"). Optional |
| `limit` | Integer | Maximum results (default: 100, max: 1000) |
| `offset` | Integer | Offset for pagination (default: 0) |

### Supported Object Types

CLAS, INTF, PROG, FUGR, TABL, STRU, DTEL, TTYP, DDLS, DDLX, XSLT, REPO, SUSH

### Response (success)

```json
{
  "success": true,
  "command": "LIST",
  "package": "$MY_PACKAGE",
  "message": "Objects retrieved successfully",
  "objects": [
    {
      "object": "CLAS",
      "obj_name": "ZCL_MY_CLASS",
      "description": "My Class"
    },
    {
      "object": "INTF",
      "obj_name": "ZIF_MY_INTERFACE",
      "description": "My Interface"
    }
  ],
  "total": 25,
  "limit": 50,
  "offset": 0,
  "error": ""
}
```

### Response (error)

```json
{
  "success": false,
  "command": "LIST",
  "package": "$NONEXISTENT",
  "error": "Package $NONEXISTENT does not exist"
}
```

## POST /view

View ABAP object definitions directly from ABAP system.

### Request Body

```json
{
  "objects": ["ZCL_MY_CLASS", "ZIF_MY_INTERFACE", "SFLIGHT"],
  "type": "CLAS",
  "full": false
}
```

| Field | Type | Description |
|-------|------|-------------|
| `objects` | Array | List of object names (required) |
| `type` | String | Object type (CLAS, INTF, TABL, STRU, DTEL, TTYP, DDLS). Auto-detected if not specified |
| `full` | Boolean | Return all source sections instead of public section only. For CLAS: returns `sections[]` with CU/CO/CP/CM*/CCDEF/CCIMP/CCAU includes. For INTF/PROG/DDLS: returns single-entry `sections[]`. Default: `false` |

### Supported Object Types

| Type | Description |
|------|-------------|
| CLAS | Global ABAP class |
| INTF | Global interface |
| TABL | Database table |
| STRU | Structure type |
| DTEL | Data element |
| TTYP | Table type |
| DDLS | CDS View/Entity |

### Response (success - class/interface)

```json
{
  "success": true,
  "command": "VIEW",
  "message": "Retrieved object(s)",
  "objects": [
    {
      "name": "ZCL_MY_CLASS",
      "type": "CLAS",
      "type_text": "Class",
      "description": "Class ZCL_MY_CLASS in $PACKAGE",
      "source": "CLASS zcl_my_class DEFINITION PUBLIC.\n  PUBLIC SECTION.\n  ...",
      "not_found": false,
      "components": []
    }
  ],
  "summary": {
    "total": 1,
    "by_type": ["CLAS"]
  },
  "error": ""
}
```

### Response (success - class with `full: true`)

When `full: true` is set, `source` is replaced by a `sections` array. Each entry represents one ABAP include. Line number rendering is done client-side by Node.js.

```json
{
  "success": true,
  "command": "VIEW",
  "message": "Retrieved object(s)",
  "objects": [
    {
      "name": "ZCL_MY_CLASS",
      "type": "CLAS",
      "type_text": "Class",
      "description": "Class ZCL_MY_CLASS in $PACKAGE",
      "not_found": false,
      "components": [],
      "sections": [
        {
          "suffix": "CU",
          "description": "Public Section",
          "method_name": "",
          "file": "",
          "lines": ["CLASS zcl_my_class DEFINITION PUBLIC FINAL CREATE PUBLIC.", "  PUBLIC SECTION.", "  ..."]
        },
        {
          "suffix": "CO",
          "description": "Protected Section",
          "method_name": "",
          "file": "",
          "lines": ["  PROTECTED SECTION."]
        },
        {
          "suffix": "CP",
          "description": "Private Section",
          "method_name": "",
          "file": "",
          "lines": ["  PRIVATE SECTION.", "    DATA mv_host TYPE string."]
        },
        {
          "suffix": "CM001",
          "description": "Class Method",
          "method_name": "CONSTRUCTOR",
          "file": "",
          "lines": ["METHOD constructor.", "  mv_host = iv_host.", "ENDMETHOD."]
        },
        {
          "suffix": "CCDEF",
          "description": "Local Definitions",
          "method_name": "",
          "file": "locals_def",
          "lines": ["CLASS lcl_helper DEFINITION.", "  ..."]
        }
      ]
    }
  ],
  "summary": {
    "total": 1,
    "by_type": ["CLAS"]
  },
  "error": ""
}
```

| Section field | Description |
|---|---|
| `suffix` | Include type: `CU` (public), `CO` (protected), `CP` (private), `CM001`…`CMxxx` (method implementations), `CCDEF` (local defs), `CCIMP` (local impl), `CCAU` (unit tests) |
| `description` | Human-readable label |
| `method_name` | Method name for CM* sections; empty for others |
| `file` | Non-empty for sections that come from a separate git file (`locals_def`, `locals_imp`, `testclasses`) |
| `lines` | Source lines of this include (no line numbers — rendering is client-side) |

### Response (success - table)

```json
{
  "success": true,
  "command": "VIEW",
  "message": "Retrieved object(s)",
  "objects": [
    {
      "name": "SFLIGHT",
      "type": "TABL",
      "type_text": "Table",
      "description": "Table SFLIGHT in SAPBC_DATAMODEL",
      "source": "",
      "not_found": false,
      "components": [
        {
          "field": "MANDT",
          "key": true,
          "type": "CLNT",
          "length": 3,
          "dataelement": "MANDT",
          "description": "Client"
        },
        {
          "field": "CARRID",
          "key": true,
          "type": "CHAR",
          "length": 3,
          "dataelement": "S_CARR_ID",
          "description": "Airline Code"
        }
      ]
    }
  ],
  "summary": {
    "total": 1,
    "by_type": ["TABL"]
  },
  "error": ""
}
```

### Response (success - data element)

```json
{
  "success": true,
  "command": "VIEW",
  "message": "Retrieved object(s)",
  "objects": [
    {
      "name": "S_CARR_ID",
      "type": "DTEL",
      "type_text": "Data Element",
      "description": "Airline Code",
      "domain": "S_CARR_ID",
      "domain_type": "CHAR",
      "domain_length": 3,
      "domain_decimals": 0,
      "not_found": false,
      "components": []
    }
  ],
  "summary": {
    "total": 1,
    "by_type": ["DTEL"]
  },
  "error": ""
}
```

### Response (not found)

```json
{
  "success": true,
  "command": "VIEW",
  "message": "Retrieved object(s)",
  "objects": [
    {
      "name": "ZIF_NONEXISTENT",
      "type": "",
      "type_text": "Unknown",
      "not_found": true,
      "components": []
    }
  ],
  "summary": {
    "total": 1,
    "by_type": [""]
  },
  "error": ""
}
```

## POST /preview

Preview data from ABAP tables or CDS views directly from the ABAP system. This is useful for exploring table/view contents without writing queries.

### Request Body

```json
{
  "objects": ["SFLIGHT", "ZC_MY_CDS_VIEW"],
  "type": "TABL",
  "limit": 10,
  "where": "CARRID = 'AA'",
  "columns": ["CARRID", "CONNID", "PRICE"]
}
```

| Field | Type | Description |
|-------|------|-------------|
| `objects` | Array | List of table/view names (required) |
| `type` | String | Object type (TABL, DDLS). Auto-detected if not specified |
| `limit` | Integer | Maximum rows to return (default: 10, max: 100) |
| `where` | String | WHERE clause filter (e.g., `CARRID = 'AA'`) |
| `columns` | Array | Column names to display (optional) |

### Supported Object Types

| Type | Description |
|------|-------------|
| TABL | Database table |
| DDLS | CDS View/Entity |

### Auto-Detection Rules

If `type` is not specified, the system detects the type from TADIR:
- CDS views (DDLS) are preferred if found in TADIR
- Otherwise defaults to table (TABL)

### Response (success - table)

```json
{
  "success": true,
  "command": "PREVIEW",
  "message": "Retrieved data",
  "objects": [
    {
      "name": "SFLIGHT",
      "type": "TABL",
      "type_text": "Table",
      "row_count": 5,
      "total_rows": 10,
      "rows": [
        {
          "MANDT": "100",
          "CARRID": "AA",
          "CONNID": 17,
          "FLDATE": "2024-10-24",
          "PRICE": 422.94,
          "CURRENCY": "USD",
          "PLANETYPE": "747-400"
        }
      ],
      "fields": [
        { "field": "MANDT", "type": "CLNT", "length": 3 },
        { "field": "CARRID", "type": "CHAR", "length": 3 },
        { "field": "CONNID", "type": "NUMC", "length": 4 },
        { "field": "FLDATE", "type": "DATS", "length": 8 },
        { "field": "PRICE", "type": "CURR", "length": 16, "decimals": 2 },
        { "field": "CURRENCY", "type": "CUKY", "length": 5 }
      ],
      "columns_displayed": 6,
      "columns_hidden": ["SEATSMAX", "SEATSOCC", "PAYMENTSUM"],
      "error": ""
    }
  ],
  "summary": {
    "total_objects": 1,
    "total_rows": 5
  },
  "error": ""
}
```

### Response (success - CDS view)

```json
{
  "success": true,
  "command": "PREVIEW",
  "message": "Retrieved data",
  "objects": [
    {
      "name": "ZC_MY_CDS_VIEW",
      "type": "DDLS",
      "type_text": "CDS View",
      "row_count": 10,
      "total_rows": 25,
      "rows": [
        {
          "PACKAGE": "ZMY_PACKAGE",
          "DESCRIPTION": "My Package",
          "PARENT": "$ZROOT"
        }
      ],
      "fields": [
        { "field": "PACKAGE", "type": "CHAR", "length": 30 },
        { "field": "DESCRIPTION", "type": "CHAR", "length": 60 },
        { "field": "PARENT", "type": "CHAR", "length": 30 }
      ],
      "columns_displayed": 3,
      "columns_hidden": [],
      "error": ""
    }
  ],
  "summary": {
    "total_objects": 1,
    "total_rows": 10
  },
  "error": ""
}
```

### Response (error - not found)

```json
{
  "success": true,
  "command": "PREVIEW",
  "message": "Retrieved data",
  "objects": [
    {
      "name": "Z_NONEXISTENT",
      "type": "TABL",
      "type_text": "Table",
      "row_count": 0,
      "total_rows": 0,
      "rows": [],
      "fields": [],
      "columns_displayed": 0,
      "columns_hidden": [],
      "error": "Table or view not found: Z_NONEXISTENT"
    }
  ],
  "summary": {
    "total_objects": 1,
    "total_rows": 0
  },
  "error": ""
}
```

## POST /where

Find where-used list for ABAP objects (classes, interfaces, programs).

### Request Body

```json
{
  "objects": ["ZCL_SUT_AUNIT_RUNNER", "ZIF_MY_INTERFACE"],
  "type": "CLAS",
  "limit": 50
}
```

| Field | Type | Description |
|-------|------|-------------|
| `objects` | Array | List of object names (required) |
| `type` | String | Object type (CLAS, INTF, PROG). Auto-detected if not specified |
| `limit` | Integer | Maximum results per object (default: 50, max: 200) |

### Supported Object Types

| Type | Description |
|------|-------------|
| CLAS | Global ABAP class |
| INTF | Global interface |
| PROG | ABAP program |

### Response (success)

```json
{
  "success": true,
  "command": "WHERE",
  "message": "Retrieved where-used list",
  "objects": [
    {
      "name": "ZCL_SUT_AUNIT_RUNNER",
      "type": "CLAS",
      "type_text": "Class",
      "references": [
        {
          "object": "PROG",
          "obj_name": "RS_AUNIT_DISPLAY",
          "include": "RS_AUNIT_DISPLAY",
          "line": 10,
          "program": "RS_AUNIT_DISPLAY"
        }
      ],
      "count": 5,
      "not_found": false,
      "error": ""
    },
    {
      "name": "ZIF_MY_INTERFACE",
      "type": "INTF",
      "type_text": "Interface",
      "references": [
        {
          "object": "CLAS",
          "obj_name": "ZCL_MY_CLASS",
          "include": "ZCL_MY_CLASS========CM001",
          "line": 5,
          "program": "ZCL_MY_CLASS"
        }
      ],
      "count": 3,
      "not_found": false,
      "error": ""
    }
  ],
  "summary": {
    "total": 2,
    "total_references": 8
  },
  "error": ""
}
```

### Response (object not found)

```json
{
  "success": true,
  "command": "WHERE",
  "message": "Retrieved where-used list",
  "objects": [
    {
      "name": "ZIF_NONEXISTENT",
      "type": "",
      "type_text": "Unknown",
      "references": [],
      "count": 0,
      "not_found": true,
      "error": ""
    }
  ],
  "summary": {
    "total": 1,
    "total_references": 0
  },
  "error": ""
}
```

### Response Structure

### Pull Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | String | 'X' for success, '' for errors |
| `message` | String | Status message |
| `error_detail` | String | Error details (if any) |
| `activated_count` | Integer | Number of activated objects |
| `failed_count` | Integer | Number of failed object entries |
| `started_at` | Timestamp | Start time of operation |
| `finished_at` | Timestamp | End time of operation |
| `log_messages` | Array | All log messages |
| `activated_objects` | Array | Unique successfully activated objects |
| `failed_objects` | Array | All error log entries |

### Syntax Check Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | String | 'X' for no errors, '' for errors |
| `object_type` | String | ABAP object type (e.g., 'CLAS', 'PROG') |
| `object_name` | String | ABAP object name |
| `error_count` | Integer | Number of syntax errors found |
| `errors` | Array | List of errors with line, column, text |

### Unit Test Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | String | 'X' for all tests passed, '' for failures |
| `test_count` | Integer | Total number of tests |
| `passed_count` | Integer | Number of passed tests |
| `failed_count` | Integer | Number of failed tests |
| `message` | String | Status message |
| `errors` | Array | Failed test details (empty if all tests pass) |

### Tree Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | Boolean | Whether the request succeeded |
| `command` | String | Command name ("TREE") |
| `package` | String | Root package name |
| `message` | String | Status message |
| `parent_package` | String | Parent package (empty if root) |
| `nodes` | Array | Flat list of all packages |
| `total_packages` | Integer | Total packages in tree |
| `total_objects` | Integer | Total objects in tree |
| `objects` | Array | Object counts by type |
| `error` | String | Error message (empty if success) |

### View Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | Boolean | Whether the request succeeded |
| `command` | String | Command name ("VIEW") |
| `message` | String | Status message |
| `objects` | Array | List of object information |
| `summary` | Object | Summary with total and by_type |
| `error` | String | Error message (empty if success) |

### Preview Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | Boolean | Whether the request succeeded |
| `command` | String | Command name ("PREVIEW") |
| `message` | String | Status message |
| `objects` | Array | List of table/view results |
| `summary` | Object | Summary with total_objects and total_rows |
| `error` | String | Error message (empty if success) |

### Preview Object Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | String | Table/view name |
| `type` | String | Object type (TABL, DDLS) |
| `type_text` | String | Human-readable type (Table, CDS View) |
| `row_count` | Integer | Number of rows returned |
| `total_rows` | Integer | Total rows available (before limit) |
| `rows` | Array | Array of row objects with field:value pairs |
| `fields` | Array | Field metadata (field, type, length, decimals) |
| `columns_displayed` | Integer | Number of columns in output |
| `columns_hidden` | Array | Column names not displayed (if limited) |
| `error` | String | Error message (empty if success) |

### Object Fields (for View)

| Field | Type | Description |
|-------|------|-------------|
| `name` | String | Object name |
| `type` | String | Object type (CLAS, INTF, TABL, STRU, DTEL, TTYP, DDLS) |
| `type_text` | String | Human-readable type |
| `description` | String | Object description |
| `source` | String | Source code (CLAS/INTF/DDLS) |
| `domain` | String | Domain name (DTEL) |
| `domain_type` | String | Domain data type (DTEL) |
| `domain_length` | Integer | Domain length (DTEL) |
| `domain_decimals` | Integer | Domain decimals (DTEL) |
| `not_found` | Boolean | true if object does not exist |
| `components` | Array | Fields/components (TABL/STRU/TTYP) |

### List Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | Boolean | Whether the request succeeded |
| `command` | String | Command name ("LIST") |
| `package` | String | Package name |
| `message` | String | Status message |
| `objects` | Array | List of ABAP objects |
| `total` | Integer | Total number of objects matching criteria |
| `limit` | Integer | Limit used |
| `offset` | Integer | Offset used |
| `error` | String | Error message (empty if success) |

### Where Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | Boolean | |
| `command Whether the request succeeded` | String | Command name ("WHERE") |
| `message` | String | Status message |
| `objects` | Array | List of objects with where-used references |
| `summary` | Object | Summary with total and total_references |
| `error` | String | Error message (empty if success) |

### Where Object Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | String | Object name |
| `type` | String | Object type (CLAS, INTF, PROG) |
| `type_text` | String | Human-readable type |
| `references` | Array | List of where-used references |
| `count` | Integer | Number of references found |
| `not_found` | Boolean | true if object does not exist |
| `error` | String | Error message (empty if success) |

### Reference Fields

| Field | Type | Description |
|-------|------|-------------|
| `object` | String | Object type of reference |
| `obj_name` | String | Object name of reference |
| `include` | String | Include name |
| `line` | Integer | Line number |
| `program` | String | Program name |

### Error Item Fields

| Field | Type | Description |
|-------|------|-------------|
| `class_name` | String | Test class name |
| `method_name` | String | Failed test method name |
| `error_kind` | String | Error type (e.g., 'ERROR', 'FAILURE') |
| `error_text` | String | Detailed error message from AUnit |
