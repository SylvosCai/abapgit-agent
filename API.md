# REST API Reference

The ABAP system exposes these endpoints via SICF handler: `sap/bc/z_abapgit_agent`

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check (also fetches CSRF token) |
| POST | `/pull` | Pull and activate repository |
| POST | `/inspect` | Inspect source file for issues |
| POST | `/unit` | Execute unit tests (AUnit) |

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
  "job_id": "CAIS20260208115649",
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
  "job_id": "CAIS20260209041349",
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

## POST /inspect

Inspect source file for issues (currently runs syntax check via Code Inspector).

### Request Body

```json
{
  "source_name": "ZCL_MY_CLASS.CLASS.ABAP"
}
```

The endpoint parses the file name to extract `obj_type` and `obj_name`:
- `zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS
- `src/zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS

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

## Response Structure

### Pull Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | String | 'X' for success, '' for errors |
| `job_id` | String | Job identifier |
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

### Error Item Fields

| Field | Type | Description |
|-------|------|-------------|
| `class_name` | String | Test class name |
| `method_name` | String | Failed test method name |
| `error_kind` | String | Error type (e.g., 'ERROR', 'FAILURE') |
| `error_text` | String | Detailed error message from AUnit |
