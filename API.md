# REST API Reference

The ABAP system exposes these endpoints via SICF handler: `sap/bc/z_abapgit_agent`

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check (also fetches CSRF token) |
| POST | `/pull` | Pull and activate repository |
| POST | `/inspect` | Inspect source file for issues |
| POST | `/unit` | Execute unit tests (AUnit) |
| POST | `/tree` | Display package hierarchy tree |
| POST | `/view` | View ABAP object definitions |
| POST | `/preview` | Preview table/CDS view data |

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

## POST /view

View ABAP object definitions directly from ABAP system.

### Request Body

```json
{
  "objects": ["ZCL_MY_CLASS", "ZIF_MY_INTERFACE", "SFLIGHT"],
  "type": "CLAS"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `objects` | Array | List of object names (required) |
| `type` | String | Object type (CLAS, INTF, TABL, STRU, DTEL). Auto-detected if not specified |

### Supported Object Types

| Type | Description |
|------|-------------|
| CLAS | Global ABAP class |
| INTF | Global interface |
| TABL | Database table |
| STRU | Structure type |
| DTEL | Data element |

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

### Response Structure

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
| `type` | String | Object type (CLAS, INTF, TABL, STRU, DTEL) |
| `type_text` | String | Human-readable type |
| `description` | String | Object description |
| `source` | String | Source code (CLAS/INTF) |
| `domain` | String | Domain name (DTEL) |
| `domain_type` | String | Domain data type (DTEL) |
| `domain_length` | Integer | Domain length (DTEL) |
| `domain_decimals` | Integer | Domain decimals (DTEL) |
| `not_found` | Boolean | true if object does not exist |
| `components` | Array | Fields/components (TABL/STRU) |

### Error Item Fields

| Field | Type | Description |
|-------|------|-------------|
| `class_name` | String | Test class name |
| `method_name` | String | Failed test method name |
| `error_kind` | String | Error type (e.g., 'ERROR', 'FAILURE') |
| `error_text` | String | Detailed error message from AUnit |
