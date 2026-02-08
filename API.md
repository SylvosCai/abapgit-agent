# REST API Reference

The ABAP system exposes these endpoints via SICF handler: `sap/bc/z_abapgit_agent`

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check (also fetches CSRF token) |
| POST | `/pull` | Pull and activate repository |
| POST | `/syntax-check` | Check syntax of specific ABAP object |
| POST | `/unit` | Execute unit tests (planned) |

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
  -d '{"url": "https://github.tools.sap/user/repo.git", "branch": "main"}'
```

### Request Body

```json
{
  "url": "https://github.tools.sap/user/repo.git",
  "branch": "main",
  "username": "git-username",
  "password": "git-token"
}
```

### Response (success)

```json
{
  "success": "X",
  "job_id": "USER123_20260206_120000",
  "message": "Pull completed successfully"
}
```

### Response (with activation errors)

```json
{
  "success": "",
  "job_id": "USER123_20260206_120000",
  "message": "Pull completed with activation errors",
  "error_detail": "Errors/Warnings:\n  - CLAS ZCL_TEST_CLASS: syntax error"
}
```

## POST /syntax-check

Check syntax of a specific ABAP object.

```bash
curl -X POST "https://your-system:44300/sap/bc/z_abapgit_agent/syntax-check" \
  -H "Content-Type: application/json" \
  -H "sap-client: 100" \
  -H "X-CSRF-Token: $CSRF" \
  -b cookies.txt \
  -u USER:PASSWORD \
  -d '{"object_type": "CLAS", "object_name": "ZCL_MY_CLASS"}'
```

### Request Body

```json
{
  "object_type": "CLAS",
  "object_name": "ZCL_MY_CLASS"
}
```

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

## Response Structure

### Pull Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | String | 'X' for success, '' for errors |
| `job_id` | String | Job identifier |
| `message` | String | Status message |
| `error_detail` | String | Error details (if any) |

### Syntax Check Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `success` | String | 'X' for no errors, '' for errors |
| `object_type` | String | ABAP object type (e.g., 'CLAS', 'PROG') |
| `object_name` | String | ABAP object name |
| `error_count` | Integer | Number of syntax errors found |
| `errors` | Array | List of errors with line, column, text |
