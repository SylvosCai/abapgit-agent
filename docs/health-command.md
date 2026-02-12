# health Command Requirements

## Overview

Check if the ABAP REST API is healthy and accessible.

## Command

```bash
abapgit-agent health
```

## Prerequisite

- `.abapGitAgent` file exists with valid credentials, or environment variables are set

## Tasks

### 1. Load Configuration

Read from `.abapGitAgent` or environment variables:
- `host`
- `sapport`
- `client`
- `user`
- `password`
- `language`

### 2. Make Health Check Request

**Endpoint:** `GET /health`

**Response:**
```json
{
  "status": "healthy",
  "abap": "connected",
  "version": "1.0.0"
}
```

### 3. Display Result

```bash
$ abapgit-agent health
{
  "status": "healthy",
  "abap": "connected",
  "version": "1.0.0"
}
```

---

## Output

| Status | Output |
|--------|--------|
| Healthy | JSON response with status |
| Error | Error message with details |

---

## Error Handling

| Error | Message |
|-------|---------|
| Connection failed | `Health check failed: <error message>` |
| Invalid credentials | Authentication error |

---

## Example

```bash
# Check health
abapgit-agent health

# Output
{
  "status": "healthy",
  "abap": "connected",
  "version": "1.0.0"
}
```

## Used By

- `create` command - validates ABAP connection before creating repo
