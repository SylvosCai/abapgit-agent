# pull Command Requirements

## Overview

Pull and activate ABAP objects from git repository.

## Command

```bash
# Auto-detect git remote and branch from current directory
abapgit-agent pull

# Pull specific files only (fast)
abapgit-agent pull --files zcl_my_class.clas.abap,zcl_other.clas.abap

# Pull from specific branch
abapgit-agent pull --branch develop

# Pull from specific URL
abapgit-agent pull --url https://github.com/org/repo.git

# With transport request
abapgit-agent pull --transport DEVK900001

# Combined options
abapgit-agent pull --branch develop --files src/zcl_my_class.clas.abap --transport DEVK900001

# Using transport from config/environment (no --transport flag needed)
abapgit-agent pull
```

## Prerequisite

- Current folder is git repo root (unless `--url` specified)
- Git remote is configured (unless `--url` specified)
- `.abapGitAgent` exists with valid credentials

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--url` | No | Git repository URL (auto-detected if not specified) |
| `--branch` | No | Branch name (default: current branch) |
| `--files` | No | Comma-separated list of files to pull |
| `--transport` | No | Transport request (config/env takes priority if not specified) |

## Transport Request Precedence

The transport request is determined in this order:

| Priority | Source | Example |
|----------|--------|---------|
| 1 | CLI `--transport` argument | `--transport DEVK900001` |
| 2 | Config file `transport` | `"transport": "DEVK900001"` in `.abapGitAgent` |
| 3 | Environment variable `ABAP_TRANSPORT` | `export ABAP_TRANSPORT="DEVK900001"` |
| 4 (default) | Not set | abapGit creates/uses default |

---

## Tasks

### 1. Detect Git Remote URL

```bash
git remote get-url origin
```

### 2. Detect Current Branch

```bash
git branch --show-current
```

Or from `.git/HEAD`

### 3. Load Configuration

Read `.abapGitAgent` for credentials

### 4. Fetch CSRF Token

```bash
GET /health (with X-CSRF-Token: fetch)
```

### 5. Make Pull Request

**Endpoint:** `POST /pull`

**Request Body:**
```json
{
  "url": "https://github.com/org/repo.git",
  "branch": "main",
  "username": "git-user",
  "password": "git-token",
  "files": ["zcl_my_class.clas.abap"],
  "transport_request": "DEVK900001"
}
```

### 6. Display Results

---

## Output

### Success

```
✅ Pull completed successfully!
   Job ID: CAIS20260208115649
   Message: Pull completed successfully

📋 Pull Log (N messages):
────────────────────────────────────────────────────────────────────────────────
Icon │Object                      │Message
─────┼────────────────────────────┼─────────────────────────────────────────
✅   │CLAS ZCL_MY_CLASS          │Object ZCL_MY_CLASS imported
...

📦 Activated Objects (N):
────────────────────────────────────────────────────────────────────────────────
✅ CLAS ZCL_MY_CLASS
...
```

### With Errors

```
❌ Pull completed with errors!
   Job ID: CAIS20260208115649
   Message: Pull completed with errors

📋 Error Details:
────────────────────────────────────────────────────────────────────────────────
CLAS ZCL_MY_CLASS: Error updating where-used list

📋 Pull Log (N messages):
...

❌ Failed Objects Log (M entries):
────────────────────────────────────────────────────────────────────────────────
❌ CLAS ZCL_MY_CLASS: Error message text
Exception: Exception details
```

---

## File Format

Files are parsed to extract `(obj_type, obj_name)`:

| File Pattern | Object Type | Object Name |
|--------------|-------------|-------------|
| `zcl_my_class.clas.abap` | CLAS | ZCL_MY_CLASS |
| `zif_my_intf.intf.abap` | INTF | ZIF_MY_INTF |
| `src/zcl_my_class.clas.abap` | CLAS | ZCL_MY_CLASS |

---

## Response Structure

```json
{
  "success": "X",
  "job_id": "CAIS20260208115649",
  "message": "Pull completed successfully",
  "activated_count": 15,
  "failed_count": 1,
  "activated_objects": [
    { "obj_type": "CLAS", "obj_name": "ZCL_MY_CLASS" }
  ],
  "failed_objects": [
    { "obj_type": "CLAS", "obj_name": "ZCL_OTHER", "text": "Error message" }
  ],
  "log_messages": [...]
}
```

---

## Key Behaviors

1. **Activated Objects** - Only includes objects that completed successfully (no errors in log)
2. **Failed Objects Log** - Shows all error messages (duplicates allowed for multiple errors per object)
3. **Error Details** - When errors occur, displays error detail section at the top

---

## Example

```bash
# Full pull
abapgit-agent pull

# Fast pull - specific files
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# With transport
abapgit-agent pull --files abap/zcl_my_class.clas.abap --transport DEVK900001
```
