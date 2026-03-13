---
layout: default
title: pull - Pull & Activate
nav_order: 1
parent: Development Commands
has_children: true
---

# pull Command Requirements

## Overview

Pull and activate ABAP objects from git repository.

## Command

```bash
# Auto-detect git remote and branch from current directory
abapgit-agent pull

# Pull specific files only (fast)
abapgit-agent pull --files src/zcl_my_class.clas.abap,src/zcl_other.clas.abap

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

## Project-Level Safeguards

Project maintainers can configure safeguards and conflict detection defaults in `.abapgit-agent.json` (checked into repository):

```json
{
  "safeguards": {
    "requireFilesForPull": true,
    "disablePull": false,
    "reason": "Large project with 500+ objects"
  },

  "conflictDetection": {
    "mode": "ignore",
    "reason": "Single-developer project"
  }
}
```

**When `requireFilesForPull: true`:**
- `abapgit-agent pull` → ❌ Error: --files is required
- `abapgit-agent pull --files ...` → ✅ Works

**When `disablePull: true`:**
- All `pull` commands are disabled
- Error message directs users to project maintainer

**`conflictDetection.mode`:**
- `"abort"` (default) — pull aborts when conflicts are detected
- `"ignore"` — conflict detection disabled project-wide

CLI `--conflict-mode` flag always takes precedence over project config.

**Example Error Message:**
```
❌ Error: --files parameter is required for this project

Reason: Large project with 500+ objects

Usage: abapgit-agent pull --files <file1>,<file2>

This safeguard is configured in .abapgit-agent.json
Contact the project maintainer if you need to change this setting.
```

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--url` | No | Git repository URL (auto-detected if not specified) |
| `--branch` | No | Branch name (default: current branch) |
| `--files` | No | Comma-separated list of files to pull |
| `--transport` | No | Transport request (config/env takes priority if not specified) |
| `--conflict-mode` | No | `abort` (default) or `ignore` — see [Conflict Detection](pull-conflict-detection.md) |

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
  "transport_request": "DEVK900001",
  "conflict_mode": "abort"
}
```

### 6. Display Results

---

## Output

### Success

```
✅ Pull completed successfully!
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
  "message": "Pull completed successfully",
  "activated_count": 15,
  "failed_count": 1,
  "activated_objects": [
    { "obj_type": "CLAS", "obj_name": "ZCL_MY_CLASS" }
  ],
  "failed_objects": [
    { "obj_type": "CLAS", "obj_name": "ZCL_OTHER", "text": "Error message" }
  ],
  "log_messages": [...],
  "conflict_report": "",
  "conflict_count": 0
}
```

---

## Key Behaviors

1. **Activated Objects** - Only includes objects that completed successfully (no errors in log)
2. **Failed Objects Log** - Shows all error messages (duplicates allowed for multiple errors per object)
3. **Error Details** - When errors occur, displays error detail section at the top
4. **Conflict Detection** - Enabled by default (`abort` mode); aborts pull if local ADT edits or branch divergence detected — see [Conflict Detection](pull-conflict-detection.md)
5. **Partial Download** - `--files` performance depends on the abapGit version installed — see [Partial Download](pull-partial-download.md)

---

## Example

```bash
# Full pull
abapgit-agent pull

# Fast pull - specific files
abapgit-agent pull --files src/zcl_my_class.clas.abap

# With transport
abapgit-agent pull --files src/zcl_my_class.clas.abap --transport DEVK900001

# Force pull through a conflict (e.g. deliberate branch switch)
abapgit-agent pull --files src/zcl_my_class.clas.abap --conflict-mode ignore
```
