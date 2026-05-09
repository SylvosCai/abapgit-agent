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

# Pull enhancement (ENHO) object by XML metadata file
abapgit-agent pull --files src/zfoo.enho.xml

# Pull enhancement (ENHO) object by hash source file — activates whole ENHO
abapgit-agent pull --files src/zfoo.enho.28bbfe2f.abap

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

# Sync serializer XML to git after pull (see XML Sync below)
abapgit-agent pull --files src/zcl_my_class.clas.abap --sync-xml
```

## Prerequisite

- Current folder is git repo root (unless `--url` specified)
- Git remote is configured (unless `--url` specified)
- `.abapGitAgent` exists with valid credentials

## Project-Level Safeguards

Project maintainers can configure safeguards and conflict detection defaults in `.abapgit-agent.json` (checked into repository). See [Project Configuration reference](../project-config.md) for the full schema.

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
| `--sync-xml` | No | Accept serializer XML output and amend last commit (see [XML Sync](#xml-sync)) |

## Transport Request Precedence

The transport request is determined in this order:

| Priority | Source | Example |
|----------|--------|---------|
| 1 | CLI `--transport` argument | `--transport DEVK900001` |
| 2 | Config file `transport` | `"transport": "DEVK900001"` in `.abapGitAgent` |
| 3 | Environment variable `ABAP_TRANSPORT` | `export ABAP_TRANSPORT="DEVK900001"` |
| 4 (default) | Not set | abapGit creates/uses default |

---

## Async Execution (Large Pulls)

When pulling **more than 10 files**, the ABAP backend automatically runs the pull as a background job to avoid HTTP timeouts. The Node.js CLI handles this transparently — the user experience is identical to a synchronous pull, but with a real-time progress bar during activation.

### How it works

1. Node.js POSTs the pull request as usual
2. ABAP detects >10 files → schedules a background job (`ABGAGT_PULL_<timestamp>`) → returns **HTTP 202 Accepted** with a job number
3. Node.js detects 202, switches to polling mode: `GET /pull?jobNumber=<N>` every 2 seconds
4. Progress bar updates in real time as ABAP reports activation milestones
5. When the job completes, Node.js reads the final result and displays the same output as a synchronous pull

### Progress stages

| Stage | Progress | Message |
|-------|----------|---------|
| INITIALIZATION | 10% | Initializing pull... |
| FIND_REPO | 30% | Finding repository... |
| ACTIVATE | 50–90% | Activating objects (N of total) |
| COMPLETE | 100% | Pull completed |

### Output during async pull

```
  Pulling from https://github.com/org/repo.git (branch: main)
  Activating 47 file(s) as background job...

  [====================          ] Activating objects (23 of 47)

✅ Pull completed successfully!
   Message: Pull completed successfully

📦 Activated Objects (47):
...
```

### HTTP 202 response (job scheduled)

```json
{
  "success": "X",
  "jobName": "ABGAGT_PULL_20260509082234",
  "jobNumber": "08223400",
  "status": "scheduled",
  "message": "Pull job started"
}
```

### Polling response (job running)

```json
{
  "jobNumber": "08223400",
  "status": "running",
  "stage": "ACTIVATE",
  "message": "Activating objects (23 of 47)",
  "progress": 65,
  "current": 23,
  "total": 47
}
```

---

## XML Sync

After a successful pull, abapGit's serializer may produce slightly different XML than the hand-crafted metadata files in git (different field presence, field order, BOM, extra sections). This causes abapGit to show the object as **M (modified)** even though the code itself was not changed.

The pull command detects this automatically by comparing the serializer output (`get_files_local()`) against the git remote files (`get_files_remote()`) byte-for-byte in ABAP — only files where bytes actually differ are reported.

### Default (no flag): report only

```
⚠️  1 XML file(s) differ from serializer output:
   src/zif_my_intf.intf.xml
   Run with --sync-xml to accept serializer output and amend the last commit
```

### `--sync-xml`: accept + amend + re-pull

```bash
abapgit-agent pull --files src/zif_my_intf.intf.abap --sync-xml
```

When `--sync-xml` is passed:

1. Writes the serializer XML bytes to local files (overwrite)
2. Stages the changed XML files: `git add <files>`
3. Amends the last commit: `git commit --amend --no-edit`
4. Pushes: `git push --force-with-lease` (sets upstream automatically if not yet set)
5. Re-pulls the same files so the ABAP system matches the amended commit

```
✅ Pull completed successfully!
   ...

🔄 Syncing 1 XML file(s) to match serializer output:
   src/zif_my_intf.intf.xml
   Re-pulling so ABAP system matches the amended commit...
   ...

✅ Synced 1 XML file(s), amended commit, re-pulled
```

### Notes

- Only XML metadata files (`.xml`) are written back — ABAP source files are untouched
- Only files that **already exist on disk** are overwritten (no new files are created)
- When `--files` is used, only XML files for the specified objects are compared
- AI agents should always pass `--sync-xml` after creating or modifying ABAP objects to keep git and the ABAP system in sync

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

### 6. Handle Response

**If HTTP 200 (synchronous — ≤10 files):** parse result and display directly.

**If HTTP 202 (async — >10 files):**
1. Read `jobNumber` from response body
2. Print `Activating N file(s) as background job...`
3. Poll `GET /pull?jobNumber=<N>` every 2 seconds
4. Update progress bar on each poll using `progress`, `message`, `current`, `total`
5. When `status = "completed"`: parse `result` (JSON string) and display as normal
6. When `status = "error"`: print error and exit 1

### 7. Display Results

---

## Output

### Success (synchronous)

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

### Success (async — >10 files)

```
  Pulling from https://github.com/org/repo.git (branch: main)
  Activating 47 file(s) as background job...

  [====================          ] Activating objects (23 of 47)

✅ Pull completed successfully!
   Message: Pull completed successfully

📋 Pull Log (N messages):
...

📦 Activated Objects (47):
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
| `ztable.tabl.xml` | TABL | ZTABLE |
| `zdtel.dtel.xml` | DTEL | ZDTEL |
| `zstru.stru.xml` | STRU | ZSTRU |
| `src/ztable.tabl.xml` | TABL | ZTABLE |

---

## Response Structure

### Synchronous (HTTP 200)

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
  "conflict_count": 0,
  "local_xml_files": [
    { "filename": "zcl_my_class.clas.xml", "path": "/src/", "data": "<base64>" }
  ]
}
```

`local_xml_files` contains only XML files where the serializer output differs from the git remote — empty array means all XML files are in sync.

### Async job scheduled (HTTP 202)

```json
{
  "success": "X",
  "jobName": "ABGAGT_PULL_20260509082234",
  "jobNumber": "08223400",
  "status": "scheduled",
  "message": "Pull job started"
}
```

### Async job completed (GET poll — final)

```json
{
  "jobNumber": "08223400",
  "status": "completed",
  "progress": 100,
  "result": "{...same structure as synchronous response...}"
}
```

---

## Key Behaviors

1. **Activated Objects** - Only includes objects that completed successfully (no errors in log)
2. **Failed Objects Log** - Shows all error messages (duplicates allowed for multiple errors per object)
3. **Error Details** - When errors occur, displays error detail section at the top
4. **Conflict Detection** - Enabled by default (`abort` mode); aborts pull if local ADT edits or branch divergence detected — see [Conflict Detection](pull-conflict-detection.md)
5. **Partial Download** - `--files` performance depends on the abapGit version installed — see [Partial Download](pull-partial-download.md)
6. **Missing `.abapgit.xml` warning** - If the repository root contains no `.abapgit.xml`, pull prints a warning on stderr. Without this file, abapGit may use an incorrect `STARTING_FOLDER` from ABAP-side persistence, causing `ACTIVATED_COUNT=0` with an empty log. In `--json` mode the result object includes `"missing_abapgit_xml": true` instead. Run `abapgit-agent init` to create the file.
7. **XML Sync** - After every successful pull, the ABAP serializer output is compared byte-for-byte against the git remote files. Differing XML files are reported as a warning; `--sync-xml` rewrites, amends, and re-pulls automatically.
8. **Async for large pulls** - Pulls with >10 files run as ABAP background jobs automatically. Node.js detects the 202 response and polls for completion, showing a progress bar. The final output is identical to a synchronous pull.

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

# Sync serializer XML after creating a new object
abapgit-agent pull --files src/zif_my_intf.intf.abap --sync-xml

# Large pull — runs as background job automatically, progress bar shown
abapgit-agent pull --files src/zcl_a.clas.abap,src/zcl_b.clas.abap,...  # >10 files
```
