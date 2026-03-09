---
layout: default
title: Conflict Detection
nav_order: 1
parent: pull - Pull & Activate
grand_parent: Development Commands
---

# pull Command - Conflict Detection

## Overview

Conflict detection prevents silent overwrites when an ABAP object has been modified locally (in ADT/SE80) since the last pull, or when switching to a branch whose content has diverged. It is **enabled by default** — every pull checks for conflicts and aborts if any are found.

---

## The Problem

Without conflict detection, `pull` calls abapGit's `deserialize()` unconditionally:

```
1. Developer pulls from git → starts editing object in ADT
2. Developer runs pull again → ADT edits are silently overwritten
```

Or across branches:

```
1. Developer pulls branch b1 (baseline recorded)
2. Developer switches to branch b2 (different content for the same object)
3. Developer runs pull → b2 content overwrites b1 without warning
```

---

## Three Conflict Types

### LOCAL\_EDIT

The system object was modified in ADT/SE80 after the last pull, but git content is unchanged. Pulling would silently overwrite local edits.

```
Pull (baseline SHA recorded)
  → Object edited in ADT                  ← system diverged from baseline
  → Pull same branch again → LOCAL_EDIT conflict
```

### SYSTEM\_EDIT

Both the ABAP system and git were modified since the last pull. Both sides diverged.

```
Pull (baseline SHA recorded)
  → Object edited in ADT                  ← system diverged
  → Someone pushes a new version to git   ← git also diverged
  → Pull again → SYSTEM_EDIT conflict
```

### BRANCH\_SWITCH

Switching to a branch whose content differs from what was last pulled.

```
Pull branch b1 (baseline: branch=b1, sha=abc)
  → Pull branch b2 where same object has sha=xyz
  → BRANCH_SWITCH conflict
```

---

## How It Works

### Tracking Table: `ZABGAGT_OBJ_META`

After every successful pull, a baseline is written to this transparent table:

| Field | Type | Description |
|-------|------|-------------|
| `obj_type` (key) | `tadir-object` | Object type (CLAS, INTF, …) |
| `obj_name` (key) | `tadir-obj_name` | Object name |
| `devclass` | `devclass` | Package (refreshed from TADIR on every pull — handles package moves) |
| `last_git_sha` | `string(40)` | SHA of the pulled file content |
| `last_branch` | `string(255)` | Branch that was pulled |
| `last_pulled_at` | `timestampl` | When the pull happened |
| `last_pulled_by` | `syuname` | Who performed the pull |
| `sys_changed_by` | `syuname` | Who last pulled (stored for conflict report) |

### Detection Logic

Before calling `deserialize()`, for each file being pulled:

```
current_sha     = SHA-1( incoming git file content )
local_sha       = SHA-1( current ABAP system content, via get_files_local() )

git_changed     = current_sha ≠ last_git_sha      (git changed since last pull)
sys_changed     = local_sha   ≠ last_git_sha       (system changed since last pull)
branch_switched = current_branch ≠ last_branch

SYSTEM_EDIT:    git_changed AND sys_changed
BRANCH_SWITCH:  git_changed AND branch_switched (and not sys_changed)
LOCAL_EDIT:     sys_changed AND NOT git_changed

Safe (no conflict):
  - git changed only, same branch      → fast-forward update
  - branch switched, same SHA          → branches have identical content
  - no baseline (first pull)           → skipped, baseline created after pull
```

### Why Content SHA (Not Timestamps)

VRSD (SAP version management) is only updated when objects are added to transport requests. Objects in local packages (prefix `$`) never get VRSD entries, making timestamp-based detection unreliable. Content SHA comparison works for all package types.

### Bootstrap Behavior (First Pull)

No baseline exists → conflict detection is skipped → pull always succeeds → baseline written.

| Pull # | Baseline exists? | Conflict detection |
|--------|------------------|--------------------|
| 1st | No | Skipped — baseline created |
| 2nd+ | Yes | Active |

---

## Output

### Conflict Aborted

```
⚠️  Pull aborted — 1 conflict(s) detected

────────────────────────────────────────────────────────────────────────────────
Pull aborted — 1 conflict(s) detected

Object:        CLAS ZCL_MY_CLASS
Conflict type: LOCAL_EDIT
System modified after last pull by DEVELOPER1
Git content unchanged — pull would overwrite local edits

To override: abapgit-agent pull --conflict-mode ignore
```

```
Object:        CLAS ZCL_MY_CLASS
Conflict type: SYSTEM_EDIT
Git changed:   abc1234... → def5678...
System changed by DEVELOPER2 (after last pull by DEVELOPER1)
```

```
Object:        CLAS ZCL_MY_CLASS
Conflict type: BRANCH_SWITCH
Branch:        main → feature/my-branch
Git changed:   abc1234... → xyz9876...
Last pull by DEVELOPER1
```

### No Conflicts

Pull proceeds as normal — no additional output. Baseline is silently updated after each successful pull.

---

## Implementation Files

| File | Description |
|------|-------------|
| `abap/zabgagt_obj_meta.tabl.xml` | Metadata tracking table |
| `abap/zif_abgagt_conflict_detector.intf.abap` | Conflict detector interface |
| `abap/zcl_abgagt_conflict_detector.clas.abap` | Core detection logic |
| `abap/zcl_abgagt_conflict_detector.clas.testclasses.abap` | ABAP unit tests (16 tests) |
| `abap/zif_abgagt_agent.intf.abap` | `iv_conflict_mode` added to `pull()` |
| `abap/zcl_abgagt_agent.clas.abap` | Conflict check + metadata store in `pull()` |
| `abap/zcl_abgagt_command_pull.clas.abap` | `conflict_mode` in params struct |
| `src/commands/pull.js` | `--conflict-mode` flag |
| `tests/integration/conflict-runner.js` | Integration tests (5 scenarios) |

---

## Configuration

### CLI Flag (highest priority)

```bash
abapgit-agent pull --conflict-mode ignore   # bypass for this run
abapgit-agent pull --conflict-mode abort    # force on for this run
```

### Project Config (`.abapgit-agent.json`)

Set the default mode for all developers on a project:

```json
{
  "conflictDetection": {
    "mode": "ignore",
    "reason": "Single-developer project — conflict detection not needed"
  }
}
```

| Field | Values | Default | Description |
|-------|--------|---------|-------------|
| `mode` | `"abort"`, `"ignore"` | `"abort"` | Default conflict detection behaviour |
| `reason` | string | `null` | Optional explanation (not currently shown in output) |

### Precedence

```
CLI --conflict-mode flag  >  .abapgit-agent.json conflictDetection.mode  >  "abort"
```

---

## Key Behaviors

1. **Always on by default** — `abort` is the default; use `--conflict-mode ignore` or project config to bypass
2. **First pull always succeeds** — no baseline yet, detection skipped
3. **Per-object tracking** — each object has its own baseline
4. **Content SHA, not timestamps** — works for all package types including local `$` packages
5. **Store after success only** — baseline never updated when pull fails or is aborted
6. **DEVCLASS auto-refreshed** — `devclass` is read from TADIR on every pull; package moves are handled automatically
