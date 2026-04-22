---
layout: default
title: run - Execute Programs & Classes
nav_order: 3
parent: Development Commands
---

# run — Execute ABAP Program or Class and Display Output

Execute an ABAP report or class directly from the CLI and capture its output. No SAP GUI required.

## When to use

**Probe and verify system state** — Write a small class that reads a table, inspects a structure, or calls a function module and prints the result. Faster than SE16/SE37 for ad-hoc queries during development:

```abap
METHOD if_oo_adt_classrun~main.
  SELECT * FROM sflight INTO TABLE @DATA(lt_flights) UP TO 5 ROWS.
  out->write( data = lt_flights name = 'Flights' ).
ENDMETHOD.
```

**Capture report output** — Run an existing executable program and see its `WRITE` output as plain text, without opening SE38 or a SAP GUI session.

---

## Scratch Workspace

By default, AI tools may create probe classes in the current project. To prevent this — for example in production-adjacent systems — set `disableProbeClasses: true` in `.abapgit-agent.json`. When set, probe classes are redirected to a dedicated scratch workspace configured in `.abapGitAgent`.

### Enabling the restriction

In `.abapgit-agent.json` (checked into version control, applies to all developers):

```json
{
  "safeguards": {
    "disableProbeClasses": true,
    "reason": "Production-adjacent system — no throwaway objects in this package."
  }
}
```

### Configuring the scratch workspace

Add `scratchWorkspace` to your personal `.abapGitAgent` (not checked into version control):

```json
{
  "scratchWorkspace": {
    "path": "/absolute/path/to/scratch-repo"
  }
}
```

The `path` points to a separate git repository initialized with `abapgit-agent init` and linked to its own scratch package (e.g. `$MY_SCRATCH`).

**Optional prefixes** — override the auto-derived defaults:

```json
{
  "scratchWorkspace": {
    "path": "/Users/me/code/my-scratch",
    "classPrefix": "YCL_PROBE_",
    "programPrefix": "YPROBE_"
  }
}
```

When `classPrefix` / `programPrefix` are omitted, the tool derives them from your SAP `user` field:

| `user` | `classPrefix` | `programPrefix` |
|--------|--------------|-----------------|
| `JOHN` | `ZCL_JOHN_` | `ZJOHN_` |
| `JOHN` | `ZCL_JOHN_` | `ZJOHN_` |

User-derived prefixes ensure uniqueness when multiple team members share a scratch package.

### Setting up a scratch repo

```bash
mkdir ~/my-scratch && cd ~/my-scratch
git init && git remote add origin <url>
abapgit-agent init --package '$MY_SCRATCH'
```

### AI-generated naming

When an AI tool creates a probe class, it uses:

```
{classPrefix}<PURPOSE>   (max 30 chars total)
```

Examples (user `JOHN`):
- "check open transports" → `ZCL_JOHN_OPEN_TRANSPORTS`
- "list active users" → `ZCL_JOHN_LIST_ACTIVE_USERS`

If a name already exists in `{path}/src/`, `_2`, `_3`, … is appended.

---

## Syntax

```bash
# Execute a PROG report
abapgit-agent run --program <NAME> [--json]

# Execute a class implementing IF_OO_ADT_CLASSRUN
abapgit-agent run --class <NAME> [--json]
```

## Options

| Flag | Required | Description |
|------|----------|-------------|
| `--program` | Yes¹ | ABAP program/report name (any executable PROG) |
| `--class` | Yes¹ | ABAP class implementing `IF_OO_ADT_CLASSRUN` |
| `--json` | No | Machine-readable JSON output |

¹ Exactly one of `--program` or `--class` is required.

---

## Examples

```bash
# Run a report
abapgit-agent run --program ZMY_REPORT

# Run a class implementing IF_OO_ADT_CLASSRUN
abapgit-agent run --class ZCL_AI_RUN_DEMO

# Machine-readable output for scripting
abapgit-agent run --class ZCL_AI_RUN_DEMO --json
```

---

## Human Output

```
--- Output ---
Hello from ZCL_AI_RUN_DEMO!
Date: 13.03.2026
Time: 10:24:55
6 * 7: 42
Fibonacci:
[ 1, 1, 2, 3, 5, 8, 13, 21 ]
--------------
✅ Completed: ZCL_AI_RUN_DEMO
```

## JSON Output (`--json`)

**Class mode:**
```json
{
  "success": true,
  "class": "ZCL_AI_RUN_DEMO",
  "output": "Hello from ZCL_AI_RUN_DEMO!\nDate: 13.03.2026\n..."
}
```

**Program mode:**
```json
{
  "success": true,
  "program": "ZMY_REPORT",
  "output": "Hello, World!\n6 * 7 = 42"
}
```

---

## How It Works

### Class mode (`--class`) — recommended

Calls the ADT classrun endpoint — designed by SAP specifically for headless execution:

```
POST /sap/bc/adt/oo/classrun/<CLASS>
Accept: text/plain
```

The class must implement `IF_OO_ADT_CLASSRUN`. The `main( out )` method is called, and all `out->write( data )` calls are returned as formatted text.

**Key advantage:** `out->write()` accepts any ABAP data object — scalars, structures, internal tables — and formats them automatically. No manual `WRITE` statements needed.

| Call | Output |
|------|--------|
| `out->write( 'Hello!' )` | `Hello!` |
| `out->write( lv_count )` | `42` |
| `out->write( data = ls_flight name = 'Flight' )` | `Flight` header + field-name columns + one row |
| `out->write( data = lt_flights name = 'Flights' )` | `Flights` header + field-name columns + one row per entry |

The `name =` parameter adds a label before the output. Structures and internal tables are rendered as a columnar table with field names as headers.

#### Minimal class template

```abap
CLASS zcl_my_runner DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_my_runner IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    " Write a string
    out->write( 'Hello!' ).

    " Write structured data — formatted automatically
    SELECT carrid, carrname FROM scarr INTO TABLE @DATA(lt_carriers).
    out->write( data = lt_carriers name = 'Airlines' ).
  ENDMETHOD.
ENDCLASS.
```

### Program mode (`--program`)

Calls the ADT programrun endpoint:

```
POST /sap/bc/adt/programs/programrun/<PROGRAM>
Content-Type: application/vnd.sap.adt.programs.programRun+xml
Accept: text/plain
```

SAP's ADT handler (`CL_SEDI_ADT_PROGRAMRUN`) executes `SUBMIT <program> EXPORTING LIST TO MEMORY AND RETURN` with no `WITH` additions. Runtime parameter passing is not supported by this endpoint — the program always runs with its coded defaults. Use `--class` mode when runtime input is needed.

---

## Workflow: Write, Run, Iterate

```bash
# 1. Create a class implementing IF_OO_ADT_CLASSRUN
vim abap/zcl_my_runner.clas.abap

# 2. Push to git and activate
git add . && git commit -m "Add runner class" && git push
abapgit-agent pull --files abap/zcl_my_runner.clas.abap

# 3. Run and see output immediately
abapgit-agent run --class ZCL_MY_RUNNER

# 4. Iterate
```

---

---

## Project Safeguards

### AI usage policy

**AI coding assistants must never call `run` proactively.** Only run when the user explicitly asks.

A class implementing `IF_OO_ADT_CLASSRUN` can do anything — modify database records, send emails, trigger RFCs. The interface signature gives no indication of side effects. Running it automatically (e.g. "to verify it works") is unsafe.

```
✅ User: "Run ZCL_MY_CLASS"  →  run it
❌ AI:   Activates class, then auto-runs it "to verify output"  →  never do this
```

This rule is enforced by `abap/CLAUDE.md` (Rule 9) and can also be hard-blocked via config (see below).

### Config-based block (`disableRun`)

Project maintainers can disable the run command in `.abapgit-agent.json` (checked into the repository):

```json
{
  "safeguards": {
    "disableRun": true,
    "reason": "Side effects in production — manual approval required"
  }
}
```

**When `disableRun: true`:**
- All `run` commands are blocked with a clear error
- The optional `reason` is shown to explain why

This is useful for shared or production-adjacent systems where executing ABAP code without review is unsafe.

---

## Troubleshooting

| Problem | Likely cause | Fix |
|---------|-------------|-----|
| `HTTP 404` | Object not found | Check name is correct (use `view` command) |
| `HTTP 403` | Authorization missing | User needs `S_PROGRAM` / `S_ADT_RES` auth |
| `(no output)` | Class not implementing `IF_OO_ADT_CLASSRUN` | Add `INTERFACES if_oo_adt_classrun.` |
| `HTTP 500` | Runtime error | Check short dumps: `abapgit-agent dump --date TODAY` |

---

## Related Commands

| Command | Purpose |
|---------|---------|
| [pull](pull-command.md) | Activate ABAP objects from git |
| [view](view-command.md) | View ABAP object source |
| [dump](dump-command.md) | Investigate runtime errors (ST22) |
| [debug](debug-command.md) | Step through ABAP code interactively |
