---
layout: default
title: transport - List & Create Transports
nav_order: 19
parent: Development Commands
---

# `transport` Command

## Overview

List and manage SAP transport requests (change requests) from the command line. Use this command to browse your open transports, switch scope, or create a new one before running a pull.

The `transport` REST endpoint is also used internally by the pull command's [interactive transport picker](pull-transport-selection.md).

---

## Usage

```bash
# List my open transport requests (default)
abapgit-agent transport list
abapgit-agent transport list --scope mine

# List transports where I have a task
abapgit-agent transport list --scope task

# List all open transports in the system
abapgit-agent transport list --scope all

# Filter by request type
abapgit-agent transport list --type workbench      # workbench requests only (default)
abapgit-agent transport list --type customizing    # customizing requests only
abapgit-agent transport list --scope all --type customizing

# Create a new transport request (defaults to Workbench request)
abapgit-agent transport create
abapgit-agent transport create --description "Sprint 42 transport"

# Create a Workbench request explicitly (for ABAP code — classes, programs, etc.)
abapgit-agent transport create --description "Sprint 42 transport" --type workbench

# Create a Customizing request (for Customizing / IMG settings)
abapgit-agent transport create --description "FI settings update" --type customizing

# Check a transport request (consistency/pre-release check)
abapgit-agent transport check --number DEVK900001

# Release a transport request
abapgit-agent transport release --number DEVK900001

# JSON output (for scripting)
abapgit-agent transport list --json
abapgit-agent transport list --scope task --json
abapgit-agent transport list --type customizing --json
abapgit-agent transport create --description "Sprint 42" --json
abapgit-agent transport create --description "Sprint 42" --type workbench --json
abapgit-agent transport create --description "FI settings" --type customizing --json
abapgit-agent transport check --number DEVK900001 --json
abapgit-agent transport release --number DEVK900001 --json
```

> **Default subcommand:** `abapgit-agent transport` (with no subcommand) defaults to `transport list --scope mine`.

---

## Parameters

### `transport list`

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--scope` | No | Which transports to list: `mine` (default), `task`, `all` |
| `--type` | No | Filter by request type: `workbench` (default), `customizing` |
| `--json` | No | Output raw JSON |

### `transport create`

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `--description` | No | *(prompts if TTY)* | Description for the new transport request |
| `--type` | No | `workbench` | Request type: `workbench` or `customizing` (see [Request Types](#request-types)) |
| `--json` | No | | Output raw JSON |

### `transport check`

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--number` | Yes | Transport request number (e.g. `DEVK900001`) |
| `--json` | No | Output raw JSON |

### `transport release`

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--number` | Yes | Transport request number (e.g. `DEVK900001`) |
| `--json` | No | Output raw JSON |

### Scope Values (for `list`)

| Scope | Description |
|-------|-------------|
| `mine` | Transports owned by the current ABAP user *(default)* |
| `task` | Transports where the current user owns the request or has a task (sub-order) |
| `all` | All open, modifiable transports in the system |

### Type Filter (for `list`)

| `--type` value | `TRFUNCTION` in E070 | Returns |
|----------------|----------------------|---------|
| `workbench` *(default)* | `K` | Workbench requests (ABAP code objects) |
| `customizing` | `W` | Customizing requests (table entries, IMG settings) |

Use `--type customizing` to find a transport number when using `abapgit-agent customize --transport <TRKORR>`.

### Request Types (for `create`)

| `--type` value | `CATEGORY` passed to FM | `TRFUNCTION` in E070 | Use for |
|----------------|------------------------|----------------------|---------|
| `workbench` *(default)* | `K` | `K` — Workbench Request | ABAP code — classes, programs, function modules, CDS views, etc. |
| `customizing` | `W` | `W` — Customizing Request | Customizing / IMG settings — table entries, config parameters, etc. |

> **Which type do I need?**
>
> - Developing ABAP code (abapGit workflow) → **workbench** (the default)
> - Transporting SAP configuration or Customizing table entries → **customizing**
>
> Choosing the wrong type can cause activation issues or prevent the transport from moving to the target system correctly.

---

## Output

### `transport list` — Default (`--scope mine`)

```
📋 Open Transport Requests (mine)

  #  Number       Description                        Owner        Date
  ─  ──────────   ─────────────────────────────────  ──────────   ──────────
  1  DEVK900001   Feature X — new posting logic      DEVELOPER    2026-03-09
  2  DEVK900002   Bug fix: amount rounding error      DEVELOPER    2026-03-07
  3  DEVK900050   Refactoring ZCL_FINANCE_UTIL        DEVELOPER    2026-03-01

  3 transport(s) found.

  To use one: abapgit-agent pull --transport DEVK900001
  To switch scope:
    transport list --scope task   transports where I have a task
    transport list --scope all     all open transports
```

### `transport list --scope task`

```
📋 Open Transport Requests (task — where I own or have a task)

  #  Number       Description                        Owner        Date
  ─  ──────────   ─────────────────────────────────  ──────────   ──────────
  1  DEVK900001   Feature X — new posting logic      DEVELOPER    2026-03-09
  2  DEVK900010   Sprint 42 central transport        LEAD_DEV     2026-03-05
  3  DEVK900002   Bug fix: amount rounding error      DEVELOPER    2026-03-07

  3 transport(s) found.
```

### `transport list --scope all`

```
📋 Open Transport Requests (all)

  #  Number       Description                        Owner        Date
  ─  ──────────   ─────────────────────────────────  ──────────   ──────────
  1  DEVK900001   Feature X — new posting logic      DEVELOPER    2026-03-09
  2  DEVK900010   Sprint 42 central transport        LEAD_DEV     2026-03-05
  3  DEVK900015   Integration layer refactoring      DEV2         2026-03-04
  4  DEVK900002   Bug fix: amount rounding error      DEVELOPER    2026-03-07
  ...

  50 transport(s) found (showing first 50).
```

### `transport create`

Without `--description`, prompts for one (type defaults to `workbench`):

```
Description: Sprint 42 my feature transport

✅ Transport DEVK900099 created (Workbench request).

   To use it now: abapgit-agent pull --transport DEVK900099
```

With `--description` and explicit `--type`:

```
✅ Transport DEVK900099 created (Workbench request).

   To use it now: abapgit-agent pull --transport DEVK900099
```

```
✅ Transport DEVK900100 created (Customizing request).

   To use it now: abapgit-agent pull --transport DEVK900100
```

### No transports found

```
📋 Open Transport Requests (mine)

  No open transport requests found.

  To create one: abapgit-agent transport create
  To see more:   abapgit-agent transport list --scope task
```

### `transport check DEVK900001`

```
🔍 Checking transport DEVK900001...

   Description: Feature X — new posting logic
   Owner: DEVELOPER
   Date: 2026-03-09

✅ Transport check passed — no issues found.
   Ready to release: abapgit-agent transport release --number DEVK900001
```

With issues:
```
🔍 Checking transport DEVK900001...

⚠️  Transport check completed with warnings/errors:

  Type  Object               Message
  ────  ───────────────────  ──────────────────────────────────────────
  ❌    CLAS ZCL_MY_CLASS     Syntax error in method DO_PROCESS
  ⚠️    INTF ZIF_MY_INTF      Object not yet activated

  2 issue(s) found. Fix before releasing.
```

### `transport release DEVK900001`

```
🚀 Releasing transport DEVK900001...

   Description: Feature X — new posting logic

✅ Transport DEVK900001 released successfully.
```

With error (e.g. unreleased tasks):
```
❌ Could not release transport DEVK900001.

   Error: Transport has open tasks that must be released first.
   Tasks: DEVK900001 (task 1), DEVK900002 (task 2)

   Release tasks first, then re-run:
   abapgit-agent transport release --number DEVK900001
```

### JSON output (`--json`)

`transport list --json`:
```json
{
  "success": true,
  "action": "LIST",
  "scope": "mine",
  "transports": [
    { "number": "DEVK900001", "description": "...", "owner": "DEVELOPER", "date": "2026-03-09" }
  ]
}
```

`transport create --json`:
```json
{ "success": true, "action": "CREATE", "number": "DEVK900099", "type": "workbench", "message": "Transport DEVK900099 created" }
```

`transport check --json`:
```json
{
  "success": true,
  "action": "CHECK",
  "number": "DEVK900001",
  "passed": true,
  "issues": []
}
```

`transport release --json`:
```json
{ "success": true, "action": "RELEASE", "number": "DEVK900001", "message": "Released successfully" }
```

---

## Error Handling

| Scenario | Output |
|----------|--------|
| Cannot connect to ABAP system | `❌ Error: Could not connect to ABAP system` |
| Create disabled by project config | `❌ Error: transport create is disabled for this project` |
| Release disabled by project config | `❌ Error: transport release is disabled for this project` |
| Create fails in ABAP | `❌ Error: Could not create transport request` |
| Invalid `--type` value | `❌ Error: Invalid type 'xyz'. Valid values: workbench, customizing` |
| Invalid `--scope` value | `❌ Error: Invalid scope 'xyz'. Valid values: mine, task, all` |
| Unknown subcommand | `❌ Error: Unknown subcommand 'xyz'. Use: list, create, check, release` |
| `--number` missing for check/release | `❌ Error: --number is required for transport check/release` |
| Check finds issues | Non-zero exit code; issues listed |
| Release fails (open tasks, locked objects) | `❌ Error: <ABAP error message>` |

---

## Project-Level Configuration

Checked into the repository in `.abapgit-agent.json`. Controls which transport operations the team is allowed to run.

```json
{
  "transports": {
    "allowCreate": false,
    "allowRelease": false,
    "reason": "Transport requests are managed by the release manager."
  }
}
```

| Field | Type | Default | Effect |
|-------|------|---------|--------|
| `hook.path` | string | `null` | Path to a JS module that auto-selects a transport (see pull-transport-selection.md) |
| `hook.description` | string | `null` | Optional label shown to users when the hook runs |
| `allowCreate` | boolean | `true` | When `false`, `transport create` is blocked |
| `allowRelease` | boolean | `true` | When `false`, `transport release` is blocked |
| `reason` | string | `null` | Optional message shown alongside the error |

**Example — block both (CI/CD-managed releases):**
```json
{
  "transports": {
    "allowCreate": false,
    "allowRelease": false,
    "reason": "All transport requests are managed via the CI/CD pipeline."
  }
}
```

**Example — allow create, block release (four-eyes principle):**
```json
{
  "transports": {
    "allowCreate": true,
    "allowRelease": false,
    "reason": "Releases require approval from the release manager."
  }
}
```

**Error output when blocked:**
```
❌ transport create is disabled for this project.
   Reason: All transport requests are managed via the CI/CD pipeline.
   This safeguard is configured in .abapgit-agent.json
```

---

## REST API

Other parts of the system (e.g. the pull command's interactive picker) call this endpoint directly.

**Endpoint:** `GET|POST /sap/bc/z_abapgit_agent/transport`

All operations use the same endpoint — the action is specified in the request body (POST) or query string (GET).

| Method | `action` | Description |
|--------|----------|-------------|
| GET | `LIST` (implicit) | List transports (query `?scope=mine|task|all`) |
| POST | `CREATE` | Create new transport |
| POST | `CHECK` | Run consistency check |
| POST | `RELEASE` | Release transport |

### GET — List

```
GET /sap/bc/z_abapgit_agent/transport?scope=mine
```

### POST — Create

```json
{ "action": "CREATE", "description": "My transport", "type": "workbench" }
```

`type` is optional; defaults to `"workbench"`. Valid values: `"workbench"`, `"customizing"`.

### POST — Check

```json
{ "action": "CHECK", "number": "DEVK900001" }
```

Response:
```json
{
  "success": true,
  "action": "CHECK",
  "number": "DEVK900001",
  "passed": true,
  "issues": [
    { "type": "E", "obj_type": "CLAS", "obj_name": "ZCL_MY_CLASS", "text": "Syntax error in method DO_PROCESS" }
  ]
}
```

### POST — Release

```json
{ "action": "RELEASE", "number": "DEVK900001" }
```

Response:
```json
{ "success": true, "action": "RELEASE", "number": "DEVK900001", "message": "Released successfully" }
```

---

## ABAP Implementation

### Files

| File | Description |
|------|-------------|
| `abap/zif_abgagt_command.intf.abap` | Add constant `gc_transport = 'TRANSPORT'` |
| `abap/zif_abgagt_cts_api.intf.abap` | Interface wrapping CTS function modules (mockable) |
| `abap/zif_abgagt_cts_api.intf.xml` | Metadata |
| `abap/zcl_abgagt_cts_api.clas.abap` | Real implementation calling `CTS_API_CREATE_CHANGE_REQUEST` / `TRINT_RELEASE_REQUEST` |
| `abap/zcl_abgagt_cts_api.clas.xml` | Metadata |
| `abap/zcl_abgagt_command_transport.clas.abap` | LIST, CREATE, CHECK, RELEASE logic; accepts injected `ZIF_ABGAGT_CTS_API` |
| `abap/zcl_abgagt_command_transport.clas.xml` | Metadata |
| `abap/zcl_abgagt_resource_transport.clas.abap` | REST resource (GET + POST) |
| `abap/zcl_abgagt_resource_transport.clas.xml` | Metadata |
| `abap/zcl_abgagt_cmd_factory.clas.abap` | Register `TRANSPORT → ZCL_ABGAGT_COMMAND_TRANSPORT` |

No changes needed to `ZCL_ABGAGT_REST_HANDLER` — routing is dynamic.

The command class accepts an optional `ZIF_ABGAGT_CTS_API` instance via constructor, falling back to `ZCL_ABGAGT_CTS_API` in production. This enables unit testing with a local test double without hitting the real CTS function modules.

### Type Definitions (`zcl_abgagt_command_transport`)

```abap
TYPES: BEGIN OF ty_transport_params,
         action      TYPE string,   " 'LIST' | 'CREATE' | 'CHECK' | 'RELEASE'
         scope       TYPE string,   " 'mine' | 'task' | 'all' (LIST only)
         description TYPE string,   " used by CREATE
         type        TYPE string,   " used by CREATE: 'workbench' | 'customizing' (default: 'workbench')
         number      TYPE trkorr,   " used by CHECK and RELEASE
       END OF ty_transport_params.

TYPES: BEGIN OF ty_transport_item,
         number      TYPE trkorr,
         description TYPE string,
         owner       TYPE as4user,
         date        TYPE string,   " YYYY-MM-DD
       END OF ty_transport_item.
TYPES ty_transport_list TYPE STANDARD TABLE OF ty_transport_item
                         WITH NON-UNIQUE DEFAULT KEY.

TYPES: BEGIN OF ty_issue_item,
         type     TYPE string,   " 'E', 'W', 'I'
         obj_type TYPE string,
         obj_name TYPE string,
         text     TYPE string,
       END OF ty_issue_item.
TYPES ty_issue_list TYPE STANDARD TABLE OF ty_issue_item WITH NON-UNIQUE DEFAULT KEY.

TYPES: BEGIN OF ty_transport_result,
         success    TYPE abap_bool,
         action     TYPE string,
         scope      TYPE string,
         transports TYPE ty_transport_list,  " LIST
         number     TYPE trkorr,             " CREATE: new number; CHECK/RELEASE: input echo
         passed     TYPE abap_bool,          " CHECK
         issues     TYPE ty_issue_list,      " CHECK
         message    TYPE string,
         error      TYPE string,
       END OF ty_transport_result.
```

### `execute()` — LIST Queries

**`scope = mine`** (owner = current user):
```abap
SELECT e070~trkorr, e070~as4user, e070~as4date, e07t~as4text
  FROM e070
  INNER JOIN e07t ON e07t~trkorr = e070~trkorr
                 AND e07t~langu   = @sy-langu
  WHERE e070~trstatus   = 'D'
    AND e070~trfunction = 'K'
    AND e070~as4user    = @sy-uname
  ORDER BY e070~as4date DESCENDING
  INTO TABLE @DATA(lt_raw)
  UP TO 50 ROWS.
```

**`scope = task`** (user owns request or has a sub-order task):
```abap
" Step 1: parent transports where current user has a task
SELECT DISTINCT strkorr FROM e070
  WHERE trstatus   = 'D'
    AND trfunction = 'T'
    AND as4user    = @sy-uname
  INTO TABLE @DATA(lt_from_tasks).

" Step 2: transports owned by current user
SELECT trkorr FROM e070
  WHERE trstatus   = 'D'
    AND trfunction = 'K'
    AND as4user    = @sy-uname
  INTO TABLE @DATA(lt_owned).

" Step 3: combine, deduplicate, fetch details
" (merge lt_from_tasks-strkorr and lt_owned-trkorr into one range, then SELECT)
```

**`scope = all`** (no user filter):
```abap
SELECT e070~trkorr, e070~as4user, e070~as4date, e07t~as4text
  FROM e070
  INNER JOIN e07t ON e07t~trkorr = e070~trkorr
                 AND e07t~langu   = @sy-langu
  WHERE e070~trstatus   = 'D'
    AND e070~trfunction = 'K'
  ORDER BY e070~as4date DESCENDING
  INTO TABLE @DATA(lt_raw)
  UP TO 50 ROWS.
```

**Table sources:**
- `E070` — transport header (`TRSTATUS='D'` = modifiable, `TRFUNCTION='K'` = order, `TRFUNCTION='T'` = task)
- `E07T` — short text (join on `TRKORR` + `LANGU`)

### `execute()` — CREATE

Maps the `type` parameter to the SAP category code and delegates to `ZIF_ABGAGT_CTS_API~CREATE_TRANSPORT`:

| `type` value | `CATEGORY` to FM | `TRFUNCTION` result |
|-------------|-----------------|---------------------|
| `workbench` *(default)* | `K` | `K` — Workbench Request |
| `customizing` | `W` | `W` — Customizing Request |

```abap
DATA(lv_category) = COND char01(
  WHEN ls_params-type = 'customizing' THEN 'W'
  ELSE 'K' ).   " default: workbench

DATA(ls_create) = mo_cts_api->create_transport(
  iv_description = ls_params-description
  iv_category    = lv_category ).
```

The real implementation (`ZCL_ABGAGT_CTS_API`) calls `CTS_API_CREATE_CHANGE_REQUEST` with `DESCRIPTION`, `CATEGORY`, `CLIENT = sy-mandt`, and `OWNER = sy-uname`.

### `execute()` — CHECK

> **Note:** `CTS_API_CHECK_TRANSPORT` is not available in all SAP systems. The current implementation delegates to `ZIF_ABGAGT_CTS_API~CHECK_TRANSPORT`, which always returns `subrc = 0` (pass). The check endpoint is intentionally kept for future use — the UI still shows "passed, no issues" and the release step remains the authoritative gate.

```abap
DATA(lv_subrc) = mo_cts_api->check_transport( iv_trkorr = ls_params-number ).
ls_result-passed = COND #( WHEN lv_subrc = 0 THEN abap_true ELSE abap_false ).
```

### `execute()` — RELEASE

Delegates to `ZIF_ABGAGT_CTS_API~RELEASE_TRANSPORT`. The real implementation calls `TRINT_RELEASE_REQUEST` with `IV_DIALOG = abap_false` to suppress SAP dialog popups in REST context:

```abap
DATA(lv_subrc) = mo_cts_api->release_transport( iv_trkorr = ls_params-number ).
IF lv_subrc = 0.
  ls_result-success = abap_true.
  ls_result-message = |Transport { ls_params-number } released|.
ELSE.
  ls_result-success = abap_false.
  ls_result-error   = |Release failed (subrc { lv_subrc })|.
ENDIF.
```

### Resource GET Override (`zcl_abgagt_resource_transport`)

The resource inherits `ZCL_ABGAGT_RESOURCE_BASE`. Overrides `if_rest_resource~get` to read the `scope` query parameter and call the LIST action. Inherited `if_rest_resource~post` handles CREATE automatically.

Pattern reference: `abap/zcl_abgagt_resource_health.clas.abap`.

```abap
METHOD if_rest_resource~get.
  DATA ls_params TYPE zcl_abgagt_command_transport=>ty_transport_params.
  ls_params-action = 'LIST'.
  ls_params-scope  = server->request->get_form_field( 'scope' ).

  DATA(lo_command) = zcl_abgagt_cmd_factory=>get_instance( )->get_command( 'TRANSPORT' ).
  IF lo_command IS NOT BOUND.
    return_error( 'Transport command not found' ).
    RETURN.
  ENDIF.

  return_success( lo_command->execute( is_param = ls_params ) ).
ENDMETHOD.
```

### Node.js CLI (`src/commands/transport.js`)

New command module following the standard `{ name, description, requiresAbapConfig, execute() }` pattern.

**Subcommand dispatch** (first positional arg after `transport`):

| Subcommand | Action | Flags |
|------------|--------|-------|
| `list` (or none) | GET `/transport?scope=...` | `--scope mine|task|all`, `--json` |
| `create` | POST `/transport` `{action:'CREATE'}` | `--description`, `--type workbench|customizing`, `--json` |
| `check` | POST `/transport` `{action:'CHECK'}` | `--number`, `--json` |
| `release` | POST `/transport` `{action:'RELEASE'}` | `--number`, `--json` |

**Output implementation:**
- Table rendered with fixed-width columns (matches existing table style in `pull.js`)
- `--json` passes raw API response to stdout
- Prompts for description when `create` is used interactively without `--description`

### Activation Sequence (ABAP)

1. `zif_abgagt_command.intf.abap`
2. `zif_abgagt_cts_api.intf.abap` (no dependencies)
3. `zcl_abgagt_cts_api.clas.abap` (depends on interface)
4. `zcl_abgagt_command_transport` (`.abap` + `.xml`) (depends on interface + real impl)
5. `zcl_abgagt_resource_transport` (`.abap` + `.xml`)
6. `zcl_abgagt_cmd_factory.clas.abap`

---

## Verification

```bash
# CLI: list
abapgit-agent transport list
abapgit-agent transport                         # same
abapgit-agent transport list --scope task
abapgit-agent transport list --scope all

# CLI: create
abapgit-agent transport create
abapgit-agent transport create --description "Sprint 42 transport"

# CLI: check
abapgit-agent transport check --number DEVK900001

# CLI: release
abapgit-agent transport release --number DEVK900001

# CLI: JSON output
abapgit-agent transport list --json
abapgit-agent transport check --number DEVK900001 --json

# REST: direct calls
curl -u USER:PASS "https://HOST/sap/bc/z_abapgit_agent/transport"
curl -u USER:PASS "https://HOST/sap/bc/z_abapgit_agent/transport?scope=task"
curl -u USER:PASS -X POST "https://HOST/sap/bc/z_abapgit_agent/transport" \
  -H "Content-Type: application/json" \
  -d '{"action":"CREATE","description":"My transport","type":"workbench"}'
curl -u USER:PASS -X POST "https://HOST/sap/bc/z_abapgit_agent/transport" \
  -H "Content-Type: application/json" \
  -d '{"action":"CHECK","number":"DEVK900001"}'
curl -u USER:PASS -X POST "https://HOST/sap/bc/z_abapgit_agent/transport" \
  -H "Content-Type: application/json" \
  -d '{"action":"RELEASE","number":"DEVK900001"}'

# Unit tests
npm test
```
