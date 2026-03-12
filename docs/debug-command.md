---
layout: default
title: debug - Interactive Debugger
nav_order: 5
parent: Development Commands
---

# Debug Command — Specification

Interactive ABAP debugger via SAP ADT REST API.

## Overview

The `debug` command exposes the same ADT debugger API that Eclipse ADT uses, directly from the terminal. It supports two usage modes:

- **Human REPL mode** — `debug attach` blocks and enters an interactive readline session
- **AI / scripting mode** — individual stateless sub-commands with `--json` output, backed by a persisted daemon process

## Usage

```
abapgit-agent debug <subcommand> [options]

Breakpoint Management:
  set     --files <file>:<n>[,...]       Set breakpoints from file paths (e.g. abap/zcl_foo.clas.abap:42)
  set     --objects <name>:<n>[,...]     Set breakpoints from object names (e.g. ZCL_FOO:42)
  set     --object <name> --line <n>     Set a single breakpoint (legacy form)
  list                                   List all breakpoints
  delete  --id <id>                      Delete a breakpoint by ID
  delete  --all                          Delete all breakpoints

  All breakpoint commands accept --json for machine-readable output.

Debug Session (Human REPL mode):
  attach  [--timeout <s>]                Attach and enter interactive REPL (default timeout: 30s)

Debug Session (AI / scripting mode):
  attach  --json [--timeout <s>]         Attach, wait for breakpoint, emit JSON, start daemon
  step    --type over|into|out|continue [--json]
  vars    [--name <var>] [--json]
  vars    --expand <var> [--json]        Drill into a complex variable (table / structure)
  stack   [--json]
  terminate [--json]

Note: step/vars/stack/terminate do NOT accept --session. The active session is
loaded automatically from the daemon state file written by `attach --json`.
```

## REPL Commands (Human Mode)

When `debug attach` is called without `--json`, an interactive readline REPL starts after the breakpoint is hit:

| Command | Aliases | Action |
|---------|---------|--------|
| `s` | `step` | Step into |
| `n` | `next` | Step over |
| `o` | `out` | Step out |
| `c` | `continue` | Continue execution (releases work process) |
| `v` | `vars` | Show variables |
| `x <var>` | `expand <var>` | Drill into a complex variable (table / structure) |
| `bt` | `stack` | Show call stack |
| `q` | `quit` | Detach debugger (program continues running) |
| `kill` | — | Terminate the running program (hard abort) |
| `h` | `help` | Show help |

## AI / Scripting Mode — Daemon IPC

When `attach --json` is called, a **background daemon** (`debug-daemon.js`) is spawned. The daemon holds the stateful ADT HTTP connection open and listens on a Unix socket. All subsequent `step`, `vars`, `stack`, and `terminate` commands communicate with the daemon via that socket rather than opening new HTTP sessions.

**Four best-practice rules for scripted use:**

1. **Wait for `"Listener active"` in the attach output** before firing the trigger — `attach --json` prints this marker to stderr the moment the long-poll POST is about to reach ADT. A blind `sleep` is not reliable under system load
2. **Keep the trigger process alive in the background** for the entire debug session
3. **Always finish with `step --type continue`** — releases the frozen ABAP work process
4. **Never pass `--session`** to `step`/`vars`/`stack`/`terminate` — the active session and socket path are loaded automatically from the state file

Example workflow:
```bash
# Step 1 — start listener (rule 1: wait for "Listener active" before firing trigger)
abapgit-agent debug attach --json > /tmp/attach.out 2>&1 &
until grep -q "Listener active" /tmp/attach.out 2>/dev/null; do sleep 0.3; done
sleep 1   # brief extra window for the POST to reach ADT

# Step 2 — fire trigger, keep alive (rule 2)
abapgit-agent inspect --files abap/zcl_my_class.clas.abap > /tmp/trigger.out 2>&1 &

# Step 3 — poll for {"session":...}
SESSION=""
for i in $(seq 1 30); do
  sleep 0.5
  SESSION=$(grep -o '"session":"[^"]*"' /tmp/attach.out 2>/dev/null | head -1 | cut -d'"' -f4)
  [ -n "$SESSION" ] && break
done

# Step 4 — use debug commands (no --session needed — rule 4)
abapgit-agent debug stack --json
abapgit-agent debug vars --json
abapgit-agent debug step --type over --json

# Step 5 — always release the work process (rule 3)
abapgit-agent debug step --type continue --json
```

## ADT API Endpoints

All endpoints are under `/sap/bc/adt/debugger/`.

| Operation | Method | Endpoint |
|-----------|--------|----------|
| Set/sync breakpoints | POST | `/sap/bc/adt/debugger/breakpoints` |
| List breakpoints | GET | `/sap/bc/adt/debugger/breakpoints?clientId=<id>` |
| Delete breakpoint | DELETE | `/sap/bc/adt/debugger/breakpoints/<id>` |
| Wait for hit | POST | `/sap/bc/adt/debugger/listeners` |
| Attach | POST | `/sap/bc/adt/debugger?method=attach` |
| Step | POST | `/sap/bc/adt/debugger?method=stepOver\|stepInto\|stepReturn\|stepContinue` |
| Get stack | POST | `/sap/bc/adt/debugger?method=getStack` |
| Get variables (child IDs) | POST | `/sap/bc/adt/debugger?method=getChildVariables` |
| Get variables (values) | POST | `/sap/bc/adt/debugger?method=getVariables` |
| Terminate | POST | `/sap/bc/adt/debugger?method=terminateDebuggee` |

## Breakpoint XML Format

**Critical:** The correct format was verified against [`abap-adt-api`](https://github.com/marcellourbani/abap-adt-api). Earlier attempts using `adtdbg:` namespace or `PUT` method failed silently.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<dbg:breakpoints xmlns:dbg="http://www.sap.com/adt/debugger"
                 scope="external"
                 debuggingMode="user"
                 requestUser="MYUSER"
                 terminalId="ABAPGIT-AGENT-CLI"
                 ideId="ABAPGIT-AGENT-CLI"
                 systemDebugging="false"
                 deactivated="false">
  <syncScope mode="full"></syncScope>
  <breakpoint xmlns:adtcore="http://www.sap.com/adt/core"
              kind="line"
              clientId="ABAPGIT-AGENT-CLI"
              skipCount="0"
              adtcore:uri="/sap/bc/adt/oo/classes/zcl_my_class/source/main#start=42"/>
</dbg:breakpoints>
```

Key rules:
- **Method**: `POST` to `/sap/bc/adt/debugger/breakpoints` (not `PUT`)
- **Content-Type / Accept**: `application/xml`
- **Root namespace**: `dbg:` (`http://www.sap.com/adt/debugger`) — not `adtdbg:`
- **`<syncScope mode="full">`** is required — this is the synchronize model: the full desired list is sent each time; absent breakpoints are removed server-side
- **URI format for classes**: `/sap/bc/adt/oo/classes/<lowercase>/source/main#start=<line>`
  - The `/source/main` suffix is **required** — without it ADT returns `errorMessage="Cannot create a breakpoint at this position"`
  - The `#start=<line>` fragment carries the line number — there is no separate line element
- **URI format for programs**: `/sap/bc/adt/programs/programs/<lowercase>#start=<line>`
- **`kind="line"`** — not `"breakpoint"`
- **`clientId`** scopes breakpoints to this tool (separate from Eclipse ADT breakpoints)

The response XML contains a `<breakpoint id="...">` element with the server-assigned ID (or `errorMessage` if the line is not a valid breakpoint position, e.g. a comment or DATA declaration).

### Object URI mapping (`objectUri()`)

```javascript
// ZCL_*/ZIF_*/YCL_*/YIF_* → OO class with /source/main
/sap/bc/adt/oo/classes/zcl_my_class/source/main

// Everything else → program
/sap/bc/adt/programs/programs/zmy_program
```

## Breakpoint State (Local Persistence)

Because the ADT synchronize model requires sending the *full* desired list on every POST, the current list is persisted locally in a tmp file (`/tmp/abapgit-debug-bp-<hash>.json`) by `debug-state.js`. This allows `set`, `delete`, and `list` to be called as independent CLI invocations without a round-trip GET each time.

`debug list` first checks local state; falls back to `GET /sap/bc/adt/debugger/breakpoints` if local state is empty.

## Listener Flow

The ADT listener is a **long-polling POST** that blocks until a breakpoint fires (or times out):

```
POST /sap/bc/adt/debugger/listeners
  ?debuggingMode=user
  &requestUser=MYUSER
  &terminalId=ABAPGIT-AGENT-CLI
  &ideId=ABAPGIT-AGENT-CLI
  &timeout=30
```

- On timeout (no hit): returns 200 with empty body → poll again
- On breakpoint hit: returns 200 with XML body containing `<DEBUGGEE_ID>`
- On conflict (another listener active): returns 406 → wait 2s and retry

The `terminalId` and `ideId` in the listener **must match** the `terminalId` and `ideId` in the breakpoint registration. If they differ, the listener never receives the notification.

## Attach

After the listener returns a `DEBUGGEE_ID`, attach to register as the active debugger for that work process:

```
POST /sap/bc/adt/debugger?method=attach
  &debuggeeId=<DEBUGGEE_ID>
  &dynproDebugging=true
  &debuggingMode=user
  &requestUser=MYUSER
```

Headers: `X-sap-adt-sessiontype: stateful`

Response contains `debugSessionId="..."` — this is used for all subsequent calls.

**The `X-sap-adt-sessiontype: stateful` header is required on all session operations** (attach, step, getStack, getVariables, terminate). Without it, each request may land on a different ABAP work process, causing `noSessionAttached` (T100KEY-NO=530) errors.

**SAP's ICM drops stateful session affinity after ~60 s of idle.** In human REPL mode, `startKeepalive()` is called automatically after the REPL appears; it fires a `getStack()` every 30 s to keep the session warm. `stopKeepalive()` is called before `detach()`/`terminate()` to prevent a race between the keepalive timer and the closing request. In daemon (AI) mode the daemon's own periodic IPC traffic keeps the session alive.

## Variables — Two-Step Protocol

Getting variable values requires two POST calls:

**Step 1 — `getChildVariables`**: Returns the variable hierarchy (parent → child IDs).

```
POST /sap/bc/adt/debugger?method=getChildVariables
Content-Type: application/vnd.sap.as+xml; charset=UTF-8; dataname=com.sap.adt.debugger.ChildVariables
Accept:       application/vnd.sap.as+xml; charset=UTF-8; dataname=com.sap.adt.debugger.ChildVariables

Body: request children of @ROOT, @PARAMETERS, @LOCALS, @DATAAGING
```

Request body:
```xml
<?xml version="1.0" encoding="UTF-8" ?>
<asx:abap version="1.0" xmlns:asx="http://www.sap.com/abapxml">
  <asx:values><DATA><HIERARCHIES>
    <STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>@ROOT</PARENT_ID></STPDA_ADT_VARIABLE_HIERARCHY>
    <STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>@PARAMETERS</PARENT_ID></STPDA_ADT_VARIABLE_HIERARCHY>
    <STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>@LOCALS</PARENT_ID></STPDA_ADT_VARIABLE_HIERARCHY>
    <STPDA_ADT_VARIABLE_HIERARCHY><PARENT_ID>@DATAAGING</PARENT_ID></STPDA_ADT_VARIABLE_HIERARCHY>
  </HIERARCHIES></DATA></asx:values></asx:abap>
```

Requesting `@PARAMETERS` and `@LOCALS` directly (in addition to `@ROOT`) is **essential** to get leaf variable IDs in a single round-trip. Without them, ADT only returns the group node IDs (`@PARAMETERS`, `@LOCALS`) but not their children (the actual variables).

The response `HIERARCHIES` table maps `PARENT_ID → CHILD_ID`. Extract `CHILD_ID` values whose `PARENT_ID` is `@PARAMETERS`, `@LOCALS`, `@ROOT`, or `@DATAAGING`, filtering out virtual group IDs (those starting with `@`).

**Step 2 — `getVariables`**: Get values for the leaf IDs from step 1.

```
POST /sap/bc/adt/debugger?method=getVariables
Content-Type: application/vnd.sap.as+xml; charset=UTF-8; dataname=com.sap.adt.debugger.Variables
Accept:       application/vnd.sap.as+xml; charset=UTF-8; dataname=com.sap.adt.debugger.Variables
```

The `dataname=` part of Content-Type is an ADT routing key — using the wrong value silently routes the request to the wrong handler.

## Stack

```
POST /sap/bc/adt/debugger?method=getStack&emode=_&semanticURIs=true
```

Response contains `<stackEntry>` elements with `stackPosition`, `programName`, `includeName`, `line`, `eventName`, `stackType`, and `adtcore:uri` attributes.

- Skip `stackType != "ABAP"` frames (DYNP/screen frames are not useful)
- Sort descending by `stackPosition` so `frames[0]` is top of stack
- `adtcore:uri` contains the source URI with `#start=<line>` — can be used to fetch source context

## Session Persistence (AI Mode)

When `attach --json` succeeds, both the `sessionId` and the daemon's Unix `socketPath` are saved to `/tmp/abapgit-debug-session-<hash>.json`. All AI-mode commands (`step`, `vars`, `stack`, `terminate`) load the socket path automatically and communicate with the daemon via IPC — they do **not** accept a `--session` flag.

The daemon auto-exits after 30 minutes of idle time or when `terminate` is called.

**`detach()` — single stepContinue:** When the REPL sends `q`, `detach()` issues one `stepContinue`. ADT returns HTTP 200 as soon as the WP resumes (regardless of whether it later hits another breakpoint — "still running" and "re-hit breakpoint" are indistinguishable in the response). Sending a second `stepContinue` to an already-running WP races with the program's execution and can stall it mid-run (e.g. while a Code Inspector job is in flight). The REPL's `q` handler deletes all breakpoints before calling `detach()` to prevent an immediate re-hit.

## Files

| File | Purpose |
|------|---------|
| `src/commands/debug.js` | CLI subcommand routing, breakpoint management, attach/step/vars/stack/terminate |
| `src/utils/debug-session.js` | `DebugSession` class — wraps all stateful ADT calls |
| `src/utils/debug-state.js` | Persists active session ID, socket path, and breakpoint list to tmp files |
| `src/utils/debug-repl.js` | readline-based interactive REPL for human mode |
| `src/utils/debug-render.js` | Shared display helpers (variable list, source context) used by REPL and AI mode |
| `src/utils/debug-daemon.js` | Background daemon — holds stateful ADT HTTP connection; accepts IPC commands via Unix socket |
| `src/utils/adt-http.js` | ADT HTTP client (CSRF, stateful sessions, XML helpers) |
| `tests/unit/debug-command.test.js` | Unit tests for debug command |
| `tests/unit/adt-http.test.js` | Unit tests for AdtHttp |
| `tests/integration/debug-scenarios.sh` | Integration tests: REPL simple, REPL takeover, scripted AI mode |

## Prerequisites

- ABAP system must have ICF node `/sap/bc/adt/` activated (transaction SICF)
- User needs `S_ADT_RES` authorization (ACTVT=16) for debugging
- Breakpoints only fire for dialog work processes executing under the configured `user`; background jobs do **not** trigger the listener

## Known Limitations

- Breakpoints fire only for dialog requests by the configured user — to trigger via `abapgit-agent`, use commands that issue synchronous REST calls to the ABAP backend: `view`, `preview`, `where`, `inspect`, `unit` (not `syntax` which parses locally, and not `pull` which runs as a background job)
- The `#start=<line>` line number in the URI must point to an executable ABAP statement — comments, blank lines, `DATA` declarations, and `METHOD`/`ENDMETHOD` lines are rejected with "Cannot create a breakpoint at this position". Use `view --objects ZCL_MY_CLASS --full --lines` to find valid line numbers — the method header hint already skips non-executable lines and points directly to the first executable statement
