---
layout: default
title: Transport Selection
nav_order: 2
parent: pull - Pull & Activate
grand_parent: Development Commands
---

# pull Command - Transport Request Selection

## Overview

The pull command resolves a transport request in four tiers, each acting as a fallback for the one before it:

```
1. --transport CLI flag
2. transport in .abapGitAgent config file
3. ABAP_TRANSPORT environment variable
4. Automatic selection (this feature)  ← fires only when 1–3 all return null
```

When tiers 1–3 yield nothing, behaviour depends on whether a hook is configured and the execution context:

- **Hook configured, returns transport**: uses the returned transport regardless of TTY context
- **Hook configured, returns null, non-interactive** (CI, pipe, AI tool): fails with an error
- **Hook configured, returns null, interactive** (TTY): warns the user, then shows the numbered picker
- **No hook, manual mode** (interactive TTY): shows a numbered picker of open transport requests
- **No hook, AI mode** (non-TTY, e.g. CI, piped, AI coding tool): proceeds without a transport

---

## Manual Mode — Interactive Picker

When running interactively in a terminal, the pull command fetches open transport requests and presents a numbered menu.

The default scope is `mine` (transports owned by the current user). The user can switch scope to see more transports:

```
Select a transport request (showing: my transports):

  1. DEVK900001  Feature X transport  (DEVELOPER, 2026-03-09)
  2. DEVK900002  Bug fix              (DEVELOPER, 2026-03-07)
  ─────────────────────────────────────────────────────────────
  s. Show transports where I have a task
  a. Show all open transports
  c. Create new transport request
  0. Skip (no transport request)

Enter number or option:
```

**Options:**

| Input | Action |
|-------|--------|
| `1`–`N` | Use the selected transport |
| `s` | Re-fetch with `scope=task` (transports where I have a task) |
| `a` | Re-fetch with `scope=all` (all open transports in the system) |
| `c` | Prompt for description → create transport via ABAP API → use it |
| `0` | Skip — proceed without a transport |

- If the ABAP system is unreachable when fetching, only "Create new" and "Skip" are shown (user is warned)

---

## AI Mode — Node.js Hook

When running non-interactively (no TTY, e.g. GitHub Actions, pipe, AI coding tool subprocess), the CLI checks for a project-level hook configured in `.abapgit-agent.json`.

The hook is a **Node.js module that lives in the same ABAP project repository**. It receives a `run(command)` helper that calls any CLI command programmatically and returns parsed JSON — no raw HTTP or credentials handling needed.

### Project Layout

```
my-abap-project/               ← ABAP project git repo
├── .abapgit-agent.json        ← configures the hook path (checked into git)
├── .abapGitAgent              ← ABAP credentials (gitignored)
├── scripts/
│   └── get-transport.js       ← the hook module (checked into git)
└── src/
    └── ... ABAP files ...
```

### Configuration

`.abapgit-agent.json` in the ABAP project repo:

```json
{
  "transports": {
    "hook": {
      "path": "./scripts/get-transport.js",
      "description": "Sprint-based central transport"
    }
  }
}
```

| Field | Type | Description |
|-------|------|-------------|
| `hook.path` | string | Path to JS module, relative to the repository root (where `abapgit-agent` is run) |
| `hook.description` | string | Optional — shown to users as context |

### Hook Contract

The module must export an async function with the following signature:

```javascript
/**
 * @param {object}   context
 * @param {object}   context.config    — loaded ABAP config (host, user, password, etc.)
 * @param {object}   context.http      — configured AbapHttp instance (ready to call ABAP)
 * @param {Function} context.run       — call any CLI command as a string, returns parsed JSON
 *                                        e.g. run('transport list --scope task')
 * @returns {Promise<string|null>}     — transport number, or null to proceed without one
 */
module.exports = async function getTransport({ config, http, run }) {
  // ...
  return 'DEVK900001';  // or null
};
```

The hook receives:
- `config` — the full `.abapGitAgent` config object (credentials, host, etc.)
- `http` — a pre-built `AbapHttp` instance already authenticated to the ABAP system
- `run(command)` — accepts a full CLI command string (e.g. `'transport list --scope task'`), always uses `--json` mode, and returns the parsed response object. Use this instead of raw `http` calls for readability.

It should return a transport number string on success, or `null` when no transport could be determined. Throwing an error is treated as `null` (silent fallback).

**What happens when the hook returns `null`** depends on context:

| Context | Hook returns `null` | Result |
|---------|---------------------|--------|
| Non-interactive (AI/CI mode) | null | ❌ Pull fails with error |
| Interactive (TTY) | null | ⚠️ Warning printed, interactive picker shown |

### Example Hooks

**Example 1 — Static transport (simplest case)**

Always use the same transport, regardless of context:

```javascript
// scripts/get-transport.js
module.exports = async function getTransport({ config }) {
  return 'DEVK900001';
};
```

---

**Example 2 — Sprint transport (calls `transport list`)**

Finds the current sprint's central transport by searching the list of open transports where the user has a task:

```javascript
// scripts/get-transport.js
// Returns the current sprint's central transport request

module.exports = async function getTransport({ run }) {
  const result = await run('transport list --scope task');
  const transports = result.TRANSPORTS || result.transports || [];

  // Find the first transport containing 'Sprint' in description
  const sprint = transports.find(t =>
    (t.DESCRIPTION || t.description || '').toLowerCase().includes('sprint')
  );

  return sprint ? (sprint.NUMBER || sprint.number) : null;
};
```

---

**Example 3 — Environment-based transport (e.g. from CI env var)**

For CI/CD pipelines that set a transport number in the environment:

```javascript
// scripts/get-transport.js
module.exports = async function getTransport({ config }) {
  // Read from a CI environment variable set by the pipeline
  return process.env.CI_TRANSPORT_REQUEST || null;
};
```

---

**Example 4 — Create a new transport if none exists for today**

More advanced: list open transports, and if there is none from today, create a new one:

```javascript
// scripts/get-transport.js
module.exports = async function getTransport({ run }) {
  const today = new Date().toISOString().slice(0, 10);  // YYYY-MM-DD

  const list = await run('transport list --scope mine');
  const transports = list.TRANSPORTS || list.transports || [];
  const todaysTransport = transports.find(t => (t.DATE || t.date) === today);

  if (todaysTransport) return todaysTransport.NUMBER || todaysTransport.number;

  // None today — create a new one
  const created = await run(`transport create --description "Daily transport ${today}"`);
  return created?.NUMBER || created?.number || null;
};
```

---

Because the hook has access to `run()`, it can invoke any `abapgit-agent` command — including `transport list`, `transport create`, or any other command your project uses.

### Hook returns null — Non-interactive mode

If a hook **is** configured but returns `null` while running non-interactively (CI, pipe, AI coding tool), the pull **fails with an error**:

```
❌ Error: transport hook returned no transport request.
   Hook: ./scripts/get-transport.js
   Sprint-based central transport
```

This ensures a misconfigured or failing hook does not silently activate objects without a transport in environments where one is required.

### Hook returns null — Interactive mode

If a hook returns `null` while running in a terminal, the pull **warns the user and falls through to the interactive picker**:

```
⚠️  Transport hook returned no transport request (./scripts/get-transport.js).
   Please select one manually:

Select a transport request (showing: my transports):

  1. DEVK900001  Feature X transport  (DEVELOPER, 2026-03-09)
  ...
```

This lets a developer continue working even when the hook (e.g. a sprint-based selector) finds no matching transport.

---

## Precedence Summary

```
--transport flag                        ← highest priority (always used when set)
  ↓ if not set
.abapGitAgent transport field           ← config file
  ↓ if not set
ABAP_TRANSPORT env var                  ← environment
  ↓ if not set
hook configured in .abapgit-agent.json?
  YES → require(hook) → call async fn({ config, http, run }) → transport number
        null returned
          isNonInteractive()?
            YES (AI/CI mode) → fail with error
            NO  (TTY)        → warn + interactive picker ← user selects or creates
                                 → skip selected → null
  NO
    isNonInteractive()?
      YES (AI/CI mode) → null (proceed without transport)
      NO  (TTY)        → interactive picker ← user selects or creates
                           → skip selected → null
```

---

## Implementation

### Node.js Files

| File | Change |
|------|--------|
| `src/config.js` | Add `getTransportHookConfig()` — reads `transportRequest` from `.abapgit-agent.json` |
| `src/utils/transport-selector.js` | **New** — `selectTransport(abapConfig, http)` |
| `src/commands/pull.js` | Call `selectTransport()` after existing resolution block when `transportRequest` is null |

### `src/config.js` — `getTransportHookConfig()`

Follows the `getConflictSettings()` pattern:

```javascript
function getTransportHookConfig() {
  const projectConfig = loadProjectConfig();
  if (projectConfig?.transports?.hook) {
    return {
      hook: projectConfig.transports.hook.path || null,
      description: projectConfig.transports.hook.description || null
    };
  }
  return { hook: null, description: null };
}
```

### `src/utils/transport-selector.js`

Key functions:

| Function | Description |
|----------|-------------|
| `buildRun(config, http, loadConfig, AbapHttp, getTransportSettings)` | Builds the `run(command)` helper closure passed to hooks |
| `isNonInteractive()` | `!stdout.isTTY \|\| !stdin.isTTY \|\| NO_TTY=1` |
| `runHook(hookPath, context)` | `require(resolved path)` then call exported async fn with `{ config, http, run }` |
| `fetchTransports(http)` | `GET /transport?scope=...`, normalises ABAP uppercase keys |
| `createTransport(http, desc)` | `POST /transport { action:'CREATE', description }` |
| `interactivePicker(transports, http)` | readline on stdin/stderr; supports scope switching (mine/task/all), create, skip |
| `selectTransport(config, http, loadConfig?, AbapHttp?, getTransportSettings?)` | Main export — branches on TTY detection |

### `src/commands/pull.js`

After the existing transport resolution block (lines 42–49):

```javascript
if (!transportRequest && !jsonOutput) {
  const { selectTransport } = require('../utils/transport-selector');
  const config = loadConfig();
  const http = new AbapHttp(config);
  transportRequest = await selectTransport(config, http, loadConfig, AbapHttp, getTransportSettings);
}
```

`--json` mode is explicitly excluded — never blocks on stdin.
The inner `pull()` method is unchanged; `null` transport omits `transport_request` from the POST body.

### ABAP Dependency

The selector calls `GET /transport` and `POST /transport` — the same endpoint used by `abapgit-agent transport`. The `transport` ABAP command and resource must be activated **before** the Node.js selector goes live. See [transport-command.md](transport-command.md) for the full spec.

---

## Edge Cases

| Scenario | Behaviour |
|----------|-----------|
| ABAP system unreachable when fetching transport list | Picker shows "Create new" and "Skip" only; user warned |
| Hook file not found | `selectTransport` catches error → treated as null → non-TTY: fail; TTY: picker shown |
| Hook module throws an error | Same as above |
| Hook returns `null`, non-interactive | Pull fails with "transport hook returned no transport request" |
| Hook returns `null`, interactive (TTY) | Warning printed, interactive picker shown |
| No hook configured, no transport | Non-TTY: proceeds without transport; TTY: interactive picker shown |
| `--json` flag | Selector never invoked |
| `upgrade` command (calls `pull.pull()` directly) | Selector not triggered — upgrade has its own transport handling |

---

## Testing

```bash
# Manual mode — interactive picker (no hook configured)
abapgit-agent pull   # No --transport set, TTY attached → shows picker

# Hook mode — hook returns a transport
cat > /tmp/get-transport.js << 'EOF'
module.exports = async function() { return 'DEVK900001'; };
EOF
# Add to .abapgit-agent.json: "transports": { "hook": { "path": "/tmp/get-transport.js" } }
abapgit-agent pull   # Uses DEVK900001

# Hook mode — hook returns null, interactive (TTY): warns + shows picker
cat > /tmp/get-transport.js << 'EOF'
module.exports = async function() { return null; };
EOF
abapgit-agent pull   # Prints warning, then shows transport picker

# Hook mode — hook returns null, non-interactive: fails with error
NO_TTY=1 abapgit-agent pull   # ❌ Error: transport hook returned no transport request

# AI mode without hook
NO_TTY=1 abapgit-agent pull   # no hook → proceeds without transport

# JSON mode bypass
abapgit-agent pull --json   # never prompts regardless of TTY

# Unit tests
npm test
```
