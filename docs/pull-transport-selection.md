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

When tiers 1–3 yield nothing, behaviour depends on the execution context:

- **Manual mode** (interactive TTY): shows a numbered picker of open transport requests — reusing the [`transport` command's](transport-command.md) REST API internally
- **AI mode** (non-TTY, e.g. CI, piped, AI coding tool): runs a project-configured shell hook that can call ABAP APIs and returns a transport number; if no hook is configured, proceeds without a transport

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

The hook is a **Node.js module that lives in the same ABAP project repository**. Because it is `require()`d by abapgit-agent, it has access to the full agent context — including a pre-configured `AbapHttp` instance ready to call any ABAP endpoint without needing to handle credentials separately.

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
  "transportRequest": {
    "hook": "./scripts/get-transport.js",
    "description": "Sprint-based central transport"
  }
}
```

| Field | Type | Description |
|-------|------|-------------|
| `hook` | string | Path to JS module, relative to the repository root (where `abapgit-agent` is run) |
| `description` | string | Optional — shown to users as context |

### Hook Contract

The module must export an async function with the following signature:

```javascript
/**
 * @param {object} context
 * @param {object} context.config    — loaded ABAP config (host, user, password, etc.)
 * @param {object} context.http      — configured AbapHttp instance (ready to call ABAP)
 * @returns {Promise<string|null>}   — transport number, or null to proceed without one
 */
module.exports = async function getTransport({ config, http }) {
  // ...
  return 'DEVK900001';  // or null
};
```

The hook receives:
- `config` — the full `.abapGitAgent` config object (credentials, host, etc.)
- `http` — a pre-built `AbapHttp` instance already authenticated to the ABAP system

It should return a transport number string on success, or `null` to proceed without a transport. Throwing an error is treated as `null` (silent fallback).

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

**Example 2 — Sprint transport (calls ABAP `transport list`)**

Finds the current sprint's central transport by searching the list of open transports where the user has a task:

```javascript
// scripts/get-transport.js
// Returns the current sprint's central transport request

module.exports = async function getTransport({ http }) {
  const result = await http.get('/sap/bc/z_abapgit_agent/transport?scope=task');

  if (!result || !result.transports) return null;

  // Find the first transport containing 'Sprint' in description
  const sprint = result.transports.find(t =>
    t.description && t.description.toLowerCase().includes('sprint')
  );

  return sprint ? sprint.number : null;
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
module.exports = async function getTransport({ http }) {
  const today = new Date().toISOString().slice(0, 10);  // YYYY-MM-DD

  const list = await http.get('/sap/bc/z_abapgit_agent/transport?scope=mine');
  const todaysTransport = (list?.transports || []).find(t => t.date === today);

  if (todaysTransport) return todaysTransport.number;

  // None today — create a new one
  const created = await http.post('/sap/bc/z_abapgit_agent/transport', {
    action: 'CREATE',
    description: `Daily transport ${today}`
  });

  return created?.number || null;
};
```

---

Because the hook has full access to `AbapHttp`, it can call any ABAP endpoint — including `transport list`, `transport create`, or any custom endpoint your project exposes.

### AI Mode Fallback

If no hook is configured, or the hook throws/returns `null`, the pull proceeds without a transport request. No error is raised.

---

## Precedence Summary

```
--transport flag                        ← highest priority (always used when set)
  ↓ if not set
.abapGitAgent transport field           ← config file
  ↓ if not set
ABAP_TRANSPORT env var                  ← environment
  ↓ if not set
isNonInteractive()?
  YES (AI mode)
    hook configured?
      YES → require(hook) → call async fn({ config, http }) → transport number
      NO  → null (proceed without transport)
  NO (Manual mode)
    → interactive picker                   ← user selects or creates
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
  if (projectConfig?.transportRequest) {
    return {
      hook: projectConfig.transportRequest.hook || null,
      description: projectConfig.transportRequest.description || null
    };
  }
  return { hook: null, description: null };
}
```

### `src/utils/transport-selector.js`

Key functions:

| Function | Description |
|----------|-------------|
| `isNonInteractive()` | `!stdout.isTTY \|\| !stdin.isTTY \|\| NO_TTY=1` |
| `runHook(hookPath, context)` | `require(resolved path)` then call exported async fn with `{ config, http }` |
| `fetchTransports(http)` | `GET /transport?scope=...`, normalises ABAP uppercase keys |
| `createTransport(http, desc)` | `POST /transport { action:'CREATE', description }` |
| `interactivePicker(transports, http)` | readline on stdin/stderr; supports scope switching (mine/task/all), create, skip |
| `selectTransport(config, http)` | Main export — branches on TTY detection |

### `src/commands/pull.js`

After the existing transport resolution block (lines 42–49):

```javascript
if (!transportRequest && !jsonOutput) {
  const { selectTransport } = require('../utils/transport-selector');
  const config = loadConfig();
  const http = new AbapHttp(config);
  transportRequest = await selectTransport(config, http);
}
```

`--json` mode is explicitly excluded — never blocks on stdin.
The inner `pull()` method is unchanged; `null` transport omits `transport_request` from the POST body.

### ABAP Dependency

The selector calls `GET /transport` and `POST /transport` — the same endpoint used by `abapgit-agent transport`. The `transport` ABAP command and resource must be activated **before** the Node.js selector goes live. See [transport-command.md](transport-command.md) for the full spec.

The `fetchTransports()` and `createTransport()` helpers in `transport-selector.js` mirror the REST calls that `src/commands/transport.js` makes — consider extracting a shared `src/utils/transport-api.js` to avoid duplication.

---

## Edge Cases

| Scenario | Behaviour |
|----------|-----------|
| ABAP system unreachable when fetching transport list | Picker shows "Create new" and "Skip" only; user warned |
| Hook file not found | Logs warning if `DEBUG=1`; proceeds without transport |
| Hook module throws an error | Silent catch; proceeds without transport |
| Hook returns `null` or non-string | Proceeds without transport |
| `--json` flag | Selector never invoked |
| `upgrade` command (calls `pull.pull()` directly) | Selector not triggered — upgrade has its own transport handling |

---

## Testing

```bash
# Manual mode — interactive picker
abapgit-agent pull   # No --transport set, TTY attached → shows picker

# AI mode with hook
cat > /tmp/get-transport.js << 'EOF'
module.exports = async function({ http }) { return 'DEVK900001'; };
EOF
# Add to .abapgit-agent.json: "transportRequest": { "hook": "/tmp/get-transport.js" }
NO_TTY=1 abapgit-agent pull   # Should use DEVK900001 without prompting

# AI mode without hook
NO_TTY=1 abapgit-agent pull   # no hook → proceeds without transport

# JSON mode bypass
abapgit-agent pull --json   # never prompts regardless of TTY

# Unit tests
npm test
```
