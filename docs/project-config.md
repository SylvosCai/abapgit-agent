---
layout: default
title: Project Configuration (.abapgit-agent.json)
nav_order: 3
---

# Project Configuration — `.abapgit-agent.json`

`.abapgit-agent.json` is the **project-level config file** for an ABAP project that uses
abapgit-agent. It is committed to the root of the project repo (not `.gitignore`d) and
contains team decisions about quality gates, safeguards, and transport policy.

It is distinct from `.abapGitAgent` (the personal/machine config with credentials — never
committed to git).

---

## Full example

```json
{
  "project": {
    "name": "my-abap-project",
    "description": "My ABAP project"
  },
  "safeguards": {
    "requireFilesForPull": true,
    "disablePull": false,
    "disableRun": true,
    "disableImport": false,
    "requireImportMessage": true,
    "importAllowedUsers": ["CI_USER", "RELEASE_MGR"],
    "disableProbeClasses": false,
    "reason": "Production system — handle with care"
  },
  "conflictDetection": {
    "mode": "abort",
    "reason": "Multi-developer project — conflicts must be resolved manually"
  },
  "coverage": {
    "threshold": 80,
    "mode": "fail",
    "excludes": [
      "src/zcl_legacy_handler.clas.abap",
      "src/zbp_*.clas.abap"
    ]
  },
  "transports": {
    "allowCreate": false,
    "allowRelease": false,
    "reason": "Release manager only",
    "hook": {
      "path": "./scripts/get-transport.js",
      "description": "Earliest open transport owned by CI user"
    }
  }
}
```

All sections are optional. Keys not present fall back to sensible defaults.

---

## `project`

Informational metadata about the project. Set automatically by `abapgit-agent init`.

| Key | Type | Description |
|-----|------|-------------|
| `name` | string | Project name |
| `description` | string | Short description |

---

## `safeguards`

Project-level restrictions that **cannot be overridden** by user config or CLI flags.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `requireFilesForPull` | boolean | `false` | If `true`, `abapgit-agent pull` must always specify `--files`. Prevents accidental full-repo pulls. |
| `disablePull` | boolean | `false` | Completely disables the `pull` command for this project |
| `disableRun` | boolean | `false` | Disables the `run` command |
| `disableImport` | boolean | `false` | Disables the `import` command |
| `requireImportMessage` | boolean | `false` | Requires a `--message` when running `import` |
| `importAllowedUsers` | string \| string[] | `null` (all) | ABAP usernames allowed to run `import` (case-insensitive). All others are blocked. |
| `disableProbeClasses` | boolean | `false` | Disables creation of throwaway probe classes |
| `reason` | string | `null` | Message shown when a safeguard blocks an action |

---

## `conflictDetection`

Controls how `abapgit-agent pull` handles conflicts between local and remote state.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `mode` | `abort` \| `ignore` | `abort` | `abort` = stop on conflict (safe default); `ignore` = proceed despite conflicts |
| `reason` | string | `null` | Optional explanation shown in output |

This setting can be overridden per-run with the `--conflict-mode` CLI flag.

---

## `coverage`

Coverage and test-file policy, read by the CI pipeline (`abapgit-agent-ci`). Evaluated
per class — a class that meets the threshold is unaffected even if another fails.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `threshold` | number | `0` | Minimum coverage % per class. `0` disables coverage enforcement. |
| `mode` | `fail` \| `warn` | `fail` | `fail` = build FAILURE when gate fires; `warn` = build UNSTABLE |
| `excludes` | string[] | `[]` | Glob patterns for `.clas.abap` files exempt from both the test-file requirement and coverage threshold |

### Test-file requirement

The CI pipeline requires every touched `.clas.abap` to have a `.testclasses.abap` companion.
Files matching a pattern in `excludes` are silently skipped.

```json
"excludes": [
  "src/zcl_legacy_report.clas.abap",
  "src/zbp_*.clas.abap"
]
```

### Coverage threshold

When `threshold > 0`, a class whose coverage falls below the threshold gets a
`coverage_threshold` failure injected into its JUnit testsuite — the Jenkins test report
shows the failure under the class it belongs to, with the actual rate in the message
(e.g. `ZCL_MY_CLASS: coverage 20% is below threshold 80%`).

---

## `transports`

Controls transport creation, release, and selection behaviour.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `allowCreate` | boolean | `true` | Whether `abapgit-agent transport` may create new transports |
| `allowRelease` | boolean | `true` | Whether transports may be released |
| `reason` | string | `null` | Message shown when a transport operation is blocked |
| `hook.path` | string | `null` | Path to a JS script that returns the transport to use |
| `hook.description` | string | `null` | Human-readable description of the hook |

See [pull-transport-selection.md](development/pull-transport-selection.md) for details on
the transport hook interface.
