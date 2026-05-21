---
layout: default
title: Project Configuration (.abapgit-agent.json)
nav_order: 3
parent: Reference
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
  "inspect": {
    "variant": "MY_VARIANT",
    "exclude": ["zcl_generated_*"],
    "suppress": [
      { "object": "zcl_my_class", "message": "*pragma*" }
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

## `inspect`

Code Inspector settings. The variant is a **project-level agreement** — the Code Inspector
variant the whole team (and CI) uses for this codebase. By versioning it here, all developers
get the same checks without having to remember a `--variant` flag.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `variant` | string | `null` (system default) | Code Inspector variant name. Overridden by `--variant` CLI flag. |
| `exclude` | string[] | `[]` | Object name patterns to skip entirely. Supports `*` wildcard. Case-insensitive. |
| `suppress` | object[] | `[]` | Downgrade matching errors/warnings to info (visible but won't fail CI). |
| `warnings` | `info` \| `failure` | `info` | How warnings are encoded in JUnit XML. `info` (default): warnings become a single `<system-out>` block on a passed `<testcase>` — Jenkins keeps the build **GREEN**. `failure`: each warning becomes a `<failure type="Warning">` element — Jenkins marks the build **UNSTABLE** (legacy behaviour). Errors are unaffected and always fail the build. |

```json
"inspect": {
  "variant": "MY_VARIANT",
  "exclude": ["zcl_generated_*"],
  "warnings": "info",
  "suppress": [
    { "object": "zcl_my_class", "message": "*pragma*" },
    { "object": "zcl_*_legacy", "message": "*obsolete statement*" }
  ]
}
```

`suppress` entries have two fields:
- `object` — object name pattern (`*` wildcard, case-insensitive)
- `message` — error/warning text pattern (`*` wildcard, case-insensitive)

Suppressed findings are moved from errors/warnings to infos with a `[suppressed]` prefix — they still appear in the output for tracking but don't count as failures.

### Why `warnings: "info"` is the default

CI pipelines that run `abapgit-agent inspect --junit-output ...` use the Jenkins JUnit
plugin to interpret the report. Any `<failure>` element marks the build UNSTABLE.
Treating warnings as failures conflates "needs attention" with "the code is broken" —
which makes warning regressions impossible to distinguish from real syntax issues.

`info` mode keeps warnings visible (as `<system-out>` text on a passing testcase) without
flagging the build. Set `warnings: "failure"` if your project policy is strict and you
want every warning to fail the gate.

---

## `timeouts`

HTTP request timeouts (in **seconds**) for backend calls. Useful when a `pull` or `import` of many objects exceeds the default 2-minute window.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `default` | number | `120` | Fallback timeout for any command not explicitly listed. |
| `pull` | number | inherits `default` | Timeout for `pull` requests. Increase for repos with many objects or slow systems. |
| `<command>` | number | inherits `default` | Per-command override (any command name supported by abapgit-agent). |

```json
"timeouts": {
  "default": 180,
  "pull": 600,
  "import": 900
}
```

Values are in **seconds**. Non-positive or non-numeric values fall back to the 120s default. CLI flags do not yet override these — adjust `.abapgit-agent.json` to change them.

---

## `exclude`

Controls which files should be excluded from git tracking and filtered from `pull` and `import`.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `patterns` | string[] | `[]` | Glob patterns. **Without `/`** → matched against file basenames. **With `/`** → matched against the full relative path. Use `**` for cross-segment paths. Case-insensitive. |
| `unsupportedTypes` | boolean | `false` | Auto-exclude all files whose object type is not supported by abapgit-agent (CLAS, INTF, PROG, FUGR, TABL, DTEL, TTYP, DOMA, DDLS, DCLS, MSAG, STRU, ENHO, SUSO, PINF). Useful after `import` to remove types abapGit cannot activate. |

```json
"exclude": {
  "unsupportedTypes": true,
  "patterns": [
    "zcl_*_dpc*.clas.*",
    "zcl_*_mpc*.clas.*",
    "src/aud_legacy/**",
    "src/aud_*/cl_temp_*.clas.*"
  ]
}
```

The first two entries are basename patterns (no `/`) — they match files
anywhere in the repo whose filename matches. The last two entries are
path patterns (contain `/`) — `src/aud_legacy/**` excludes every file
under that folder; `src/aud_*/cl_temp_*.clas.*` mixes wildcards in the
directory portion with a basename glob.

Run `abapgit-agent exclude` after adding patterns to remove already-tracked files from git.
See [exclude command](setup/exclude-command.md) for details.

> **Path-based patterns and `import`**: the ABAP-side filter inside `import`
> only sees file basenames, so path patterns are silently skipped during
> `import` (a CLI warning is printed). They take effect for `exclude` and
> `pull --files` (where the JS-side filter has the full path). Run
> `abapgit-agent exclude` after `import` to prune the unwanted files from git.

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
