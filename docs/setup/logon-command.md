---
layout: default
title: logon - Configure and Select SAP Client
nav_order: 7
parent: Setup Commands
---

# logon Command Requirements

**Status:** implemented on this branch for POSIX systems; Windows central mode fails closed. This specification follows the [Central Systems and Credentials design](../architecture/central-systems-client-logons.md).

## Overview

Create centrally configured client logons, and inspect/select the SAP logon client within the development system bound to a repository. Each client uses its own centrally configured credentials, which may use file, built-in Bitwarden local-serve or registered CLI-helper providers.

`list`, `current`, `use` and `create` are local configuration/metadata operations. They do not log on to SAP, retrieve credentials, execute adapters or contact a vault. A configured selection does not prove that the account is authenticated or authorized.

Changing client changes logon/data/customizing context, not the ABAP code version: repository objects and conflict baselines are shared across clients of the same backend.

## Command

```bash
abapgit-agent logon list [--json]
abapgit-agent logon current [--json]
abapgit-agent logon use <client> [--json]
abapgit-agent logon create <client> [--system <name>] --provider file --user <username> [--json]
abapgit-agent logon create <client> [--system <name>] --provider bitwarden --item-id <UUID> [--json]
abapgit-agent logon create <client> [--system <name>] --provider cli --adapter <custom-name> [--arg <lookup-arg>] [--json]
abapgit-agent logon --help

# One-command override, before command dispatch; does not save the client
abapgit-agent --logon-client 300 status
```

With no subcommand, show usage without loading configuration. No runtime system-switching command is provided.

## Prerequisite

- `list`, `current` and `use` require an explicit central `system` binding in raw repository configuration.
- `create` uses explicit `--system` when supplied, otherwise the repository binding. Explicit `--system` allows setup outside a repository; it is a creation target, not a runtime deployment switch.
- The selected catalog defines that system. `list`/`current` require an effective selected client, `use` requires its positional target, and `create` requires that the new client does not already exist.
- `use` additionally requires the repository-local `.abapGitAgent` in the current repository root; do not write an installed-package or `CONFIG_PATH` fallback file.
- No SAP connection, backend version check or unlocked credential provider is required for metadata operations.

An old flat configuration remains supported by existing commands. Selection operations and explicit `--logon-client` require a central system binding. `create --system` can add a central entry without changing a legacy repository; do not auto-migrate or silently replace its SAP client.

## Parameters

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `list`, `current`, `use`, `create` | Yes | - | Select the local operation |
| `<client>` | For `use`/`create` | - | Exactly three digits, including leading zeros; existing for `use`, new for `create` |
| `--system` | No; `create` | Repository binding | Central system receiving the new client entry; no rebind |
| `--provider` | For `create` | - | `file`, `bitwarden` or `cli` |
| `--user` | For file creation | - | Nonempty SAP username; password is filled in manually later |
| `--adapter` | For CLI creation | - | Registered custom adapter |
| `--item-id` | For Bitwarden creation | - | Exact Login item UUID |
| `--arg` | No; custom CLI creation | None | Literal lookup argument; repeat in order, use `--arg=--flag` for leading `-` |
| `--json` | No | Off | Return the operation's metadata as JSON |
| `--help`, `-h` | No | - | Show usage before configuration access or validation |
| `--logon-client <client>` | No; global | - | Override the effective logon client for one invocation within the bound system |

| Environment Variable | Default | Description |
|----------------------|---------|-------------|
| `ABAP_SYSTEMS_PATH` | `~/.config/abapgit-agent/systems.json` | Select the personal catalog |
| `ABAP_LOGON_CLIENT` | Unset | Override effective selection in central SAP mode only; ignored by legacy SAP mode |

The global option is parsed before command dispatch, connection gates and version checks. Reject missing values, repeated overrides and values other than exactly three digits. Do not reinterpret a command-specific `--client`, `--source-client` or `--target-client` as a global logon selector.

## Tasks

### 1. Resolve Local Selection

Read the raw settings/catalog needed for the bound system, without materializing credentials. Validate the system binding, client keys and provider metadata; reject ambiguous central/flat SAP settings rather than merging credentials. Unrelated Git profiles and authentication readiness are not checked.

For `list`, `current` and SAP command execution, effective selection is:

1. Explicit global `--logon-client`.
2. `ABAP_LOGON_CLIENT`.
3. Repository `client`.

Missing, malformed or unknown effective clients fail. Do not select the first configured client, inherit another client's credentials or fall back after an invalid explicit override. Selection source is `cli`, `environment` or `repository` in JSON output.

### 2. List Clients (`list`)

Sort configured client IDs and display provider metadata for the bound system, marking the effective selection. Include a CLI adapter name only for `provider: "cli"`; do not show usernames, passwords, item IDs, lookup arguments or tool environment values.

### 3. Report Effective Selection (`current`)

Return only the bound system alias, effective client and selection source. Do not retrieve the selected account's username or check whether it can log on.

### 4. Save Repository Selection (`use`)

1. Validate the positional client against the repository's bound system; this target does not come from environment/global overrides.
2. Update only the raw repository file's `client` field. Preserve `system`, `gitProfile`, package/folder/workflow settings, all `clientSettings` and unrelated fields. Never serialize a resolved connection object.
3. Use a safe atomic write in the same directory with private permissions/appropriate ACLs. Reject symlinks/unsafe paths and detect concurrent edits; leave the original file intact on failure.
4. If already saved, report success without rewriting. Never edit the central catalog or repository policy file.

Saving `200` does not cancel an active environment/global override. Output reports the **saved** client; `current` reports the effective client. No credentials, sessions or conflict metadata are created, reset or modified.

Each repository remembers its default in its own ignored `.abapGitAgent` `client` field, not in the shared catalog or session cache. `logon use` is the explicit persistence operation. CLI/environment overrides and secondary SO10 logons do not save a last-used client or history. A later invocation without an override uses the saved repository client again. `logon current --json` reports the effective client and its source (`cli`, `environment` or `repository`).

### 5. Create Client Logon (`create`)

1. Resolve the creation target from `--system` or the repository binding, never from a runtime client override. Validate that system, the three-digit client and the provider-specific arguments. Do not require a currently selected client or resolve its credentials.
2. Add only `systems[targetSystem].clients[newClient].credentials` to the existing catalog. Reject existing clients and conflicting/unknown provider fields; no overwrite or `--force`.
3. For `file`, save the supplied nonempty `user` and `password: ""`. Do not accept a `--password` option, prompt for a password or read one from stdin/environment. Tell the user to fill in the password manually in the private catalog.
4. For Bitwarden, save only `provider: "bitwarden"` and the validated exact Login `itemId`; for CLI, save `provider: "cli"`, a registered `adapter` and literal `args` (default `[]`). Check provider schema/registration only, without executing tools, checking local serve, resolving secrets or validating authentication. Reject CLI fields on Bitwarden entries and `itemId` on CLI entries.
5. Preserve all other catalog entries using the same private, symlink-safe, concurrency-checked atomic-write rules as [`system create`](system-command.md). Do not write the repository config or implicitly select the new client; use `logon use` separately.

An empty file password is a deliberate setup draft, not a usable credential. List/current/use may inspect/select it, but an actual SAP credential lookup fails before SAP contact until the password is a nonempty string. Preserve the manually entered password exactly when resolving it. Creation does not add transport/client settings or create an account in SAP.

Creation accepts `file`, `bitwarden` and `cli`. Built-in Bitwarden uses user-managed local serve only, not a CLI adapter. The user unlocks and starts `bw serve` before credential resolution; the consumer needs no `BW_SESSION` or `bw` executable check. `BW_SERVE_URL` defaults to `http://[::1]:8087`; the architecture defines validation and bounded lookup. Setup/list/current/use never contact that endpoint.

### 6. Apply Selection to SAP Commands

Before SAP contact, resolve only the selected client's credentials and applicable `clientSettings` into an immutable connection snapshot. Use verified HTTPS for centrally managed credentials. Pin nested commands, re-pulls, hooks and debugger sessions to that snapshot; a later selection does not retarget an active session. Git profiles are independent and resolved lazily for the actual Git destination.

In central SAP mode, absent client settings mean no configured transport and the selected client as customizing default; do not inherit another client's transport. SO10 TRANSPORT's secondary client is explicit `--target-client`, then primary `customizingClient`, then primary client, with separate credentials/settings from the same system. SO10 COPY/READ retain their existing data-client semantics. See the architecture for the async secondary-logon boundary and legacy compatibility requirements.

## Output

### List (Human Readable)

```text
SAP clients for DEV

Client  Provider   Adapter  Selected
100     file       -
200     bitwarden  -        *
300     file       -

Selected client: 200 (environment)
```

### List (JSON)

```json
{
  "system": "DEV",
  "client": "200",
  "source": "environment",
  "clients": [
    { "client": "100", "provider": "file", "selected": false },
    { "client": "200", "provider": "bitwarden", "selected": true },
    { "client": "300", "provider": "file", "selected": false }
  ]
}
```

### Current (JSON)

```json
{
  "system": "DEV",
  "client": "300",
  "source": "cli"
}
```

Human output reports the same three fields, for example `System: DEV`, `Client: 300`, `Source: cli`.

### Use (JSON)

```json
{
  "system": "DEV",
  "savedClient": "200",
  "changed": true
}
```

Human output is `Saved logon client 200 for DEV.` An unchanged selection returns `changed: false`. JSON mode emits exactly one object on stdout, without banners or progress text.

### Create

Human output for a file entry: `Created logon DEV/100. Fill in its password manually in the private catalog before use.` Bitwarden and CLI entries report only successful configuration, not authentication readiness.

```json
{
  "system": "DEV",
  "client": "100",
  "created": true,
  "passwordRequired": true
}
```

Bitwarden and CLI creation return `passwordRequired: false`; neither has tested the provider. The file-provider credentials saved under the new client are:

```json
{
  "provider": "file",
  "user": "DEVELOPER",
  "password": ""
}
```

This block documents stored data; the command never dumps credential objects into terminal output.

## Error Handling

| Error | Behavior |
|-------|----------|
| Unknown subcommand, unsupported option, missing/invalid client or repeated global override | Show safe usage/validation diagnostics; no write or SAP contact |
| No central system binding for selection, or no binding/explicit `--system` for creation | Explain the required central target; leave legacy config unchanged |
| Missing/unreadable/invalid catalog or unknown system/client | Fail without legacy/client fallback or raw catalog output |
| Competing flat SAP endpoint/credential or transport settings | Reject ambiguous configuration as specified by the architecture |
| Missing repository-local file for `use`, unsafe path, concurrent edit or write failure | Fail without creating/mutating a fallback file or losing existing settings |
| Duplicate client, invalid create provider/arguments or password argument | Fail before writing; no overwrite, secret collection or provider execution |
| Empty draft file password on actual credential lookup | Explain that the password must be maintained manually; no SAP contact or fallback |
| Locked vault, unavailable adapter or rejected SAP authorization | Not tested by metadata commands; credential-bearing commands fail when resolution/execution is attempted |

Help, missing-subcommand usage and successful operations exit zero. Errors exit nonzero with sanitized stderr diagnostics and no success JSON. Never dump raw config, credentials or provider errors.

## Examples

```bash
# Add a file-backed client; fill its empty password manually in the catalog
abapgit-agent logon create 100 --system DEV --provider file --user DEVELOPER

# Add an exact Bitwarden reference without unlocking/querying the vault
abapgit-agent logon create 200 --system DEV --provider bitwarden \
  --item-id 11111111-1111-4111-8111-111111111111

# Inspect and save a client without contacting SAP or the credential tool
abapgit-agent logon list
abapgit-agent logon use 200
abapgit-agent logon current --json

# Override only this invocation; the saved repository client remains 200
abapgit-agent --logon-client 300 status

# Environment selection takes precedence over the saved client
ABAP_LOGON_CLIENT=300 abapgit-agent logon current --json

# Show exactly what the explicit global override selects
abapgit-agent --logon-client 100 logon current --json
```

## Related Commands

- [`system list`](system-command.md) - List centrally defined endpoints
- [`credential adapters`](../utility/credential-command.md) - List adapter metadata
- [`init`](init-command.md) - Preserve central references when updating repository settings
- [`so10`](../development/so10-command.md) - Data clients and secondary customizing logons have separate semantics

## Acceptance Checks

- Precedence/source reporting, leading-zero clients and one-command overrides match text/JSON output; invalid explicit selections never fall back.
- Legacy configuration remains unchanged, ignores `ABAP_LOGON_CLIENT` and rejects explicit central-only selection.
- Metadata commands work with locked/unavailable tools, execute no adapters and make no SAP/vault/version requests.
- `use` changes only the saved client, preserves unrelated settings/permissions, handles no-op writes and refuses symlinks, concurrent edits and fallback-file writes safely.
- Overrides do not affect positional `use`/`create` targets; saving under an override accurately distinguishes saved/effective selections.
- Creation adds only one new client to the intended system, preserves catalog contents and leaves repository selection/settings unchanged. Duplicate/unsafe/concurrent writes fail safely.
- File creation writes a nonempty username and empty password without collecting/logging secrets; metadata operations permit this draft, while actual resolution rejects it before SAP contact. Bitwarden creation stores only its provider/UUID and rejects CLI fields without contacting local serve; CLI creation validates registered references without tool execution.
- Connection and transport snapshots remain pinned, existing command-specific client options retain their semantics, and no secret is printed/persisted or conflict baseline reset.
