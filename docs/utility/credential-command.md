---
layout: default
title: credential - Configure Credentials and Adapters
nav_order: 5
parent: Utility Commands
---

# credential Command Requirements

**Status:** implemented on this branch for POSIX systems; Windows central mode fails closed. This specification follows the [Central Systems and Credentials design](../architecture/central-systems-client-logons.md#21-cli-adapter-dispatcher).

## Overview

Create central Git credential profiles, register custom helpers, and list the named CLI adapters available to central SAP-client and Git-profile credential configuration. `cli` uses registered external helpers only; users register them once in their personal catalog and reference them from several credential entries. Built-in Bitwarden is a separate local-serve provider, not a CLI adapter.

These operations never retrieve credentials, run an adapter, check vault status or test authentication. Creation writes configuration only; listing returns metadata only. Local executable availability does not mean that the tool is unlocked, authenticated or able to resolve a particular item.

## Command

```bash
abapgit-agent credential adapters
abapgit-agent credential adapters --json
abapgit-agent credential register <name> --executable <absolute-path> [--arg <prefix-arg>] [--json]
abapgit-agent credential create <git-profile> --origin <https-origin> --provider file --user <username> [--json]
abapgit-agent credential create <git-profile> --origin <https-origin> --provider bitwarden --item-id <UUID> [--json]
abapgit-agent credential create <git-profile> --origin <https-origin> --provider cli --adapter <custom-name> [--arg <lookup-arg>] [--json]
abapgit-agent credential --help
```

Support `adapters`, `create` (Git profiles) and `register` (custom CLI adapters). SAP-client credentials are created through [`logon create`](../setup/logon-command.md). With no subcommand, show usage without reading configuration. No login/unlock, credential-output, adapter installation, update/delete or vault-management operation is provided.

## Prerequisite

- No repository, SAP configuration, SAP connection or unlocked vault is required.
- A missing default catalog produces an empty adapter list; there are no built-in CLI adapters.
- Custom registrations come only from the personal catalog at `~/.config/abapgit-agent/systems.json`, or explicit `ABAP_SYSTEMS_PATH`.

For `adapters`, a missing default catalog is allowed: return an empty adapter list; a missing explicit catalog is an error. `create`/`register` may initialize a missing catalog at the selected path. An unreadable or invalid present file is always an error, never a reason to replace its contents.

## Parameters

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `adapters`, `create`, `register` | Yes | - | List adapters, create a Git profile, or register a custom helper |
| `<git-profile>` | For `create` | - | New nonempty name in `gitProfiles` |
| `<name>` | For `register` | - | New custom adapter name; provider names are reserved |
| `--origin` | For `create` | - | HTTPS origin with optional port; no path, query, fragment or userinfo |
| `--provider` | For `create` | - | `file`, `bitwarden` or `cli` |
| `--user` | For file creation | - | Nonempty Git username; password/token maintained manually later |
| `--adapter` | For CLI creation | - | Registered custom adapter |
| `--item-id` | For Bitwarden creation | - | Exact Login item UUID |
| `--executable` | For `register` | - | Trusted absolute helper/interpreter path |
| `--arg` | No; custom `create`/`register` | None | Literal lookup/prefix argument; repeat for multiple arguments |
| `--json` | No | Off | Return only operation-specific success/adapter metadata |
| `--help`, `-h` | No | - | Show usage before configuration access |

| Environment Variable | Default | Description |
|----------------------|---------|-------------|
| `ABAP_SYSTEMS_PATH` | `~/.config/abapgit-agent/systems.json` | Select the personal catalog containing custom registrations |

## Configuration

Register an external executable once with `credential register` (or maintain the same private catalog entry manually):

```json
{
  "version": 1,
  "cliAdapters": {
    "company-vault": {
      "executable": "/Users/me/bin/company-vault-credentials"
    }
  }
}
```

Names match `^[a-z][a-z0-9-]*$` and must be unique. Provider names `file`, `bitwarden` and `cli` are reserved adapter names. Executables must be explicit absolute paths. An optional string-array `args` prefix supports a trusted interpreter plus an absolute helper-script path. Missing argument arrays default to `[]`.

Reference the adapter from either a central SAP client's or Git profile's `credentials`:

```json
{
  "provider": "cli",
  "adapter": "company-vault",
  "args": ["sap/dev/200"]
}
```

The built-in Bitwarden provider needs no registration:

```json
{
  "provider": "bitwarden",
  "itemId": "11111111-1111-4111-8111-111111111111"
}
```

Bitwarden uses an exact Login UUID, not `adapter`, custom `args` or an executable override. The user unlocks and starts `bw serve`; the consumer needs no `BW_SESSION`. `BW_SERVE_URL` selects the local endpoint, defaulting to `http://[::1]:8087`; see the architecture for loopback validation and runtime bounds. No `bw` executable check, CLI transport or automatic server lifecycle is provided. The helper path and UUID are examples. No registrations or lookup arguments are accepted from repository policy files, repository-controlled scripts or Git URLs.

File and Bitwarden providers are not CLI adapters and are not rows in adapter-list output. Creation accepts all three providers and validates metadata only; it never checks the serve endpoint. No operation creates an item in an external vault.

## Tasks

### 1. Load Adapter Metadata

1. Handle help and validate arguments before reading configuration.
2. Start with an empty CLI adapter list; there are no built-in CLI adapters.
3. Read the selected/default catalog once if present. Validate JSON/version and custom registration names, uniqueness, absolute paths and string argument arrays. Reject reserved provider names.
4. Do not resolve client credentials or validate unrelated SAP systems/Git profiles. Do not scan directories, load custom modules or discover/download plugins.

Parsing a shared catalog may read plaintext secret fields into memory. Never echo the file or treat this as isolation between secret entries. Project only the output whitelist below.

### 2. Check Local Executable Availability

- Inspect only registered absolute executable paths and any explicit interpreter script path locally. Do not search PATH or check for `bw`.
- Report `available: true` only when the required local executable/files are present and satisfy the supported platform's launch requirements. Missing/nonexecutable files or unsupported launch formats give `available: false`, not a failed listing.
- Do not run the executable, version probes, a shell fallback or any other subprocess. Do not retrieve tool sessions or contact SAP/vault/network services, including the local Bitwarden status endpoint.

Availability is a local prerequisite check, not runtime verification. Supported-platform launch behavior still needs implementation tests.

### 3. Display Adapters

Sort adapters by name. Each entry contains exactly:

| Field | Values | Meaning |
|-------|--------|---------|
| `name` | Adapter name | Used by `credentials.adapter` |
| `source` | `custom` | Where the adapter is defined |
| `available` | Boolean | Whether its required local executable/files satisfy the local check |

Never include executable paths, arguments, item references, usernames, passwords, environment values, raw provider diagnostics or authentication status.

### 4. Create Git Credential Profile (`create`)

Validate the profile name, exact HTTPS origin and provider fields before writing. Canonicalize origin host/effective port for matching; reject URL paths, userinfo, queries/fragments and non-HTTPS origins. Add exactly one new `gitProfiles[name]` entry with `origin` and `credentials`. Reject duplicates; do not overwrite or change repository `gitProfile` references.

Use the same provider arguments and draft behavior as `logon create`: `file` saves the supplied nonempty `user` and `password: ""`; Bitwarden saves `provider: "bitwarden"` and an exact `itemId`; CLI saves its registered adapter name and literal lookup `args`. Check registration/schema only, not executability, vault status, remote credentials or repository access. Never accept a password/token argument, prompt, environment input or stdin secret.

Tell file-provider users to fill in the password/token manually in the private catalog. The draft is valid for setup/listing, but actual credential lookup must fail before the Git-consuming SAP request until its password is a nonempty string. Preliminary SAP compatibility/session checks may occur before lazy Git resolution; they never receive the Git credentials. Preserve manually entered password bytes. Bitwarden and CLI entries are configured, not authenticated. Reject CLI fields on Bitwarden entries and `itemId` on CLI entries.

### 5. Register Custom Adapter (`register`)

Validate the new name, absolute executable and optional literal prefix arguments against the registration schema. Add only `cliAdapters[name]`; reject duplicates and reserved provider names. Missing executables may be registered for later installation and will list as unavailable. This operation does not execute, install or download a helper and does not configure the built-in Bitwarden provider.

For both write operations, initialize version `1` only if the selected catalog is missing. Preserve unrelated sections/entries. Apply the private-permission, symlink-safe, concurrency-checked atomic-write requirements of [`system create`](../setup/system-command.md). There is no overwrite/`--force` or success output on failed writes.

`--arg` values are literal strings, with no templating or expansion by the agent. Use `--arg=--context` for an argument beginning with `-`; repeat in order. Never put passwords, tokens or session keys in helper arguments. Registration and lookup argument arrays default to `[]`.

## Output

### Human Readable

```text
CLI credential adapters

Name           Source    Available
company-vault  custom    no

Availability does not imply authentication or a usable credential lookup.
```

### JSON

```json
{
  "adapters": [
    { "name": "company-vault", "source": "custom", "available": false }
  ]
}
```

Without a default catalog or custom registrations, return `{"adapters":[]}`; human output says `No CLI adapters registered.` Bitwarden never appears in this list. JSON mode emits exactly one object on stdout without banners, progress text or additional fields.

### Creation/Registration

File creation reports: `Created Git credential profile work-git. Fill in its password/token manually in the private catalog before use.` JSON returns only:

```json
{
  "gitProfile": "work-git",
  "created": true,
  "passwordRequired": true
}
```

Bitwarden and CLI profiles return `passwordRequired: false`; authentication has not been tested. Registration reports the adapter name, without its path/arguments:

```json
{
  "adapter": "company-vault",
  "registered": true
}
```

The saved file-provider profile has this shape (documentation example, not command output):

```json
{
  "origin": "https://github.tools.sap",
  "credentials": {
    "provider": "file",
    "user": "I045696",
    "password": ""
  }
}
```

## Error Handling

| Error | Behavior |
|-------|----------|
| Unknown subcommand or unsupported option | Show usage and exit nonzero |
| Default catalog absent | Succeed with an empty adapter list |
| Explicit catalog absent for listing, unreadable file, invalid JSON or unsupported version | Fail with a sanitized diagnostic; only explicit write operations may initialize a missing catalog |
| Duplicate profile/registration, invalid origin/provider fields or password option | Fail before writing; no overwrite or secret collection |
| Unsafe path, concurrent edit or write failure | Leave existing catalog data unchanged; no success JSON |
| Empty file-provider password on actual lookup | Require manual password/token maintenance; no credential transmission or fallback |
| Invalid/duplicate registration name, reserved provider name, relative executable or invalid argument array | Fail before any tool execution; do not print configured arguments or catalog excerpts |
| Missing/nonexecutable tool or unsupported launch format | List the adapter as unavailable; listing still succeeds |
| Locked/missing session or invalid vault item | Not checked by listing; relevant only during credential resolution |

Help, missing-subcommand usage and successful listing exit zero. Errors exit nonzero with sanitized stderr diagnostics and no success JSON. Suppress raw parser/subprocess errors that could expose catalog fields.

## Credential Resolution Contract

Listing does not execute this contract. A later credential-bearing SAP/Git operation uses the shared dispatcher:

- Select the named adapter from trusted personal configuration. Custom invocation is exactly `executable` with `registration.args + credentials.args`, both string arrays; no shell, substitutions or environment expansion in arguments.
- Inherit the user's environment, close stdin and require prior user-managed authentication/unlock/sync. Never pass passwords, tokens or session keys as arguments. Trusted custom helpers run with user permissions/environment, not in a security sandbox.
- Require exit zero and exactly one UTF-8 JSON object with nonempty string `user` and `password`. Preserve password content; reject extra output, arrays, malformed/partial results and nonzero exits.
- Capture stdout/stderr privately, suppress raw errors/full arguments and enforce one fixed 15-second total lookup deadline with a combined 64-KiB output bound for the entire helper lookup. On timeout, terminate the helper's POSIX foreground process group. Helpers must not detach/daemonize their credential-bearing children; those launch patterns are unsupported.
- Fail closed with an adapter name and safe error category; never return partial credentials or fall back to another adapter/client. Never persist or print the credential result.
- Git-origin validation precedes the selected CLI credential lookup. Built-in Bitwarden does not execute this subprocess contract; its separate HTTP provider validates the UUID, unlocked status, Login item and bounded responses as specified in the architecture.

The command's JSON output contains **adapter metadata**, never the helper's `{user,password}` result. See the architecture for verified TLS, lazy resolution, session isolation and built-in Bitwarden local-serve restrictions. No dynamic plugin SDK or built-in CLI adapters are required.

## Examples

```bash
# Create a Git profile; fill the empty password/token manually in the catalog
abapgit-agent credential create work-git --origin https://github.tools.sap \
  --provider file --user I045696

# Register one helper for reuse across SAP-client and Git-profile entries
abapgit-agent credential register company-vault \
  --executable /Users/me/bin/company-vault-credentials

# Record a Bitwarden Login reference without contacting local serve
abapgit-agent credential create work-vault-git --origin https://github.tools.sap \
  --provider bitwarden --item-id 11111111-1111-4111-8111-111111111111

# Works without a repository or unlocked vault
abapgit-agent credential adapters

# Inspect registered CLI adapter availability without running tools
ABAP_SYSTEMS_PATH=/Users/me/.config/abapgit-agent/work-systems.json \
  abapgit-agent credential adapters --json
```

### User-created Bitwarden CLI adapter

If you prefer an already unlocked CLI session instead of a local vault HTTP server, see the [standalone Bitwarden CLI helper example](../../examples/credentials/README.md). It includes user-managed setup, custom registration, Git/SAP credential references and isolated tests. The helper uses the defined external-adapter contract; it remains separate from the central dispatcher.

## Related Commands

- [`system list`](../setup/system-command.md) - List central endpoint metadata
- [`logon`](../setup/logon-command.md) - Inspect/change the bound system's client selection
- [Central Systems and Credentials](../architecture/central-systems-client-logons.md) - Providers, dispatcher and security boundaries

## Acceptance Checks

- An absent default catalog or no registrations returns an empty adapter list; default/explicit catalog rules, sorting and exact text/JSON field whitelist are tested. Bitwarden is never a CLI row.
- Custom registrations are reused across SAP/Git entries; invalid names, collisions, paths/arguments and duplicate declarations fail safely.
- Local missing/unsupported tools show unavailable; locked vaults and missing sessions do not affect listing.
- Listing writes nothing; all operations avoid subprocess/status/version invocation, SAP/vault/network calls, credential resolution and sensitive output.
- Creation/registration initialize only missing catalogs or add one entry, preserving unrelated settings with private, safe atomic writes. Duplicates, reserved names, invalid origins/provider arguments, unsafe paths and concurrent/failed writes cannot destroy data.
- File profiles are created with an empty password without collecting secrets; later lookup rejects drafts before transmission. Bitwarden profiles validate exact UUIDs and reject CLI fields without any local-serve access; CLI profiles and registrations are validated without execution. Neither is claimed to be authenticated.
- Separate dispatcher tests use mock providers/dummy helpers for argument order, environment/closed stdin, no-shell behavior, exact JSON validation, password preservation, total timeout/output bounds, descendant cleanup and redacted failures. The unit suite includes local-process and local HTTP fixtures for these boundaries; it does not authenticate a real vault or verify every installed helper/platform.
