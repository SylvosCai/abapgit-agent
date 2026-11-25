---
layout: default
title: system - Manage Central Systems
nav_order: 6
parent: Setup Commands
---

# system Command Requirements

**Status:** implemented on this branch for POSIX systems; Windows central mode fails closed. This specification follows the [Central Systems and Credentials design](../architecture/central-systems-client-logons.md).

## Overview

Create and list the SAP systems defined in the personal central catalog. Endpoints are maintained once and reused by repositories through a `system` reference in their ignored `.abapGitAgent` files.

These are local configuration/metadata operations. It does not connect to SAP, resolve credentials, execute credential adapters or query a vault. Listing a system is not proof that it is reachable or that a user can log on.

## Command

```bash
abapgit-agent system list
abapgit-agent system list --json
abapgit-agent system create <name> --host <hostname> [--port 443] [--language EN] [--json]
abapgit-agent system --help
```

Support `list` and `create`. With no subcommand, show usage without loading configuration. There is no `system use`, overwrite/update/delete operation or runtime `--system` deployment switch.

## Prerequisite

- A personal catalog at `~/.config/abapgit-agent/systems.json`, or the path explicitly selected by `ABAP_SYSTEMS_PATH`.
- Catalog version `1` and a `systems` object with valid endpoint definitions.
- No repository binding, Git remote, SAP connection or unlocked credential tool is required.

An empty `systems` object is valid. `list` requires an existing catalog. `create` may initialize a missing catalog at the selected path, including an explicit `ABAP_SYSTEMS_PATH`; an unreadable or invalid existing file must never be replaced.

## Parameters

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `list`, `create` | Yes | - | List endpoints or create a central system |
| `<name>` | For `create` | - | Nonempty system alias; reject an existing alias |
| `--host` | For `create` | - | Hostname/IP, not a URL |
| `--port` | No; `create` | `443` | Integer from 1 to 65535, stored as `sapport` |
| `--language` | No; `create` | `EN` | SAP logon language, stored on the system entry |
| `--json` | No | Off | Return metadata as JSON |
| `--help`, `-h` | No | - | Show usage before any configuration access |

| Environment Variable | Default | Description |
|----------------------|---------|-------------|
| `ABAP_SYSTEMS_PATH` | `~/.config/abapgit-agent/systems.json` | Select the personal catalog; does not bind a repository to a system |

## Tasks

### 1. Read Central Metadata

1. Handle help and validate command arguments before reading configuration.
2. Read the selected catalog once. Validate its version, `systems` object and endpoint metadata needed for this listing.
3. Require `protocol: "https"`, a hostname/IP rather than a URL, and an integer `sapport` from 1 to 65535. Apply endpoint defaults of port 443 and language `EN` when omitted.
4. Do not require or resolve client credentials, validate unrelated Git profiles or test adapter availability.

The catalog can contain plaintext secrets. Reading it is not per-entry secret isolation: keep its contents private and project only the allowed metadata fields into output and diagnostics.

### 2. List Systems

Sort aliases by name and display each system's alias, protocol, host, port and language. Do not include clients, usernames, passwords, Git profiles, item references, adapter arguments or raw catalog data.

No SAP/backend compatibility check, vault status call, subprocess, remote probe or configuration write occurs.

### 3. Create System (`create`)

Validate arguments and add exactly one endpoint with `protocol: "https"`, the supplied host, port/language defaults and an empty `clients` object. Do not create a SAP account, resolve credentials or automatically bind/rebind a repository.

If the catalog is missing, create version `1` with a `systems` mapping at the selected path. Preserve all existing sections and entries when adding to an existing catalog. Reject duplicate aliases; there is no implicit overwrite or `--force`.

Use private directory/file permissions (`0700`/`0600` on POSIX; Windows central mode currently fails closed), a safe same-directory atomic write, symlink/unsafe-path rejection and concurrent-edit detection. Leave an existing file unchanged on validation/write failure. Never print the catalog or unrelated secrets.

Then use [`logon create`](logon-command.md) to add client credentials. Creating an empty system is valid; it cannot be used for a SAP connection until a configured client has usable credentials.

## Output

### Human Readable

```text
Central SAP systems

System  Protocol  Host                  Port  Language
DEV     https     dev.example.internal  443   EN
```

For an empty catalog, print `No central SAP systems configured.` and exit successfully.

### JSON

```json
{
  "systems": [
    {
      "name": "DEV",
      "protocol": "https",
      "host": "dev.example.internal",
      "sapport": 443,
      "language": "EN"
    }
  ]
}
```

An empty list returns `{"systems":[]}`. JSON mode emits exactly one JSON object on stdout, without banners or progress text.

### Create

Human output: `Created central SAP system DEV. Add a client with logon create.`

```json
{
  "system": "DEV",
  "created": true
}
```

Only success metadata is returned, not raw configuration. A new catalog entry has this shape:

```json
{
  "protocol": "https",
  "host": "dev.example.internal",
  "sapport": 443,
  "language": "EN",
  "clients": {}
}
```

## Error Handling

| Error | Behavior |
|-------|----------|
| Missing/unknown subcommand or unsupported option | Show usage; invalid arguments exit nonzero |
| Missing catalog for `list`, or unreadable existing catalog | Report a safe catalog-access error and how to configure `ABAP_SYSTEMS_PATH` |
| Duplicate alias, invalid create arguments, unsafe path, concurrent edit or write failure | Fail without overwriting existing entries; missing catalogs are initialized only by explicit `create` |
| Invalid JSON, unsupported version or invalid system metadata | Fail without dumping catalog contents or parser excerpts |
| Missing provider executable or locked vault | Not checked by this command; does not prevent endpoint listing |

Help and successful listing/creation exit zero. Operational errors exit nonzero, use sanitized stderr diagnostics and emit no success JSON. Missing subcommand shows usage only and exits zero.

## Examples

```bash
# Create an endpoint without creating accounts or storing passwords
abapgit-agent system create DEV --host dev.example.internal
abapgit-agent logon create 100 --system DEV --provider file --user DEVELOPER

# List shared endpoints from the default personal catalog
abapgit-agent system list

# List endpoints from a separately managed catalog
ABAP_SYSTEMS_PATH=/Users/me/.config/abapgit-agent/work-systems.json \
  abapgit-agent system list --json
```

To bind a repository, manually set `system` and `client` references in its ignored `.abapGitAgent`, following the architecture document. Binding is a deliberate configuration change, not a safe cross-system deployment operation. Use SAP transports for downstream deployment.

## Related Commands

- [`logon`](logon-command.md) - Inspect or change the selected client within a bound system
- [`credential adapters`](../utility/credential-command.md) - List available credential adapter metadata
- [`init`](init-command.md) - Initialize repository configuration; central references must remain intact

## Acceptance Checks

- Default/explicit catalog selection and endpoint defaults produce the specified sorted text/JSON output.
- Empty systems succeed; missing catalogs fail for listing but can be initialized by creation. Invalid JSON/version/endpoints and unknown arguments fail safely.
- Listing works outside a repository and with unavailable/locked credential tools.
- Listing performs no writes; neither operation makes SAP/vault requests, compatibility probes or adapter calls, and no secret-bearing fields/raw errors appear in output.
- Creation initializes only an explicitly selected missing catalog or adds one entry to a valid existing catalog, preserving all unrelated settings. Duplicates, unsafe paths, concurrent edits and failed writes cannot overwrite existing data.
