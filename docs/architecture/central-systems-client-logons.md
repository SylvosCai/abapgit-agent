---
layout: default
title: Central Systems and Credentials
nav_order: 3
parent: Architecture
grand_parent: Reference
---

# Central systems with per-client logon credentials

**Status:** implemented on this branch for POSIX systems with foreground-only custom helpers (support scope approved 2026-11-23); not yet release-verified. Windows central mode fails closed rather than using permissive ACLs or incomplete process-tree cleanup. Verified-TLS SAP and provider-backed Bitwarden smoke checks passed for GZE/client 100. SAP-to-Git redirect behavior is an upstream follow-up, not a release criterion for this change (scope clarified 2026-11-24); no redirect-safety guarantee is claimed. Local fixtures and unit tests are not evidence for untested deployed integrations. The original design was reviewed against commit `527f2e85ad14be81c2e3411b8fa8c076bc675067` on 2026-11-18. Supersedes the earlier general multi-system switching proposal. Retains central reuse while keeping one development system bound to each repository. Provider/adapter design updated on 2026-11-19: built-in Bitwarden uses user-managed local serve only; CLI adapters are registered external helpers.

## 1. Model

```text
Personal central catalog
  System DEV: endpoint defined once
    Client 100: file credentials
    Client 200: Bitwarden credentials
    Client 300: file credentials
  System OTHER_DEV: endpoint defined once
    Client 100: Bitwarden credentials

Repository A → DEV, selected client 100
Repository B → DEV, selected client 200
Repository C → OTHER_DEV, selected client 100
```

Multiple repositories reference the same system entry. Changing DEV's host/port or client 200's credential reference changes it once for all future invocations in those repositories. SAP credentials are configured **per system/client**, not duplicated in each repository, and providers may differ between clients of one system. The same personal catalog also holds independent named **Git credential profiles**, reusable across repositories and SAP systems. Existing flat `.abapGitAgent` files continue to work unchanged; using the central catalog is opt-in.

This is not an A → B code-deployment feature. Each repository is bound to its development system; moving its code to downstream systems remains a SAP transport workflow. Switching its client changes logon/data/customizing context, not the cross-client ABAP code version.

## 2. Central personal configuration

Default: `~/.config/abapgit-agent/systems.json`. An explicit `ABAP_SYSTEMS_PATH` can select another catalog for CI or a separate personal setup.

```json
{
  "version": 1,
  "cliAdapters": {
    "company-vault": {
      "executable": "/Users/me/bin/company-vault-credentials"
    }
  },
  "systems": {
    "DEV": {
      "protocol": "https",
      "host": "dev.example.internal",
      "sapport": 443,
      "language": "EN",
      "clients": {
        "100": {
          "credentials": {
            "provider": "file",
            "user": "DEVELOPER",
            "password": "<local secret; never commit>"
          }
        },
        "200": {
          "credentials": {
            "provider": "bitwarden",
            "itemId": "11111111-1111-4111-8111-111111111111"
          }
        },
        "300": {
          "credentials": {
            "provider": "file",
            "user": "CUSTOMIZER",
            "password": "<local secret; never commit>"
          }
        }
      }
    }
  },
  "gitProfiles": {
    "work-git": {
      "origin": "https://github.tools.sap",
      "credentials": {
        "provider": "bitwarden",
        "itemId": "22222222-2222-4222-8222-222222222222"
      }
    },
    "other-git": {
      "origin": "https://git.example.internal:8443",
      "credentials": {
        "provider": "file",
        "user": "GIT_USER",
        "password": "<PAT or password; never commit>"
      }
    }
  }
}
```

The endpoints, helper path and UUIDs are examples, not real values. File credentials live directly in this private central file. `cli` selects a named adapter through the dispatcher, not an executable path in each credential entry. Built-in `bitwarden` is a provider, not a CLI adapter. The user unlocks and starts local `bw serve` beforehand; abapgit-agent neither requires `BW_SESSION` nor starts the server.

This provider schema replaces the earlier proposal's Bitwarden CLI mode and optional serve transport. Bitwarden now uses local serve only; `cli` is reserved for registered external helpers. Central configuration has not shipped, so no released central schema is being migrated; compatibility with existing flat `.abapGitAgent` files remains unchanged.

Rules:

- System entries own endpoint fields. Client entries cannot override host/protocol/port.
- Validate catalog version `1`, plain-object sections and explicitly selected references. Endpoint `host` is a hostname/IP rather than a URL, `sapport` is an integer from 1 to 65535, and central SAP connections use HTTPS. Missing port/language use 443/EN. Client/provider entries cannot supply endpoint overrides.
- Each client key is exactly three digits; each has exactly one supported provider: `file`, `bitwarden`, or `cli`. Resolved credentials must contain nonempty string `user`/`password` fields; preserve password bytes rather than trimming or coercing them. Validate CLI entries against their selected adapter's schema; reject unknown adapters or competing provider fields before execution.
- Never inherit credentials from another client or fill missing fields from legacy environment variables.
- Fetch only the selected client's Bitwarden Login item, by exact UUID; use its username/password. Do not search by item name or enumerate the vault.
- Different clients may use the same vault item only if the user deliberately configures that reuse. Do not infer it.
- The first version maintains one configured account per system/client. Multiple named accounts for the same client can be added only if needed; they are not necessary to meet the present requirement.

### 2.1. CLI adapter dispatcher

The shared credential resolver selects a provider; only `provider: "cli"` dispatches by adapter name:

```text
SAP client / Git profile
  → credential resolver
      file      → private catalog credentials
      bitwarden → built-in provider → user-started local bw serve
      cli       → CLI dispatcher → registered external executable
  ← validated { user, password }
```

There are no built-in CLI adapters in the first version. Bitwarden uses the separate provider in section 2.2, not a `bw` subprocess. No executable or session lookup is needed to list CLI adapters.

**Custom adapters:** register a named executable once under `cliAdapters` in the personal catalog. Names must match `^[a-z][a-z0-9-]*$` and be unique. Provider names `file`, `bitwarden` and `cli` are reserved adapter names to prevent confusing the two namespaces. The dispatcher does not load user JavaScript modules into its process, scan directories or download plugins. There is no plugin SDK/versioning system; the external executable uses the contract below.

A custom adapter credential entry, valid for either a SAP client or Git profile:

```json
{
  "provider": "cli",
  "adapter": "company-vault",
  "args": ["sap/dev/200"]
}
```

The registration's executable is maintained once; credential entries contain only the selected adapter and its lookup arguments. Registration may optionally contain a string-array `args` prefix, for example to run a helper script with a trusted Python executable. Both argument arrays default to `[]` and must contain only strings. Invoke exactly `executable` with `registration.args + credentials.args`; do not interpolate templates, expand environment variables in arguments or construct a shell command. Bitwarden provider entries use only `provider` and `itemId`, not `adapter`, `args` or executable overrides. The central catalog is the only source of registrations and lookup parameters, never `.abapgit-agent.json`, repository-controlled scripts or an untrusted Git URL.

**Executable resolution:** custom registrations use an explicit absolute executable path (and absolute script path when using an interpreter). Do not search PATH, add the repository directory or its `node_modules/.bin`, or probe `bw`. Resolve the registered executable once per invocation and retain that choice for nested requests. Missing or unsupported executable launch formats fail clearly; do not introduce a shell fallback for scripts or Windows command wrappers. Platform/installed-CLI compatibility is an implementation verification requirement.

**External helper contract:**

- Receives configured string arguments and inherits the user's process environment, including an already established tool session. Never put passwords, tokens or session keys into CLI arguments or adapter registrations; file-provider secrets remain subject to the private-catalog rules. Use item references as literal lookup arguments, not as shell substitutions.
- Runs noninteractively with closed stdin. Authentication/unlock/sync is managed by the user beforehand, not by the dispatcher or supported helper contract.
- Exits zero and writes exactly one UTF-8 JSON object with nonempty string `user` and `password` to stdout. Whitespace around the object is allowed; extra output, arrays, partial fields or invalid JSON fail validation. Preserve password content exactly.
- Exits nonzero on failure and emits only sanitized diagnostics. The dispatcher also suppresses raw stdout/stderr and subprocess error objects; it reports the adapter name and a safe error category, not credential values or full argument lists.
- The dispatcher captures output privately, never streams it into terminal/logs, and enforces a fixed 15-second total lookup deadline and combined 64-KiB stdout/stderr capture bound. Failure/timeout/overflow stops the credential-bearing operation; no fallback to another adapter/client or partial credential result. Timeout handling terminates the spawned helper's POSIX process group, including descendants that stay in it. Supported helpers must not daemonize, disown or create separate sessions/process groups for credential-bearing children. This is not hostile-process containment; helpers requiring detached children need stronger platform isolation before support.

Any executable can implement this contract; wrappers normalize tools that cannot produce the required JSON directly. The [user-created Bitwarden CLI example](../../examples/credentials/README.md) demonstrates an optional helper using an already unlocked CLI session without local serve; it is not a built-in adapter or a dispatcher implementation. Users maintain their custom adapter; we provide the built-in Bitwarden provider and this external-helper contract. No speculative tool-specific CLI adapters are included. External helpers execute trusted code with the user's permissions; a separate process is not a security sandbox. Environment inheritance can expose sessions for more than one vault to a custom helper and is part of this trust boundary.

Existing repository transport hooks remain trusted, in-process code: they receive the resolved connection and authenticated HTTP client. Central registration ownership is not isolation from repository hooks; run credential-bearing commands only in repositories whose hooks you trust.

### 2.2. Built-in Bitwarden provider (local serve only)

Use `{"provider":"bitwarden","itemId":"<UUID>"}` for an exact Login item. No adapter registration, `mode` selector, catalog URL setting or CLI transport is provided. This replaces an unimplemented proposal, not a released configuration format.

**User setup:** the user logs in/unlocks Bitwarden and starts `bw serve` locally before a credential-bearing command. The existing `bwserve` shell helper performs this setup; it is not an executable that abapgit-agent invokes. The running server retains its session, so a consumer terminal needs no `BW_SESSION`. abapgit-agent never logs in, unlocks, syncs, starts/stops a server or collects the master password. If unavailable or locked, report a sanitized instruction to unlock/start the server and stop the credential-bearing operation.

**Endpoint:** read `BW_SERVE_URL` when the provider is first needed, defaulting to `http://[::1]:8087`, and pin the validated endpoint for the invocation, including secondary logons and re-pulls. An override changes the local port only: require literal IPv6 loopback `[::1]`, HTTP and a valid port (1–65535); reject other hosts, userinfo, paths other than `/`, queries/fragments, redirects and proxies. Never resolve a remote hostname or forward a request to a remote vault. Local HTTP is a same-machine transport exception; centrally managed SAP/Git recipients still require verified HTTPS.

**Lookup:**

1. Validate the exact UUID before network access. Reject competing credential fields such as `adapter`, `args`, `user` or `password`.
2. `GET /status`: require a successful HTTP/API envelope and `data.template.status === "unlocked"`.
3. `GET /object/item/<UUID>`: require a successful envelope, matching item ID, Login item type and nonempty string `data.login.username`/`data.login.password`. Map them to `{user,password}` and preserve password bytes exactly. Secure Notes, attachments and arbitrary custom-field extraction are unsupported.
4. Capture responses privately; status and item requests share one 15-second total deadline and a combined 64-KiB response-body bound. Abort pending requests on timeout/overflow. Reject malformed responses/failed API envelopes and suppress raw bodies/errors; never enumerate/search the vault, persist secrets, fall back to CLI/file credentials or expose credentials in command output.

**Exposure:** local serve exposes the entire unlocked vault to other local processes over plaintext loopback HTTP. Exact-item lookup limits our requests, not the server's access scope. The server can outlive its starting terminal; the user must stop it (for the existing helper, `bwserve stop`) when finished. Metadata-only commands never check status or contact this endpoint.

## 3. Minimal repository configuration

Continue using ignored `.abapGitAgent`:

```json
{
  "system": "DEV",
  "client": "100",
  "gitProfile": "work-git",
  "package": "$MY_PACKAGE",
  "folder": "/src/",
  "folderLogic": "PREFIX",
  "clientSettings": {
    "100": {
      "transport": "DEVK900123",
      "customizingClient": "300"
    },
    "200": {},
    "300": {
      "transport": "DEVK900456"
    }
  }
}
```

No endpoint, username, password or Bitwarden item is copied into the repository. Other repositories only repeat the **references** (`system`, `gitProfile`) and their selected client, not the system or credential definitions. The Git profile is independent of the SAP system/client: switching client does not switch Git credentials.

Keep package/folder/workflow settings and transport requests project-local: different repositories can target different packages/tasks even though their system and credentials are shared. In central SAP mode, resolve `transport` and `customizingClient` only from `clientSettings[selectedClient]`; absent settings mean no configured transport and the selected client as customizing default. Reject competing top-level `transport`, `customizingClient` and `customizingLogon` fields rather than accidentally inheriting a development-client task. Explicit command transport arguments and existing project transport policies/hooks retain their current behavior against the selected snapshot.

For SO10 TRANSPORT, select the secondary client by explicit `--target-client`, then the primary client's `customizingClient`, then the primary client. Resolve a different client's credentials asynchronously from the same central system, and its transport only from that client's settings; never change the primary logon. Adapt the current synchronous `transportProfile(config, targetClient)` boundary instead of assuming that it already understands central references. COPY/READ data-client options keep their existing behavior and must not silently become secondary logons. Legacy SAP mode, including Git-only migration, retains today's flat transport/customizingLogon behavior.

`.abapgit-agent.json` remains the separate repository policy/safeguard file; it is not a credential catalog. Legacy flat `.abapGitAgent` remains supported. A central `system` reference together with flat endpoint/credentials is ambiguous and must be rejected rather than silently merged.

### 3.1. Legacy compatibility and gradual migration

Retain the existing `.abapGitAgent` filename and flat schema, including `host`, `sapport`, `client`, `user`, `password`, `protocol`, `language`, `gitUsername`, `gitPassword`, transport and workflow settings. No migration, renaming, central file, Bitwarden installation or new fields are required to keep using an old config.

Choose SAP and Git sources independently:

| Repository configuration | SAP credentials | Git credentials |
|---|---|---|
| Old flat config, no new references | Existing local fields/environment behavior | Existing `gitUsername`/`gitPassword` behavior |
| `system` only | Central system/client | Existing legacy Git fields |
| `gitProfile` only | Existing legacy SAP fields | Central Git profile |
| Both references | Central system/client | Central Git profile |

The mere presence of a central catalog must not change an old repository's behavior. With `system`, reject competing flat SAP endpoint/user/password fields, but continue accepting `client` as the selected central client. With `gitProfile`, reject competing local Git credential fields. This detects ambiguous mixed credentials without preventing independent gradual migration. Unknown references or locked providers fail closed; do not fall back to legacy secrets after a central reference was explicitly selected.

Preserve current legacy discovery and caller behavior, including project-file-first lookup, package/`CONFIG_PATH` fallback and environment-only fallback, until a separately approved precedence change is made. Existing `getAbapConfig()` Git-environment fallbacks must be characterized by regression tests rather than silently generalized to every caller. New central resolution is an explicit mode, not an excuse to rewrite unrelated legacy precedence or TLS behavior.

### 3.2. Central Git credentials

A Git profile contains an exact HTTPS origin (protocol, hostname, effective port) and one file/Bitwarden/CLI-adapter credential source. Every source returns `user`/`password`, mapped to Git username and PAT/password. The Bitwarden provider normalizes its Login fields into this shape; custom adapters do the same for their tools. Never reuse the SAP password as a Git token, or key Git credentials by SAP client.

Resolve Git credentials only when a command needs authenticated Git access, after its actual destination URL is known—including `pull --url` overrides. Require the URL's origin to match the selected profile before releasing the secret; reject embedded credentials, unsupported schemes and malformed URLs. Do not use substring hostname checks, send credentials to another origin or automatically choose a Git account based on an untrusted override. Public access must remain explicit and need not unlock Bitwarden. Origin validation protects the CLI-to-SAP credential handoff, but does not prove how the SAP backend's abapGit HTTP client handles subsequent Git redirects. That unchanged SAP-to-Git transport belongs to abapGit/SAP HTTP-client handling; its redirect behavior is a separately tracked upstream question, not a demonstrated regression or release criterion for this feature. Do not claim upstream redirect protection from the CLI's SAP redirect setting alone.

Inject an async `resolveGitCredentials(url)` helper for Git-consuming command paths rather than unlocking the Git vault for every SAP-only command. Preserve `pull`'s existing explicit override contract: `null` means no credentials, `undefined` means configured credentials. Re-pulls retain the same resolved Git destination/credentials and SAP snapshot.

Current `create`, `import`, `pull` and the legacy integration helper consume `gitUsername`/`gitPassword`; they must all reach the same resolver. For the existing SAP/abapGit flow, these credentials are passed to the selected SAP backend, which fetches the repository. That backend is therefore an additional trusted recipient: use verified HTTPS, redact payloads and do not claim end-to-end secrecy from SAP. This also applies to Git-only migration with legacy SAP fields: opting into a central Git profile requires verified HTTPS for its SAP recipient, not today's insecure legacy HTTP client settings.

This first version centralizes the Git credentials currently handled by `.abapGitAgent`. Native `git push`/`fetch` authentication, including the current `--sync-xml` push, still belongs to Git's configured credential helper/SSH agent. It is not automatically changed by `gitProfile`; no tokens are installed into global Git configuration or a credential store. Any later integration of centrally managed credentials into native Git needs a separate scoped helper design and approval.

## 4. Commands and selection

Proposed command specifications, following the existing command documentation format:

- [`system`](../setup/system-command.md): create/list central endpoints.
- [`logon`](../setup/logon-command.md): create client logons, list/current/use and global `--logon-client` selection.
- [`credential`](../utility/credential-command.md): create Git profiles, register custom helpers and list adapter metadata.

The implementation provides these interfaces:

```sh
abapgit-agent system create DEV --host dev.example.internal
abapgit-agent logon create 100 --system DEV --provider file --user DEVELOPER
abapgit-agent credential create work-git --origin https://github.tools.sap --provider file --user I045696
abapgit-agent credential register company-vault --executable /Users/me/bin/company-vault-credentials
abapgit-agent system list
abapgit-agent credential adapters --json
abapgit-agent logon list
abapgit-agent logon current --json
abapgit-agent logon use 200
abapgit-agent --logon-client 300 status
```

- `system list`: list central system aliases and endpoints, locally, without credentials or SAP/vault contact.
- `credential adapters`: list only registered custom CLI adapters, their source (`custom`) and local executable availability. There are no built-in CLI rows; Bitwarden and file providers are not listed. Read the selected/default catalog if present; an absent default catalog yields `{"adapters":[]}`, while an invalid present catalog or missing explicit `ABAP_SYSTEMS_PATH` is an actionable error. Inspect registered paths locally without executing tools, searching PATH, reading tool sessions or contacting local serve. JSON contains names/source/availability only, not arguments, item references, environment values or secrets. Available does not imply authenticated or usable for a lookup.
- `logon list`: list clients/providers for the repository's bound system; no passwords and no vault lookup.
- `logon current`: report bound system, selected client and selection source; no credential retrieval.
- `logon use 200`: persist only the repository's selected client in `.abapGitAgent`, atomically and preserving other settings. No SAP call or secret materialization.
- `--logon-client 300`: one-command override within the bound system; no file write. Parse globally before dispatch.
- In central SAP mode, selection is explicit global override → `ABAP_LOGON_CLIENT` → repository `client`. Missing/unknown clients fail; no “first configured client” fallback. Legacy SAP mode ignores the new environment selector; an explicit `--logon-client` or `logon use` requires a central system binding and fails clearly without one.
- Do not repurpose command-specific `--client` options, which can mean data/customizing client rather than actual logon.

System binding is selected explicitly by editing the repository's `.abapGitAgent` (`system` and `client`; `gitProfile` is independent). `init` still creates the legacy flat configuration; it does not provide a central-binding wizard. There is no ordinary runtime `system use` or global `--system` deployment switch. `logon create --system DEV` names a catalog insertion target only; it does not rebind a repository or authorize deployment. Deliberately rebinding a repository is a configuration change, not proof that pulling its code into another system is safe.

### 4.1. Local creation and manual passwords

- `system create`: add one HTTPS endpoint with an empty `clients` object; defaults are port 443/language EN. It creates no SAP account and does not bind a repository.
- `logon create`: add a new three-digit client to an existing explicit `--system` or repository-bound system. `credential create` adds an independent Git profile for an exact HTTPS origin. Both accept `--provider file --user <username>`, `--provider bitwarden --item-id <UUID>` or registered CLI adapter/reference arguments defined in their specs. Neither selects the entry, resolves a provider or probes authentication.
- File creation saves `credentials: {"provider":"file","user":"<username>","password":""}` and reports `passwordRequired: true`. The user fills in **only the password/token manually** in the private catalog. No password option, prompt, stdin or environment secret input is supported. Empty passwords are deliberate setup drafts permitted by metadata/selection operations. SAP drafts fail before SAP contact; Git drafts fail before the Git-consuming SAP request, although preliminary SAP compatibility/session checks may already have occurred. Preserve manually entered password bytes; never copy them into repository configuration.
- Bitwarden creation records only its provider and exact Login UUID; CLI creation records a registered adapter and literal lookup arguments. Neither logs item references/arguments, resolves secrets, checks local serve or executes/unlocks a tool. User-managed local serve setup remains separate.
- `credential register`: add one custom adapter's absolute executable and optional literal prefix arguments; no installation, execution or reserved-name override.
- Write only the selected personal catalog. `system create`, `credential create` and `credential register` may initialize a missing catalog (default or explicit path) as version `1`; `logon create` requires an existing system. All preserve unrelated entries, reject duplicates and invalid/unsafe existing files, and use private permissions, symlink-safe same-directory atomic replacement and concurrent-edit protection. No implicit overwrite, `--force`, update/delete or resolved-secret persistence.
- Print only operation-specific success metadata, never raw credential/catalog objects. Creation is configuration setup, not proof of account existence, access rights or credential readiness. Repository system/Git-profile binding and project transport settings remain explicit, separate configuration.

## 5. Resolution and compatibility

At the CLI boundary:

1. Parse global options and handle help/offline commands before connection gates, backend version checks or credential resolution. Local list/current/use/create/register commands access only the settings/catalog they need, without connection gates, compatibility checks or credential resolution.
2. Read raw settings through the existing discovery path. With no central references, do not read or validate the catalog and preserve legacy connection behavior. The new environment selector alone must not opt an old repository into central mode.
3. With `system`, load/validate its central entry and selected client, then resolve only that client's SAP credentials. Without `system`, preserve legacy SAP fields even when `gitProfile` is selected.
4. Combine the selected endpoint/credentials and applicable project/client settings into one immutable flat SAP command configuration. Snapshot each catalog when first needed (at SAP resolution, or the first Git operation in Git-only mode), including its references/provider settings; do not reload it between nested requests or re-pulls. A later invocation observes catalog edits.
5. Run backend compatibility checks and execute SAP requests against that snapshot. Resolve `gitProfile` lazily for the actual Git URL only on Git-consuming paths, validating its definition before the credential-bearing operation. SAP-only commands do not access a Git-only catalog; an invalid Git reference fails when used, not on an unrelated SAP command.

Existing `AbapHttp(config, options)` and `AdtHttp(config)` constructors can keep receiving flat host/port/client/user/password fields. Inject the resolved snapshot through command context; HTTP clients do not need to know about providers, adapter names or executable paths. A shared CLI dispatcher owns registration lookup, safe process execution and result validation; the built-in Bitwarden provider owns local HTTP validation/parsing, while custom helpers own their external tool integration. Raw config writes must never serialize this resolved object. Nested commands, hooks, polling and debugger sessions remain pinned to their original snapshot.

Connection/client selection needs no ABAP backend change. Audit `src/config.js`, CLI bootstrap and command registration, both HTTP clients, debugger state, version checks, init, SO10 secondary logons, transport hooks, duplicate helper loaders, help/guide and tests. Add the shared CLI dispatcher and built-in Bitwarden local-serve provider at the credential-resolution boundary, plus the local creation/registration and metadata-only listing commands with matching command/help documentation. Validate custom registrations on explicit listing, creation/registration or selection, not on unrelated legacy/offline paths. The connection gate accepts a repository config, an explicit existing `CONFIG_PATH`, or complete effective legacy `ABAP_HOST`/`ABAP_USER`/`ABAP_PASSWORD` environment settings. Existing discovery precedence is unchanged: repository file → explicit path or package-directory fallback → environment. An implicit package-directory fallback alone does not activate SAP commands from an unrelated working directory; it also takes precedence over environment credentials. Help bypasses configuration and all side-effecting handlers must return before actions, including nested help on debug/transport and guide migration/reference cloning.

## 6. Safety boundaries

- **Central plaintext:** owner-only file (`0600`) and private directory (`0700`) on POSIX. Windows central mode is unsupported and fails closed; future Windows support requires appropriate ACLs. Never commit, echo or copy credentials into repository configs. Safe writes must handle symlinks and concurrent edits. The entire catalog remains sensitive if any SAP or Git entry uses file credentials; parsing it can read those plaintext fields even when their profiles are not selected. Lazy provider resolution is not an isolation guarantee between entries in one file.
- **CLI execution:** registered custom adapters use the bounded, no-shell process boundary and validated credential result. Executables, prefix arguments and lookup references come only from trusted personal configuration. Never log child output/errors, persist resolved secrets or pass session/password values in arguments. Custom helpers inherit user permissions/environment; this is arbitrary trusted code execution, not a sandbox. Metadata listing must not invoke that code.
- **Bitwarden:** built-in local-serve provider only; no consumer session requirement or `bw` subprocess. Never auto-login/unlock/sync/start/stop or collect a master password. Validate the IPv6 loopback endpoint with redirects/proxies disabled and enforce bounded status/item requests. Redact responses/errors, fetch into memory only and fail before a credential-bearing operation if unavailable/locked. The user owns server lifecycle; local vault exposure remains broad, not server-enforced single-item access.
- **Target trust and TLS:** the central endpoint determines where credentials are sent. Protect the catalog and use verified HTTPS; endpoint edits affect every referencing repository. Active commands do not retarget mid-run. Carry one explicit verified TLS policy through `AbapHttp`, `AdtHttp`, compatibility probes and secondary logons, using Node's trusted CA configuration for enterprise certificates. Reject HTTP and certificate-verification bypasses on any connection receiving centrally managed credentials, including a central Git token sent through legacy SAP fields. Completely legacy configurations keep their current TLS behavior; that existing risk is not silently repaired or endorsed here.
- **Sessions:** isolate by complete endpoint + client + username. Current port/protocol-omitting HTTP/debug cache keys must be corrected; old ambiguous cache entries must not be reused. Private runtime files and pinned debugger connections remain required. Central connection state lives in an owner-only `~/.cache/abapgit-agent/connections` subdirectory, separate from the existing nonsensitive version cache; do not loosen state permissions or change the parent version-cache permissions.
- **Shared ABAP code:** switching clients does not create separate code versions. SAP authorization and existing repository safeguards decide whether the selected user may change shared code.

## 7. Conflict metadata and transports

`ZABGAGT_OBJ_META` is object-global within a SAP backend and has no `MANDT`. Clients of the same bound system share the baseline intentionally. Do not reset or duplicate it when changing client credentials/providers. HTTP session isolation and ABAP conflict-baseline scope are different concerns.

Development-system → test-system deployment uses SAP transports. Do not add cross-system first-pull adoption or automatic `--sync-xml` to the client-logon feature. See [pull conflict detection](../development/pull-conflict-detection.md) and [object-file handling](object-file-handling.md) for existing behavior and coverage. Intentional rebinding still has backend limitations: a missing baseline can allow overwrite of an existing destination object, missing stored source can be treated as unchanged, complete source bundles are not covered, and the weak fingerprint fallback can collide. None is fixed by client-logon selection. Repository rebinding must not be presented as a safe deployment shortcut.

## 8. Acceptance checks

- Two repositories reference DEV: endpoint/credentials maintained once, selections/packages/transports remain independent.
- DEV client 100 uses file credentials and 200 uses built-in Bitwarden local serve; switching resolves the correct provider and never falls back to 100. A user-started unlocked server works without a consumer session variable or CLI execution.
- One custom adapter registration can serve multiple SAP clients and Git profiles with different lookup arguments; executable configuration is not repeated in entries.
- Adapter listing includes registered custom helpers only, with source `custom` and executable availability; no tool/session/vault access. No default catalog or registrations yields an empty list; Bitwarden never appears as a CLI row.
- Unknown/reserved adapter names, invalid registration/arguments, missing executables, nonzero exit, timeout/overflow, malformed JSON and empty credentials fail closed with redacted diagnostics. No shell interpretation, dynamic module loading or fallback occurs.
- Custom helper lookup obeys the total deadline/output bounds, retains password bytes and terminates the helper's foreground process group on timeout; detached/daemonizing children are unsupported. Bitwarden HTTP status/item calls share one deadline/body bound, reject non-Login or mismatched items and unsafe endpoints, and abort on timeout/overflow without CLI fallback.
- SAP-only/legacy/offline commands do not execute unrelated adapters; Git destination validation occurs before invoking its selected adapter.
- Global client override does not change the saved selection; existing command-specific `--client` semantics remain unchanged.
- Unknown SAP system/client, ambiguous SAP fields, locked SAP provider or malformed SAP credentials fail before SAP contact. Invalid/locked Git profiles fail before their credential-bearing Git operation; unrelated SAP commands remain usable.
- List/current/use/create/register need no vault/SAP connection and never expose credentials or execute tools.
- Creation/registration preserve unrelated catalog entries, reject duplicates/unsafe paths/concurrent edits and initialize only explicitly requested missing catalogs with private permissions. Failure must not replace valid existing data.
- File-provider creation saves a username and empty password, reports manual password maintenance and never accepts secret input. Draft entries remain inspectable/selectable; actual lookup rejects empty passwords before credential-bearing operations. Bitwarden/CLI creation validates references without execution, local-serve access or authentication claims.
- Config/init writes preserve references and unrelated settings; no resolved password/token appears in logs or files. Manually maintained file-provider passwords stay only in the private catalog.
- HTTP/debug sessions are isolated across clients/users/endpoints while ABAP conflict metadata remains shared within one backend.
- SO10 TRANSPORT resolves the intended secondary client/provider asynchronously, defaults correctly when no client settings exist, and does not inherit another client's transport. COPY/READ options retain their data-client meaning.
- Centrally managed credentials require verified HTTPS through both HTTP clients, secondary logons and compatibility probes, including Git-only migration. Verify CLI destination-origin validation independently; it does not establish upstream redirect safety.
- Old flat configurations work unchanged even when the central catalog exists, is unavailable, or Bitwarden is locked; offline commands remain offline.
- SAP-only central migration, Git-only central migration and migration of both work independently; ambiguous fields fail explicitly.
- Git credentials are shared across referring repositories without being stored in them; wrong-origin `--url` overrides are rejected before secret retrieval/transmission.
- SAP-only commands do not resolve a Git profile or call its vault provider; public Git access preserves explicit no-credentials semantics. A shared plaintext catalog is still read as one file, not a per-entry secret store.
- `create`, `import`, `pull`, re-pulls, `drop --pull` and integration helpers use the same configured Git source; native Git auth remains unchanged and documented. `drop --pull` preflights its actual remote and credentials before deleting an object, then pins those credentials for the nested pull. Later network/backend failures can still leave a dropped object; this is not an atomic rollback guarantee.

Unit tests should mock credential providers/subprocesses and verify registration/selection, argument ordering, deadline/output bounds, environment handling, result validation and redaction. Mock Bitwarden HTTP responses to test locked/unavailable/malformed/missing-field/wrong-item results, endpoint validation, redirect/proxy refusal and shared deadline/body bounds without requiring a consumer session. Use a dummy external helper for dispatcher integration without real credentials. Authorized runtime checks should exercise two clients in one development system, two referencing repositories, file credentials, built-in Bitwarden and a custom helper; verify supported-platform launch and process-tree cleanup separately. Bitwarden runtime verification requires a separately authorized user-started loopback server; metadata operations must make no requests. Authorized runtime checks on 2026-11-23 passed verified-TLS SAP health and repository lookup for two local repositories using the same GZE/client 100 endpoint: one with file providers and one with built-in Bitwarden SAP/Git Login providers. Fresh linked-CLI Bitwarden health worked without `BW_SESSION`; actual provider-backed Git API user/repository checks passed. An unavailable alternate loopback port failed without file-provider fallback; the real vault was not stopped or locked. These checks did not forward Git credentials through SAP and did not exercise two distinct clients. Deployed custom-helper integration, secondary-client runtime, SAP-mediated Git credential forwarding, Linux/Node-floor coverage and exact published-commit CI remain unverified. These are recorded coverage limitations, not passing runtime results. Redirect behavior in the unchanged SAP-to-Git transport is a separate upstream follow-up, not this feature's release criterion. Unit tests and local helper/HTTP/TLS fixtures under `tests/unit/` cover dispatcher behavior and command-path TLS/help regressions; they are not proof of those remaining deployed-runtime checks. Windows central mode remains unsupported.

**Upstream follow-up (scope clarified 2026-11-24):** read-only inspection of the active GZE `ZCL_ABAPGIT_HTTP` and `ZCL_ABAPGIT_HTTP_CLIENT` classes found Authorization installed before native HTTP `send`/`receive`, with HTTP-302 handling only after `receive`. No explicit redirect-disable setting was found in those two classes. Exit hooks/native HTTP configuration can affect behavior; this inspection does not prove leakage or safety. No SAP redirect probe ran. Probe work is deferred outside this feature's release criteria; the SAP Git transport was not changed by this implementation.

## 9. Original design-review outcome

The design review found and resolved the following issues:

| Finding | Resolution |
|---|---|
| Unconditional catalog loading contradicted legacy compatibility and lazy Git resolution | Explicit mode-dependent loading; offline paths and old configurations do not access the catalog |
| New `clientSettings` could silently retain flat transport/customizing credentials, and SO10's helper is currently synchronous | Defined defaults, competing-field rejection, and an async secondary-logon boundary while preserving legacy/data-client paths |
| Git-only migration could send a centrally managed token over today's unverified SAP TLS connection | Verified TLS is mandatory for all recipients of central credentials, consistently across clients and compatibility checks |
| CLI origin checks do not establish SAP-side Git redirect safety | Initially requested backend verification; scope clarified on 2026-11-24: upstream follow-up, not this feature's release criterion. No redirect-safety guarantee is claimed |
| A temporary analysis link would break after publishing | Replaced it with repository documentation links and a self-contained warning |
| A Bitwarden-only CLI or per-entry executable path would couple users to one tool and repeat configuration | Separated the built-in Bitwarden local-serve provider from a named CLI dispatcher with personal custom registrations and a bounded external-helper contract; listing is metadata-only |
| Requiring a consumer session variable and a second Bitwarden CLI transport complicated user setup | Built-in Bitwarden uses user-managed local serve only, with exact-item lookup and no automatic lifecycle; custom CLI helpers stay separate |
| Manual JSON editing was required even to add endpoints, usernames and provider references | Added narrow creation/registration commands; file-provider drafts leave only password/token maintenance to the user and fail closed until completed |

**Original design verdict:** passed documentation/design review after these corrections. Implementation was subsequently authorized; this historical verdict is not proof that runtime acceptance checks pass. Native Git credential-helper integration and the existing cross-system conflict defects remain deliberately outside scope.

Source review covered `src/config.js`, `bin/abapgit-agent`, `src/commands/create.js`, `src/commands/import.js`, `src/commands/pull.js`, `src/commands/init.js`, `src/commands/so10.js`, `scripts/claude-integration.js`, and the HTTP, ADT, version-check, debug-state, SO10-client-profile and transport-selector utilities. JSON/schema/reference and repository-link checks validate the document only. No live SAP/Bitwarden checks, ABAP execution or implementation test suite was run for this documentation change.

## Scope decision

**Keep:** unchanged legacy configs, central definitions for multiple systems, per-system/client SAP credentials with mixed providers, independent reusable Git credential profiles, a named CLI dispatcher for user-registered external adapters, built-in Bitwarden through user-managed local serve only, gradual migration, narrow local creation/registration with manual file-password completion, repository references, client-logon switching, isolated sessions and per-client project transport settings.

**Defer:** routine cross-system code pulls/deployment, automatic migration, vault discovery/writes/unlock, multiple accounts per client, dynamic in-process plugins/SDK/package discovery, speculative adapters for unused tools and a general configuration editor. This avoids confusing a reusable system catalog with permission to develop the same repository across several systems.
