---
layout: default
title: so10 - Manage Standard Texts
nav_order: 24
parent: Development Commands
---

# `so10` Command

## Overview

Manage SAPscript standard texts from the command line. Import texts into Git as ITF JSON files, create or update them with conflict protection, copy between clients, and record texts on customizing transports.

Texts use object `TEXT` and are identified by client, name, text ID and language. They are not ABAP repository objects and do not require `pull` activation.

---

## Usage

```bash
abapgit-agent so10 list --prefix ZAPP_ --id ST --language EN --json
abapgit-agent so10 import --prefix ZAPP_ --id ST --language EN
abapgit-agent so10 read ZAPP_FOOTER --language EN --json
abapgit-agent so10 create ZAPP_FOOTER --language EN --itf-file drafts/footer.itf.json
abapgit-agent so10 update ZAPP_FOOTER --language EN
abapgit-agent so10 copy ZAPP_FOOTER --language EN --target-client "$TARGET_CLIENT"
abapgit-agent so10 drop ZAPP_FOOTER --language EN
abapgit-agent so10 transport ZAPP_FOOTER --language EN --transport "$CUSTOMIZING_TASK"
```

---

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `<action>` | Yes | `list`, `import`, `read`, `create`, `update`, `copy`, `drop` or `transport`. |
| `<text-name>` | Except LIST/IMPORT | Exact text name, case-preserved, up to 70 characters. |
| `--language <key>` | Except LIST/IMPORT | SAP language key or ISO language code, such as `E` or `EN`. |
| `--id <id>` | No | Text ID, up to 4 characters. Defaults to `ST` for exact-key actions; optional filter for LIST/IMPORT. |
| `--prefix <prefix>` | LIST/IMPORT | Required literal prefix. Repeat to select multiple prefixes. |
| `--name <name>` | No | Exact name filter for LIST/IMPORT. |
| `--max-hits <n>` | No | Combined LIST/IMPORT limit, default 100, maximum 1000. Overflow fails without partial results. |
| `--dry-run` | No | IMPORT preview: list headers without reading bodies or writing files/baselines. |
| `--adopt-system` | No | IMPORT may replace differing local content only when it is tracked and clean. |
| `--file <path>` | No | CREATE/UPDATE override using UTF-8 plain text. |
| `--itf-file <path>` | No | CREATE/UPDATE override using an ITF array or version 1 payload. Mutually exclusive with `--file`. |
| `--conflict-mode abort\|ignore` | No | CREATE/UPDATE conflict policy, default `abort`. Ignore does not bypass ownership, authorization or locks. |
| `--source-client <client>` | No | READ/LIST/IMPORT/COPY data client, default configured client. |
| `--target-client <client>` | No | COPY/DROP/TRANSPORT target. COPY/TRANSPORT default to `customizingClient`, then configured client; DROP defaults to configured client. |
| `--overwrite` | No | COPY may replace an existing target text. |
| `--transport <request/task>` | No | Existing customizing request/task for TRANSPORT only. |
| `--json` | No | Output JSON. Failures return a nonzero exit code. |
| `--help`, `-h` | No | Show command help. |

---

## Output

### JSON output (`--json`)

READ returns the canonical text key and ordered ITF lines. Additional fields report source fingerprints and baseline ownership.

```json
{
  "success": true,
  "command": "SO10",
  "action": "READ",
  "object": "TEXT",
  "name": "ZAPP_FOOTER",
  "id": "ST",
  "language": "E",
  "found": true,
  "line_count": 1,
  "saved": false,
  "lines": [{ "tdformat": "*", "tdline": "Application footer" }],
  "error": ""
}
```

Modifying actions report their outcomes independently:

| Field | Meaning |
|-------|---------|
| `saved` | Text save status. False for adoption, idempotent updates and DROP. |
| `baseline_recorded` | Source baseline recording status. |
| `deleted`, `baseline_removed` | DROP text absence and baseline removal status. Git files are retained. |
| `transport_recorded` | TRANSPORT recording status; request/task/key are returned separately. |
| `file_written` | IMPORT/local file preparation status, independent of SAP changes. |

SAP outcome flags can be `false`, `true` or `null` (unconfirmed). Inspect the text before retrying an unknown outcome. If CREATE saved the text but baseline recording failed, recover with explicit IMPORT rather than retrying CREATE.

---

## Examples

### Import and update Git-managed texts

Track this configuration in the application repository's `.abapgit-agent.json`:

```json
{
  "so10": { "repoId": "example.app", "folder": "texts/so10" }
}
```

```bash
# Preview, then explicitly import matching texts and their source baselines
abapgit-agent so10 import --prefix ZAPP_ --id ST --language EN --dry-run
abapgit-agent so10 import --prefix ZAPP_ --id ST --language EN

# Edit the imported file, then save it
abapgit-agent so10 update ZAPP_FOOTER --language EN
```

Managed files contain the complete key and ordered ITF content:

```json
{
  "version": 1,
  "key": { "object": "TEXT", "name": "ZAPP_FOOTER", "id": "ST", "language": "E" },
  "lines": [{ "tdformat": "*", "tdline": "Application footer" }]
}
```

CREATE/UPDATE use the managed file by default. Explicit file overrides preserve differing dirty/untracked work. No command automatically stages, commits or pushes Git files. Outside an enabled repository, CREATE/UPDATE require an explicit file and `--conflict-mode ignore` and do not advance Git baselines.

### Copy between clients

```bash
abapgit-agent so10 copy ZAPP_FOOTER --language EN \
  --source-client "$SOURCE_CLIENT" --target-client "$TARGET_CLIENT"
```

Clients must differ. COPY uses the existing HTTP connection with backend client selection; it accepts no files and does not advance Git baselines. Add `--overwrite` only when replacement of an existing target is intended.

### Drop a Git-managed text

```bash
abapgit-agent so10 drop ZAPP_FOOTER --language EN
```

DROP requires a clean committed full-key payload/configuration and matching owned SAP baseline/content. It deletes only the selected text and baseline, retaining Git files for recovery. No standalone mode, conflict ignore or `delete` alias is supported. Explicitly import/adopt and commit a copied target before dropping it.

### Record a customizing transport

```bash
abapgit-agent so10 transport ZAPP_FOOTER --language EN --transport "$CUSTOMIZING_TASK"
```

TRANSPORT uses an existing modifiable customizing request/task. Resolution is CLI → effective client's configured transport → `ABAP_TRANSPORT` → existing-task hook/picker. A different logon client requires `.abapGitAgent.customizingLogon` with matching `client`, `user`, `password` and optional `transport`; development credentials/tasks are not copied. Recording does not save/copy text or create/release/import transports.

---

## Notes

- LIST/IMPORT require literal prefixes; selection is bounded and deduplicated by complete key. LIST/READ never enroll texts. IMPORT writes files and source baselines, not text content.
- CREATE fails if the text exists; UPDATE fails if it is missing. Managed UPDATE requires an owned baseline and rejects source changes, including edits by the same SAP user, unless explicit conflict ignore is supplied.
- ITF formats are limited to 2 characters and lines to 132. Empty/all-blank content, controls, invalid UTF-8 and key mismatches are rejected; empty content is not a deletion mechanism.
- CREATE/UPDATE use the configured client. Selected-client access, native text authorization, client policy and full-key locks apply to modifying actions. Only the intended target is changed.
- Saving and dropping never automatically record transports. Inspect partial outcomes before retrying; no mass deletion, cross-system copy or dependency copying is supported.
- For the detailed AI workflow and backend contracts, use `abapgit-agent ref --topic so10` after `abapgit-agent guide --ai`.

---

## Related Commands

| Command | Purpose |
|---------|---------|
| [`transport`](transport-command.md) | Manage transport requests |
| [`customize`](customize-command.md) | Write customizing table entries |
| [API](../api.md#post-so10) | SO10 REST request and response fields |
| [Project configuration](../project-config.md#so10) | Configure the managed text folder and repository identity |
