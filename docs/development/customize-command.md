---
layout: default
title: customize - Write Customizing Table Entries
nav_order: 22
parent: Development Commands
---

# `customize` Command

## Overview

Write a single row (insert or update) into a SAP customizing table directly from the command line. No SAP GUI required.

Customizing tables are tables with delivery class `C` (Customer Customizing) or `E` (System Settings). They store customer-specific configuration — feature flags, default values, integration settings, etc. Unlike ABAP code objects, these entries are **not managed by abapGit** — they are written directly with `MODIFY` and optionally recorded on a customizing transport request.

---

## Usage

```bash
# Write an entry — transport resolved from .abapGitAgent config (same as pull)
abapgit-agent customize ZTABLE_CONFIG --set KEY=MYAPP VALUE=active

# Explicit transport override
abapgit-agent customize ZTABLE_CONFIG --set KEY=MYAPP VALUE=active --transport DEVK900001

# Skip transport recording (local package or intentional bypass)
abapgit-agent customize ZTABLE_CONFIG --set KEY=MYAPP VALUE=active --no-transport

# Multiple field=value pairs in one call
abapgit-agent customize ZTABLE_CONFIG --set CODE=001 DESCRIPTION=enabled STATUS=A

# JSON output (for scripting)
abapgit-agent customize ZTABLE_CONFIG --set KEY=MYAPP VALUE=active --no-transport --json
```

---

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `<table>` | Yes | Name of the customizing table (delivery class `C` or `E`) |
| `--set <field=value> ...` | Yes | One or more `field=value` pairs to write. Field names are uppercased automatically. |
| `--transport <TRKORR>` | No | Explicit transport request number. Must be an open customizing request (`TRFUNCTION='W'`). |
| `--no-transport` | No | Bypass transport recording. Write directly without recording on any transport. |
| `--json` | No | Output raw JSON response. |

### Transport Resolution Priority

Same priority order as the `pull` command:

| Priority | Source |
|----------|--------|
| 1 | CLI `--transport <TRKORR>` |
| 2 | `transport` field in `.abapGitAgent` |
| 3 | `ABAP_TRANSPORT` environment variable |
| 4 | Interactive picker (TTY) or transport hook (AI/CI) — filtered to customizing requests |
| 5 | `--no-transport` (explicit bypass) |

If the table is in a **local package** (package name starts with `$`) or the package's `KORRFLAG` is not set, no transport is required and the entry is written directly.

If the table is in a **transportable package** and no transport is provided, the command errors with a message asking you to supply `--transport` or `--no-transport`.

---

## Output

### Success (with transport)

```
✅ Customizing entry written to ZTABLE_CONFIG
   Action:    MODIFIED
   Transport: DEVK900001 (recorded)
   Customizing entry MODIFIED in ZTABLE_CONFIG and recorded on transport DEVK900001
```

### Success (local package / no transport needed)

```
✅ Customizing entry written to ZTABLE_CONFIG
   Action:    MODIFIED
   Transport: none (delivery class C)
   Customizing entry MODIFIED in ZTABLE_CONFIG
```

### JSON output (`--json`)

```json
{
  "success": "X",
  "command": "Customize",
  "message": "Customizing entry MODIFIED in ZTABLE_CONFIG",
  "error": "",
  "table_name": "ZTABLE_CONFIG",
  "action": "MODIFIED",
  "transport": "",
  "delivery_class": "C"
}
```

---

## Validation

The command validates the following before writing:

| Check | Error Message |
|-------|---------------|
| Table not in data dictionary | `Table <X> not found in ABAP data dictionary` |
| Delivery class is not `C` or `E` | `Table <X> is not a customizing table (delivery class: <X>)` |
| Table in transportable package, no transport | `Table <X> is in a transportable package. Provide --transport <TRKORR> or use --no-transport.` |
| Transport not found | `Transport <T> not found` |
| Transport already released | `Transport <T> is already released (not modifiable)` |
| Transport is a workbench request (`TRFUNCTION='K'`) | `Transport <T> is a workbench request. Customizing entries require a customizing transport.` |
| Field does not exist in table | `Field <F> does not exist in table <X>` |
| Date field with wrong format | `Field <F> has type D (date, format YYYYMMDD), cannot assign '<V>'` |
| Time field with wrong format | `Field <F> has type T (time, format HHMMSS), cannot assign '<V>'` |
| Numeric char field with non-digits | `Field <F> has type N (numeric char), value must contain only digits` |
| Integer field with non-integer | `Field <F> has type I (integer), value must be a whole number` |
| Hex field with invalid hex string | `Field <F> has type X (hex), value must be even-length hexadecimal (e.g., 1A2B)` |
| MODIFY fails | `Failed to write to table <X> (sy-subrc: <N>)` |

---

## Transport Types

The `customize` command requires a **customizing** transport request, not a workbench request.

| Request Type | `TRFUNCTION` | Used for |
|---|---|---|
| Workbench | `K` | ABAP code — classes, programs, function modules |
| **Customizing** | `W` | **Customizing table entries, IMG settings** |

If you don't have a customizing transport yet, create one:

```bash
abapgit-agent transport create --description "My config changes" --type customizing
```

Then use that number with `--transport` or add it to `.abapGitAgent` as the `transport` value.

---

## Examples

### Typical use case — set a configuration flag

```bash
# Write one entry (transport from .abapGitAgent config)
abapgit-agent customize ZAPP_CONFIG --set CONFIG_KEY=ENABLE_FEATURE CONFIG_VALUE=X

# Write with explicit transport
abapgit-agent customize ZAPP_CONFIG --set CONFIG_KEY=ENABLE_FEATURE CONFIG_VALUE=X --transport DEVK900123
```

### CI/CD pipeline — no-transport write

```bash
# Automated setup in dev/test system (local package, no transport needed)
abapgit-agent customize ZAPP_CONFIG --set ENV=DEV DEBUG=X --no-transport
```

### Scripting with JSON output

```bash
result=$(abapgit-agent customize ZAPP_CONFIG --set KEY=VAL --no-transport --json)
action=$(echo "$result" | jq -r '.action')
echo "Action: $action"   # MODIFIED
```

### Verify the result

```bash
# Preview table data after writing
abapgit-agent preview --objects ZAPP_CONFIG
```

---

## Notes

- **Insert or update:** Uses `MODIFY` — if a row with the same key already exists, it is updated; otherwise it is inserted.
- **Single row per call:** Each invocation writes exactly one row. For bulk inserts, run the command multiple times.
- **MANDT:** The client field (`MANDT`) is set automatically by the system — do not include it in `--set`.
- **No delete:** This command does not support deleting rows. Use SM30 or a custom ABAP program for deletion.
- **Delivery class:** Only tables with delivery class `C` (Customer Customizing) or `E` (System Settings) are accepted. Tables with class `A`, `L`, `S`, `W`, or `G` are rejected.

---

## Related Commands

| Command | Purpose |
|---------|---------|
| [`transport create --type customizing`](transport-command.md) | Create a customizing transport request |
| [`transport list`](transport-command.md) | List your open transport requests |
| [`preview`](preview-command.md) | Preview table contents after writing |
| [`pull`](pull-command.md) | Activate ABAP code objects |
