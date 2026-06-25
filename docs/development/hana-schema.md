---
layout: default
title: schema subcommands
nav_order: 2
parent: hana - HANA Views & Catalog
grand_parent: Development Commands
---

# `hana schema` subcommands

Catalog queries against HANA's public dictionary (`SYS.SCHEMAS`, `SYS.TABLES`,
`SYS.TABLE_COLUMNS`). Use these when authoring a HANA view that references raw ABAP
tables — you need to know which schema a table lives in and the exact column names
before writing the view's `<dataSources>` and `<keyMapping>` entries.

| Subcommand | Returns | When to use |
|---|---|---|
| `hana schema list`   | Schemas in HANA                | Find which schema a replicated ABAP table lives in |
| `hana schema tables` | Tables in a schema             | Discover available raw tables for a view to reference |
| `hana schema fields` | Columns of a table             | Verify exact column names and types before writing the view definition |

---

## `hana schema`

Query the HANA catalog to discover schemas, tables, and columns. Used when authoring HANA views that reference raw ABAP tables (possibly in replicated schemas) — you need to know which tables exist and their column structure before writing the view's `<dataSources>` and `<keyMapping>` entries.

Three subcommands form a top-down lookup flow:

```bash
# 1. find schemas
abapgit-agent hana schema list --filter %CORE%

# 2. find tables in a schema
abapgit-agent hana schema tables --schema SAP_FRA_CORE_S4 --filter T001

# 3. inspect a table's columns
abapgit-agent hana schema fields --schema SAP_FRA_CORE_S4 --table T001
```

### Usage

```
abapgit-agent hana schema list   [--filter <pattern>] [--limit <n>] [--json]
abapgit-agent hana schema tables --schema <name> [--filter <pattern>] [--limit <n>] [--json]
abapgit-agent hana schema fields --schema <name> --table <name> [--json]
```

### Options

| Option | Required for | Description |
|---|---|---|
| `--schema <name>` | tables, fields | HANA schema name (case-insensitive — uppercased server-side) |
| `--table <name>` | fields | HANA table name (case-insensitive) |
| `--filter <pattern>` | list, tables | SQL `LIKE` pattern, e.g. `%FRA%` or `T0%` |
| `--limit <n>` | list, tables | Max rows (default `100`; not applicable to `fields`) |
| `--json` | all | Output raw JSON instead of formatted table |

### How it works

Each subcommand issues a single ADBC query against HANA's public dictionary views:

- `list` → `SYS.SCHEMAS`
- `tables` → `SYS.TABLES`
- `fields` → `SYS.TABLE_COLUMNS`

The HTTP request body is:

```json
{
  "action": "schema",
  "subaction": "list" | "tables" | "fields",
  "schema": "...",
  "table": "...",
  "filter": "...",
  "limit": 100
}
```

### Output — `list`

```
  Schema
  ────────────────────
  SAP_FRA_CORE_S4
  SYS

  2 schema(s) found.
```

### Output — `tables`

```
📋 Schema: SAP_FRA_CORE_S4

  Table                           Column?
  ──────────────────────────────  ────────────
  T001                            TRUE
  T001W                           TRUE

  2 table(s) found.
```

### Output — `fields`

```
📋 Schema: SAP_FRA_CORE_S4   Table: T001

  Pos    Column                Type                Length    Scale     Nullable
  ─────  ────────────────────  ──────────────────  ────────  ────────  ──────────
  1      MANDT                 NVARCHAR            3         0         FALSE
  2      BUKRS                 NVARCHAR            4         0         FALSE
  3      BUTXT                 NVARCHAR            25        0         TRUE

  3 column(s) found.
```

### Empty result

```
  No matches found.
```

### Notes

- Excludes views and synonyms — `SYS.TABLES` returns only physical row/column-store tables. This matches the "find raw ABAP tables" use case.
- No elevated privileges required; the public `SYS` dictionary is readable by every HANA user.
- `--filter` is a SQL `LIKE` pattern — use `%` as wildcard. Pattern is passed through as-is to support case-sensitive matches.
- Empty result returns exit code 0 with `No matches found.` — not an error.
