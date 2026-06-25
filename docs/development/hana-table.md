---
layout: default
title: table subcommands
nav_order: 3
parent: hana - HANA Views & Catalog
grand_parent: Development Commands
---

# `hana table` subcommands

Preview rows from a raw HANA table. Complements `hana view preview` (which queries
activated views via `_SYS_BIC`) — together they cover the input (source tables a view
references) and the output (the view's result) of view authoring.

| Subcommand | Returns | When to use |
|---|---|---|
| `hana table preview` | Rows from `"<schema>"."<table>"` | Sample source data before/while authoring a view; supports physical `--schema` and logical `--logical-schema` (via `DB_SCHEMA_MAP`) |

---

## `hana table preview`

Preview rows from a raw HANA table. Complements `hana view preview` — together they cover the
input (source tables a view references) and the output (the view's result) of view authoring.

The target table can be addressed by **physical** schema name (e.g. `SAP_FRA_CORE_S4`) or by a
**logical** schema name maintained in transaction `DB_SCHEMA_MAP`. Use the logical form when
working across landscapes where the physical schema name varies.

### Usage

```bash
abapgit-agent hana table preview --schema SCHEMA --table TABLE [options]
abapgit-agent hana table preview --logical-schema NAME --table TABLE [options]
```

### Options

| Option | Required | Description |
|---|---|---|
| `--schema <name>` | one-of | Physical HANA schema name (e.g. `SAP_FRA_CORE_S4`, `SYS`). Case-insensitive. Mutually exclusive with `--logical-schema`. |
| `--logical-schema <name>` | one-of | Logical schema name maintained in `DB_SCHEMA_MAP`. Resolved server-side via `$ABAP.schema( … )`. |
| `--table <name>` | yes | HANA table name |
| `--limit N` | no | Max rows. Default 100. Max 500. |
| `--offset N` | no | Skip first N rows for paging. Default 0. |
| `--where "<expr>"` | no | SQL WHERE clause body (no `WHERE` keyword) |
| `--columns A,B,C` | no | Project only the given columns |
| `--vertical` | no | Render one column per line |
| `--compact` | no | Truncate cell values to column width |
| `--json` | no | Emit raw JSON; bypass the table renderer |

### How it works

1. Validate args — exactly one of `--schema` / `--logical-schema`; `--table` required.
2. For physical schemas, pre-check the table exists in `SYS.TABLES` (clear `table_not_found` if not).
3. Build the FROM clause:
   - Physical: `"<SCHEMA>"."<TABLE>"`
   - Logical: `"$ABAP.schema( <LOGICAL> )"."<TABLE>"`
4. Build the rest of the SQL — all dynamic fragments (schema, table, column names, WHERE) routed through `cl_abap_dyn_prg=>escape_quotes`:
   ```sql
   SELECT <columns or *>
     FROM "<schema-or-macro>"."<table>"
    [WHERE <where>]
    LIMIT <limit> OFFSET <offset>
   ```
5. Execute via ADBC; build an all-string row struct via `cl_sql_result_set->get_struct_ref( md_tab = …, string_only = abap_true )`.

### Output samples

**Success:**
```
📋 HANA table: SAP_FRA_CORE_S4.T001  (limit=100)

  MANDT  BUKRS  BUTXT
  ─────  ─────  ─────────────────
  100    1000   ACME Corp
  100    2000   ACME EMEA

  2 row(s) returned.
```

**Logical schema:**
```
📋 HANA table: FRA_CORE_ERP.T001  (limit=100)

  ...
```

**Table not found:**
```
❌ Error: table_not_found
   Table FOO not found in schema SAP_FRA_CORE_S4.
   Hint: list tables with `abapgit-agent hana schema tables --schema SAP_FRA_CORE_S4 --filter FOO%`
```

**Logical schema not mapped:**
```
❌ Error: schema_not_mapped
   Logical schema "FRA_CORE_ERP" is not mapped in DB_SCHEMA_MAP on this system.
   Configure the logical schema in transaction DB_SCHEMA_MAP and retry.
```

**Insufficient privilege:**
```
❌ Error: invalid_sql
   insufficient privilege: ...
```

### Out of scope

- Write operations on raw tables — out of scope; use ABAP transactions or HANA Studio for mutations.
- Aggregation / GROUP BY / ORDER BY — `--where` and `--columns` are the only shaping operators.
- Multi-table JOIN — single-table only; for JOINs build a HANA view and preview it with `hana view preview`.
