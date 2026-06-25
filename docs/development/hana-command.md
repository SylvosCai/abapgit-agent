---
layout: default
title: hana - HANA Views & Catalog
nav_order: 23
parent: Development Commands
has_children: true
---

# `hana` Command

## Overview

Manage HANA repository views (attribute views and calculation views) from the command line via the ABAP application server proxy.

The command has three noun groups: `view` (operations on HANA repository views), `schema` (catalog queries to help author those views), and `table` (preview raw HANA table data). Each group's subcommand specs live on a dedicated child page — see the links below.

### `view` — manage HANA repository views

→ Full specs: [`view` subcommands](hana-view.md)

| Subcommand | Direction | When to use |
|---|---|---|
| `hana view deploy` | `hana/` files → HANA | Day-to-day: push edited files into HANA |
| `hana view import` | HANA → `hana/` files | One-time bootstrap: pull existing HANA content into repo |
| `hana view list`   | Read HANA state      | Verify what is deployed in a package and its status |
| `hana view drop`   | HANA → ⌀             | Delete a HANA view from the HANA repository (local file kept) |
| `hana view preview`| Read HANA data       | Preview the data produced by an activated view (via `_SYS_BIC`) |

### `schema` — query the HANA catalog

→ Full specs: [`schema` subcommands](hana-schema.md)

| Subcommand | Returns | When to use |
|---|---|---|
| `hana schema list`   | Schemas in HANA                | Find which schema a replicated ABAP table lives in |
| `hana schema tables` | Tables in a schema             | Discover available raw tables for a view to reference |
| `hana schema fields` | Columns of a table             | Verify exact column names and types before writing the view definition |

### `table` — preview raw HANA table data

→ Full specs: [`table` subcommands](hana-table.md)

| Subcommand | Returns | When to use |
|---|---|---|
| `hana table preview` | Rows from `"<schema>"."<table>"` | Sample source data before/while authoring a view; supports physical `--schema` and logical `--logical-schema` (via `DB_SCHEMA_MAP`) |

> **Landscape note:** HANA ports (30013, 30015) are not reachable directly from a developer machine — they are only accessible from the ABAP application server. All HANA subcommands send requests to the ABAP server over HTTP; ABAP performs all HANA communication internally via the HTA API (`CL_CTS_HTA_API_FACTORY`) and ADBC.

---

## Repository Structure

HANA content lives in a `hana/` folder at the repo root. The folder path encodes the HANA package ID — path separators replace dots:

```
<repo-root>/
├── abap/                              ← ABAP objects (abapGit format)
└── hana/                              ← HANA objects
    └── com/
        └── example/
            └── myapp/
                ├── CustomerSegments.attributeview
                └── SalesAnalytics.calculationview
```

Folder path → HANA package ID: `hana/com/example/myapp/` → `com.example.myapp`

### File naming

`<ObjectName>.<suffix>` — matching exactly the HANA object name and suffix in `_SYS_REPO.ACTIVE_OBJECT` and `CTS_HOT_OBJECT`:

| File | `OBJECT_NAME` | `OBJECT_SUFFIX` |
|---|---|---|
| `CustomerSegments.attributeview` | `CustomerSegments` | `attributeview` |
| `SalesAnalytics.calculationview` | `SalesAnalytics` | `calculationview` |

### Supported file types

| Extension | HANA object type |
|---|---|
| `.attributeview` | Attribute View (XML model) |
| `.calculationview` | Calculation View (XML model — graphical or script-based) |

> Other XS-classic suffixes (`.hdbview`, `.hdbprocedure`, `.hdbtablefunction`) are **not in scope**. For SQL-script logic, use a CDS table function on the ABAP side.

---

## REST Endpoints

All HANA actions share the same ABAP REST resource at `/sap/bc/z_abapgit_agent/hana`. The action is specified in the request body.

### `deploy` — transport check (phase 1)

**POST** `/sap/bc/z_abapgit_agent/hana`

```json
{ "action": "check", "hanaPackage": "com.example.myapp", "abapPackage": "" }
```

Response:
```json
{ "success": true, "transport_required": true }
```

### `deploy` — deploy objects (phase 2)

**POST** `/sap/bc/z_abapgit_agent/hana`

```json
{
  "action":      "deploy",
  "hanaPackage": "com.example.myapp",
  "abapPackage": "",
  "transport":   "DEVK900123",
  "force":       false,
  "dryRun":      false,
  "objects": [
    { "filename": "SalesAnalytics.calculationview", "type": "calculationview", "content": "<?xml version=\"1.0\" ..." }
  ]
}
```

Response:
```json
{
  "success":   true,
  "transport": "DEVK900123",
  "deployed":  1,
  "skipped":   0,
  "results": [
    { "filename": "SalesAnalytics.calculationview", "type": "calculationview", "status": "deployed" }
  ]
}
```

### `import`

**POST** `/sap/bc/z_abapgit_agent/hana`

```json
{
  "action":      "import",
  "hanaPackage": "com.example.myapp",
  "abapPackage": "ZMYAPP",
  "transport":   "",
  "subpackages": false,
  "dryRun":      false
}
```

Response:
```json
{
  "success":   true,
  "transport": "DEVK900123",
  "objects": [
    {
      "hana_package": "com.example.myapp",
      "name":         "SalesAnalytics",
      "suffix":       "calculationview",
      "filename":     "hana/com/example/myapp/SalesAnalytics.calculationview",
      "content":      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Calculation:scenario ..."
    }
  ]
}
```

### `list`

**POST** `/sap/bc/z_abapgit_agent/hana`

```json
{
  "action":      "list",
  "hanaPackage": "com.example.myapp",
  "subpackages": false
}
```

Response:
```json
{
  "success": true,
  "package": "com.example.myapp",
  "objects": [
    { "name": "CustomerSegments", "suffix": "attributeview",   "deploy_state": "deployed",     "hot_status": "A" },
    { "name": "DraftView",        "suffix": "calculationview", "deploy_state": "not_deployed", "hot_status": "I" }
  ]
}
```

### `schema`

**POST** `/sap/bc/z_abapgit_agent/hana`

Three variants share a single endpoint, dispatched by `subaction`:

```json
{ "action": "schema", "subaction": "list",   "filter": "%CORE%",  "limit": 100 }
{ "action": "schema", "subaction": "tables", "schema": "SAP_FRA_CORE_S4", "filter": "T0%", "limit": 100 }
{ "action": "schema", "subaction": "fields", "schema": "SAP_FRA_CORE_S4", "table": "T001" }
```

Response shapes:

```json
{ "success": true, "subaction": "list",   "rows": [{ "schema_name": "SYS" }, ...] }
{ "success": true, "subaction": "tables", "rows": [{ "table_name": "T001", "is_column_table": "TRUE" }, ...] }
{ "success": true, "subaction": "fields", "rows": [{ "position": 1, "column_name": "MANDT", "data_type_name": "NVARCHAR", "length": 3, "scale": 0, "is_nullable": "FALSE" }, ...] }
```

On error:

```json
{ "success": false, "subaction": "...", "error": "schema is required" }
```

---

## ABAP Implementation

### Files

| File | Purpose |
|---|---|
| `abap/zcl_abgagt_resource_hana.clas.abap` | Single REST resource for all HANA subcommands |
| `abap/zcl_abgagt_resource_hana.clas.xml` | Metadata |
| `abap/zcl_abgagt_command_hana.clas.abap` | Business logic — routes `action` to `deploy`, `import`, `list`, `check` methods |
| `abap/zcl_abgagt_command_hana.clas.xml` | Metadata |

Register in:
- `zif_abgagt_command.intf.abap` → constant `gc_hana = 'HANA'`
- `zcl_abgagt_cmd_factory.clas.abap` → `HANA → ZCL_ABGAGT_COMMAND_HANA`
- `zcl_abgagt_rest_handler.clas.abap` → route `/sap/bc/z_abapgit_agent/hana`

### Key ABAP classes used

| Class / FM | Used by |
|---|---|
| `CL_CTS_HTA_API_FACTORY` | `deploy`, `import`, `list` — entry point for all HTA operations |
| `IF_CTS_HTA_OBJECT->deploy()` | `deploy` — activates object in HANA from HOT content |
| `IF_CTS_HTA_API_FACTORY->create_full_package_hana_name()` | `import`, `list` — loads package + all objects |
| `IF_CTS_HTA_COMPONENT->get_deploy_state()` | `list` — deployed / not_deployed per object |
| `CTS_HOT_OBJECT` (MODIFY / SELECT) | `deploy` writes content; `list` reads `hot_status` |
| `CTS_API_CREATE_CHANGE_REQUEST` | `deploy`, `import` — create transport when required |
| `RS_CORR_INSERT` | `deploy`, `import` — assign objects to transport (`LIMU HOTO`, `R3TR HOTA`) |
| `TDEVC` (SELECT `korrflag`) | `check` — determine whether transport is required |

### `ty_request` (shared across all actions)

```abap
TYPES: BEGIN OF ty_request,
         action      TYPE string,     " 'check' | 'deploy' | 'import' | 'list'
         hanapackage TYPE string,
         abappackage TYPE string,
         transport   TYPE trkorr,
         force       TYPE abap_bool,  " deploy only
         dryrun      TYPE abap_bool,  " deploy, import
         subpackages TYPE abap_bool,  " import, list
         objects     TYPE ty_objects, " deploy only
       END OF ty_request.
```

---

## Migration from separate commands

The existing `hana-deploy` REST endpoint (`/sap/bc/z_abapgit_agent/hanadeploy`) and ABAP class (`ZCL_ABGAGT_COMMAND_HANA_DEPLOY`) are replaced by the unified `hana` command. The new endpoint is `/sap/bc/z_abapgit_agent/hana`.

Node.js: `src/commands/hana-deploy.js` is replaced by `src/commands/hana.js`.

---

## Related

- [transport command](transport-command.md) — list or create transport requests
- SAP transaction `SCTS_HOTA_ORGANIZER` — UI for HANA object transport management
