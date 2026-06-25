---
layout: default
title: view subcommands
nav_order: 1
parent: hana - HANA Views & Catalog
grand_parent: Development Commands
---

# `hana view` subcommands

Manage HANA repository views (attribute views and calculation views) on the HANA tenant. Files in the local `hana/` folder are the single source of truth; `view deploy` pushes them into HANA and `view import` does the one-time bootstrap of an existing HANA package back into the repo.

| Subcommand | Direction | When to use |
|---|---|---|
| `hana view deploy` | `hana/` files → HANA | Day-to-day: push edited files into HANA |
| `hana view import` | HANA → `hana/` files | One-time bootstrap: pull existing HANA content into repo |
| `hana view list`   | Read HANA state      | Verify what is deployed in a package and its status |
| `hana view drop`   | HANA → ⌀             | Delete a HANA view from the HANA repository (local file kept) |
| `hana view preview`| Read HANA data       | Preview the data produced by an activated view (via `_SYS_BIC`) |

---

## `hana view deploy`

Push local `hana/` files into the HANA repository. The files are the **single source of truth** — this command makes HANA match them, analogous to how `pull` makes the ABAP system match git.

### Usage

```bash
# Deploy a single file
abapgit-agent hana view deploy --file hana/com/example/myapp/SalesAnalytics.calculationview

# Deploy all artifacts in a package folder
abapgit-agent hana view deploy --folder hana/com/example/myapp/

# Assign to an existing transport request
abapgit-agent hana view deploy --folder hana/com/example/myapp/ --transport DEVK900123

# First-time deploy — objects not yet in TADIR, supply ABAP devclass
abapgit-agent hana view deploy --folder hana/com/example/myapp/ --abap-package ZMYAPP

# Force re-deploy even if already active in HANA
abapgit-agent hana view deploy --folder hana/com/example/myapp/ --force

# Dry run — show what would be deployed without writing anything
abapgit-agent hana view deploy --folder hana/com/example/myapp/ --dry-run
```

### Options

| Option | Description |
|---|---|
| `--file <path>` | Path to a single HANA artifact file. Package ID derived from folder path relative to `hana/` |
| `--folder <path>` | Path to a package folder. Deploys all supported artifacts; package ID derived from folder path |
| `--transport <trkorr>` | Existing transport request to assign objects to. If the package is transportable and this is omitted, a transport is selected via the standard transport picker |
| `--abap-package <devc>` | ABAP devclass for objects not yet in TADIR. Required on first deploy |
| `--force` | Re-deploy even if the object is already active in HANA |
| `--dry-run` | Validate content without writing to HOT tables, HANA, or transport |

`--file` and `--folder` are mutually exclusive.

### How it works

**Phase 1 — Transport check**

Before sending objects, the Node.js command calls the ABAP backend to check whether the ABAP package is transportable (`TDEVC-KORRFLAG`). If it is and no `--transport` was given, the standard transport picker runs (hook → interactive → auto-create).

**Phase 2 — Deploy**

Three steps per object in the ABAP backend:

1. Write file content into `CTS_HOT_OBJECT` (`hot_status = 'I'`)
2. Deploy from HOT to HANA via HTA API (`IF_CTS_HTA_OBJECT->deploy()`) — activates in HANA
3. If a transport was resolved: assign via `RS_CORR_INSERT` (`LIMU HOTO`)

### Output

**Success:**
```
🚀 Deploying to HANA  (package: com.example.myapp)

  File                                Type                  Status
  ──────────────────────────────────  ────────────────────  ────────────
  CustomerSegments.attributeview      attributeview         deployed
  SalesAnalytics.calculationview      calculationview       deployed

✅ Deployed 2 object(s).  Transport: DEVK900123
```

**Already active (no `--force`):**
```
  File                            Type                  Status
  ──────────────────────────────  ────────────────────  ────────────
  SalesAnalytics.calculationview  calculationview       skipped
    └─ already active

✅ Deployed 0 object(s).  Skipped: 1 (already active)
   Use --force to redeploy already-active objects.
```

**Dry run:**
```
🚀 Dry-run deploy to HANA  (package: com.example.myapp)

  File                                Type                  Valid
  ──────────────────────────────────  ────────────────────  ─────
  SalesAnalytics.calculationview      calculationview       yes

  1 object(s) would be deployed.
```

**Error — package not in TADIR:**
```
❌ Error: deploy_failed

  HANA package not registered in TADIR: COM/EXAMPLE/MYAPP
  Re-run with --abap-package <devclass>.
```

**Error — HANA activation failure (XML/SQL validation):**

Each error line from the HANA activator is resolved through its `SCTS_HOT` / `SCTS_HTA`
message class and rendered as a separate line. Read top to bottom — the most
actionable line is usually the short one in the middle (e.g. `XML not valid.`,
`Syntax error: …`).

```
❌ Error: deploy_failed
  File                              Type             Status
  ────────────────────────────────  ───────────────  ────────────
  SalesAnalytics.calculationview    calculationview  failed
    └─ Deploy failed (E)
  HANA activation ID: 1731, HANA return code: 40136
  HANA Message: Repository: Activation failed for at least one object;At least one runtime reported an error during activation
  . Please see CheckResults for details
  SalesAnalytics.calculationview (com.example.myapp) activation failed
  Repository: Encountered an error in repository runtime extension; DeployCalcView: Deploy Calculation View failed, authoring
  XML not valid. Please verify Calculation View.
  HANA time stamp (UTC+2): 2026-06-24 05:00:30.4250000, HANA severity: 3, HANA return code: 40117
  Summary of objects with activation errors:
  SalesAnalytics.calculationview (com.example.myapp)
  For analysis of import/activation errors, see SAP Note 2109690
```

> Implementation note: error messages from `IF_CTS_HTA_OBJECT->deploy()` arrive
> as `sprot_u` records with `var1..var4` placeholders. The command resolves them
> through `MESSAGE ID … TYPE … NUMBER … WITH … INTO` so templates like
> `SCTS_HOT/563` (`HANA activation ID: &1, HANA return code: &2`) and
> chunked-text templates like `SCTS_HOT/532` (`&1&2&3&4`) render properly
> instead of as raw `var1 var2 var3 var4` segments.

---

## `hana view import`

Import existing HANA objects from the HANA repository into the local `hana/` folder. **One-time bootstrap only.** After the initial import, use `hana view deploy` for all further changes.

### Usage

```bash
# Bootstrap a HANA package into the local repo
abapgit-agent hana view import --package com.example.myapp --abap-package ZMYAPP

# Bootstrap including sub-packages
abapgit-agent hana view import --package com.example --subpackages --abap-package ZMYAPP

# Use an existing transport request
abapgit-agent hana view import --package com.example.myapp --abap-package ZMYAPP --transport DEVK900123

# Dry run — show what would be imported without writing files or transport entries
abapgit-agent hana view import --package com.example.myapp --dry-run
```

### Options

| Option | Description |
|---|---|
| `--package <id>` | **(Required)** HANA package ID to import (e.g. `com.example.myapp`) |
| `--subpackages` | Include sub-packages recursively. Default: `false` |
| `--abap-package <devc>` | ABAP devclass to register HOTA objects under in TADIR. Required when objects are not yet registered |
| `--transport <trkorr>` | Existing transport request. If the package is transportable and omitted, a transport is selected via the standard picker |
| `--dry-run` | Show what would be imported without writing files, HOT tables, or transport entries |

### How it works

1. **Discover** — queries HTA API (`create_full_package_hana_name`) for all active objects in the HANA package
2. **Read content** — ABAP reads each object's `cdata`/`bdata` from HANA and returns it in the response
3. **Write files** — Node.js writes each object as a file under `hana/`, deriving the folder path from the package ID
4. **Assign transport** — `RS_CORR_INSERT` for each object (`LIMU HOTO`) and the package (`R3TR HOTA`)

### Output

**Success:**
```
🚀 Importing from HANA  (package: com.example.myapp)

  Object                          Suffix              Status
  ──────────────────────────────  ──────────────────  ────────────────────────
  CustomerSegments                attributeview       written
  SalesAnalytics                  calculationview     written

✅ Wrote 2 object(s) to hana/com/example/myapp/
```

**Dry run:**
```
🚀 Importing from HANA  (package: com.example.myapp)

  Object                          Suffix              Status
  ──────────────────────────────  ──────────────────  ────────────────────────
  CustomerSegments                attributeview       would write
  SalesAnalytics                  calculationview     would write

  2 object(s) would be written to hana/com/example/myapp/
```

**Partial failure:**
```
🚀 Importing from HANA  (package: com.example.myapp)

  Object                          Suffix              Status
  ──────────────────────────────  ──────────────────  ────────────────────────
  CustomerSegments                attributeview       written
  BrokenView                      calculationview     error: not found in _SYS…

✅ Wrote 1 object(s) to hana/com/example/myapp/
❌ 1 object(s) failed to import.
```

---

## `hana view list`

List all HANA objects in a package and their deployment status. Use this to verify what is deployed in HANA, compare it against the local `hana/` files, and spot missing or stale objects.

### Usage

```bash
# List all objects in a HANA package
abapgit-agent hana view list --package com.example.myapp

# Include sub-packages
abapgit-agent hana view list --package com.example --subpackages

# JSON output (for scripting)
abapgit-agent hana view list --package com.example.myapp --json
```

### Options

| Option | Description |
|---|---|
| `--package <id>` | **(Required)** HANA package ID to list (e.g. `com.example.myapp`) |
| `--subpackages` | Include sub-packages recursively. Default: `false` |
| `--json` | Output raw JSON |

### How it works

Calls `IF_CTS_HTA_API_FACTORY->create_full_package_hana_name()` to load the package and all its objects from the HTA repository. For each object, reads:

- `object_key` — package name, object name, suffix
- `get_deploy_state()` — whether the object is deployed in HANA (`deployed` / `not_deployed`)
- `hot_status` from `CTS_HOT_OBJECT` — HTA internal status (`A` = active, `I` = inactive/pending)

### Output

```
📋 HANA package: com.example.myapp

  Object                      Suffix                Deploy state   HOT status
  ──────────────────────────  ────────────────────  ─────────────  ──────────
  CustomerSegments            attributeview         deployed       A
  SalesAnalytics              calculationview       deployed       A
  DraftView                   calculationview       not_deployed   I

  3 object(s) found.  ⚠️  1 not deployed — run: abapgit-agent hana view deploy --folder hana/com/example/myapp/
```

**Empty package:**
```
📋 HANA package: com.example.myapp

  No objects found in HTA repository for this package.
  If the package exists in HANA but not in HTA, run: abapgit-agent hana view import --package com.example.myapp
```

**JSON output (`--json`):**
```json
{
  "success": true,
  "package": "com.example.myapp",
  "objects": [
    {
      "name":         "CustomerSegments",
      "suffix":       "attributeview",
      "deploy_state": "deployed",
      "hot_status":   "A"
    },
    {
      "name":         "DraftView",
      "suffix":       "calculationview",
      "deploy_state": "not_deployed",
      "hot_status":   "I"
    }
  ]
}
```

---

## `hana view drop`

Delete a HANA view from the HANA repository. The local file under `hana/` is preserved — accidental drops can be recovered with `hana view deploy --file <same path> --force`.

### Usage

```bash
abapgit-agent hana view drop --file <path> [--dry-run]
```

### Options

| Option | Description |
|---|---|
| `--file <path>` | Local path; encodes HANA package + name + suffix |
| `--dry-run` | Show what would be dropped without changes |

### How it works

1. Parse the `--file` path → derive `hanaPackage`, `objectName`, `objectSuffix`.
2. Look up the row in `CTS_HOT_OBJECT`. If absent, return success with `not found in HOT` (idempotent).
3. Set `hot_status = 'D'` (`co_hot_status_to_be_deleted`) via `IF_CTS_HOT_DB_ACCESS->modify_cts_hot_object`.
4. Call `IF_CTS_HTA_OBJECT->deploy()` — the HTA propagates the delete to HANA and cleans up the HOT row.

### Output samples

**Success:**
```
🚀 Dropping from HANA  (package: tmp.abgagt)

  Object                            Status
  ────────────────────────────────  ──────────────
  SalesAnalytics.calculationview    dropped

✅ Dropped 1 object from HANA.
   Local file kept: hana/tmp/abgagt/SalesAnalytics.calculationview
```

**Already absent (idempotent):**
```
🚀 Dropping from HANA  (package: tmp.abgagt)

  Object                            Status
  ────────────────────────────────  ──────────────
  SalesAnalytics.calculationview    skipped
    └─ not found in HOT

✅ No objects dropped (already absent in HANA).
```

**Dry-run:**
```
🚀 Dry-run drop from HANA  (package: tmp.abgagt)

  Object                            Status
  ────────────────────────────────  ──────────────
  SalesAnalytics.calculationview    would drop

  1 object(s) would be dropped.
```

---

## `hana view preview`

Preview the data produced by an activated HANA view. The view is queried at its runtime
column-view location `"_SYS_BIC"."<package>/<view>"` via ADBC.

### Usage

```bash
abapgit-agent hana view preview --file <path> [options]
```

### Options

| Option | Required | Description |
|---|---|---|
| `--file <path>` | yes | Local path to the HANA view; encodes package + view name |
| `--limit N` | no | Max rows. Default 100. Max 500. |
| `--offset N` | no | Skip first N rows for paging. Default 0. |
| `--where "<expr>"` | no | SQL WHERE clause body (no `WHERE` keyword) |
| `--columns A,B,C` | no | Project only the given columns. Default: all columns. |
| `--vertical` | no | Render one column per line (good for wide views) |
| `--compact` | no | Truncate cell values to column width |
| `--json` | no | Emit raw JSON; bypass the table renderer |

### How it works

1. Verify the view is active in `_SYS_REPO.ACTIVE_OBJECT` (fail-fast with `view_not_active`).
2. Build the SQL — all string fragments routed through `cl_abap_dyn_prg=>escape_quotes`:
   ```sql
   SELECT <columns or *>
     FROM "_SYS_BIC"."<package>/<view>"
    [WHERE <where>]
    LIMIT <limit> OFFSET <offset>
   ```
3. Execute via ADBC; build an all-string row struct via `cl_sql_result_set->get_struct_ref( md_tab = ..., string_only = abap_true )`.
4. Stream rows back as a `string_table` per row plus a typed column-metadata array.

### Output samples

**Success:**
```
📋 HANA view: tmp.abgagt/SalesAnalytics  (limit=100)

  MESSAGE             DUMMY_MEASURE
  ──────────────────  ───────────────
  integration test    1

  1 row(s) returned.
```

**Filtered:**
```
📋 HANA view: tmp.abgagt/SalesAnalytics  (limit=100, where: DUMMY_MEASURE = 1)

  MESSAGE             DUMMY_MEASURE
  ──────────────────  ───────────────
  integration test    1

  1 row(s) returned.
```

**Empty:**
```
📋 HANA view: tmp.abgagt/SalesAnalytics

  No rows returned.
```

**Vertical (`--vertical`):**
```
📋 HANA view: tmp.abgagt/SalesAnalytics

────── Row 1 ──────
  MESSAGE         : integration test
  DUMMY_MEASURE   : 1

  1 row(s) returned.
```

**Not active:**
```
❌ Error: view_not_active
   View "tmp.abgagt/SalesAnalytics" is not active in HANA. Deploy it first: abapgit-agent hana view deploy --file <path>
```

**Insufficient privilege** (no analytic privilege on `_SYS_BIC.<view>`):

The deploy-time `applyPrivilegeType` setting in the view XML governs ACL.
`SQL_ANALYTIC_PRIVILEGE` (default) requires the caller to hold an analytic
privilege grant on the view. For test/scratch views, deploy with
`applyPrivilegeType="NONE"` to make the view freely queryable.

```
❌ Error: invalid_sql
   A database error occured. The error message in the database was: "insufficient privilege: …".
```

### Out of scope

- Parameterised views (calc-view `PLACEHOLDER`) — not supported in v1.
- Cross-view JOIN — write raw SQL.
- Server-side cursor / streaming — `--offset` is the only paging mechanism.
