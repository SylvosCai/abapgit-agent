---
layout: default
title: HANA Views Authoring
nav_order: 27
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# HANA Views Authoring (`.attributeview`, `.calculationview`)

This guide covers everything you need to author and deploy HANA repository
views with `abapgit-agent`. Read it before you write your first view — most of
the rules below are non-obvious and the default approach will fail.

In scope: `.attributeview` (XML attribute view) and `.calculationview` (XML
calculation view, graphical or script-based). Not in scope: `.hdbview`,
`.hdbprocedure`, `.hdbtablefunction` — those XS-classic types are not
supported by this tool. For SQL-script logic, use a CDS table function (see
`abapgit-agent ref --topic table-function`).

---

## Step 1 — Discover the data model first

Before writing the view, find out which schemas, tables, and columns are
available. **Never guess** — column names are case-sensitive in HANA, and
replicated schemas may have a subset of columns or renamed columns.

```bash
# 1a. Find the schema (HANA may have many: SAP<sid>, replication targets, etc.)
abapgit-agent hana schema list --filter '%CORE%'

# 1b. Find the table inside that schema
abapgit-agent hana schema tables --schema SAP_FRA_CORE_S4 --filter 'T001%'

# 1c. Get the exact column names and types
abapgit-agent hana schema fields --schema SAP_FRA_CORE_S4 --table T001

# 1d. Sample a few rows to verify the data actually looks like what you expect
abapgit-agent hana table preview --schema SAP_FRA_CORE_S4 --table T001 --limit 5
abapgit-agent hana table preview --schema SAP_FRA_CORE_S4 --table T001 --where "BUKRS = '1000'"
```

`hana schema` queries HANA's `SYS.SCHEMAS` / `SYS.TABLES` / `SYS.TABLE_COLUMNS`
dictionary directly via ADBC. It returns only physical tables (no views or
synonyms). Default limit is 100 rows per query — override with `--limit N`.

`hana table preview` runs `SELECT ... FROM "<schema>"."<table>"` against the actual
table data. Use `--logical-schema <name>` if your team uses a `DB_SCHEMA_MAP` entry
(landscape-portable) instead of hard-coding the physical schema name.

Use the discovered column names **verbatim** in the view XML — HANA is
case-sensitive in identifiers within view definitions.

---

## Step 2 — Never write the XML from scratch

```
❌ WRONG: Read SAP HANA modeling docs → write <View:ColumnView> XML by hand
✅ CORRECT: Import an existing activated view → use it as a template
```

**Why this rule is non-negotiable**:

HANA Studio's source view shows a **display transform** of the view — the
`<View:ColumnView>` XML you see in the editor is NOT what's stored on disk.
What HANA actually persists in `_SYS_REPO.ACTIVE_OBJECT.CDATA` is a different
XML schema, e.g. `<Calculation:scenario>` with namespace
`xmlns:Calculation="http://www.sap.com/ndb/BiModelCalculation.ecore"`.

Deploying Studio's displayed XML always fails with `XML not valid.
Please verify Calculation View.`

**The right way**:

```bash
# Import some existing activated view from any HANA package
abapgit-agent hana view import --package <some.existing.package>

# This writes the real on-disk XML into hana/<dotted/path>/<name>.calculationview
# Open it, copy it, rename it, and edit it
cp hana/some/existing/Sample.calculationview \
   hana/your/target/YourNewView.calculationview

# Then edit YourNewView.calculationview:
#   - change `id="..."` in <Calculation:scenario>
#   - change `<descriptions defaultDescription="..."/>`
#   - update <dataSources>, <viewAttributes>, <logicalModel> to your needs
```

If the target HANA package has no existing view to copy, **import from any
other package on the same system** — the XML schema is identical.

---

## Step 3 — Iterate with `--force`

`hana view deploy` skips objects whose `HOT_STATUS = 'A'` (already active in
HANA). On the **second** deploy after a successful first activation, the
command reports `skipped (already active)` and does NOT pick up your changes.

When iterating on a view's definition:

```bash
abapgit-agent hana view deploy --file hana/.../MyView.calculationview --force
```

`--force` redeploys regardless of `HOT_STATUS`. Use it from the second deploy
onwards until the view is final.

---

## Step 4 — Reading deploy failures

`hana view deploy` resolves error messages from HANA through the `SCTS_HOT` /
`SCTS_HTA` message classes and prints each on its own line. Read **top to
bottom** — the actionable error is usually a short line in the middle, buried
between header noise and trailer summary:

```
❌ Error: deploy_failed
  File                              Type             Status
  ────────────────────────────────  ───────────────  ────────────
  MyView.calculationview            calculationview  failed
    └─ Deploy failed (E)
  HANA activation ID: 1731, HANA return code: 40136
  HANA Message: Repository: Activation failed for at least one object;...
  . Please see CheckResults for details
  MyView.calculationview (pkg) activation failed
  Repository: Encountered an error in repository runtime extension;...
  XML not valid. Please verify Calculation View.           ← ACTIONABLE
  HANA time stamp (UTC+2): ..., HANA severity: 3, HANA return code: 40117
  Summary of objects with activation errors:
  MyView.calculationview (pkg)
  For analysis of import/activation errors, see SAP Note 2109690
```

The single actionable error here is `XML not valid. Please verify Calculation
View.` — everything else is metadata about the failure.

> **Important**: `abapgit-agent inspect` and `abapgit-agent syntax` do NOT
> validate HANA views. They only check that ABAP source is parseable. For
> HANA, the only place to see the real activation error is the `deploy`
> response.

### Common error → cause

| HANA error message | Cause |
|---|---|
| `XML not valid. Please verify Calculation View.` | The XML root/namespace doesn't match what HANA expects. You probably wrote it from scratch or copied from Studio's source view. Re-import a template (Step 2). |
| `Syntax error: unknown variable name: …` | `.hdbview` content reached HANA. We don't support `.hdbview` — use `.calculationview` instead. |
| `A SQL error occurred while creating a SQL view` | The view references a non-existent table or column. Verify with `hana schema fields` (Step 1). |
| `unknown column: <NAME>` | Column casing wrong, or column not in the replicated schema. Look up in `hana schema fields` and match exactly. |
| `Repository: Activation failed for at least one object` (only) | Look at the next message — this one is just the header. |

---

## Step 5 — Schema references inside the view XML

The `<dataSources>` element references **physical HANA schemas by name**:

```xml
<dataSources>
  <DataSource id="MyDataSource">
    <viewAttributes>...</viewAttributes>
    <columnObject schemaName="SAP_FRA_CORE_S4" columnObjectName="T001"/>
  </DataSource>
</dataSources>
```

```
❌ WRONG: Use "$ABAP.schema( FRA_CORE_ERP )" inside view XML
   (that's an AMDP-only macro; HANA views don't support it)

✅ CORRECT: Use the physical schemaName directly
   (HANA resolves it at activation time)
```

**Cross-landscape portability** of schema names is handled at HANA level via
synonyms or replication configuration — **not** inside the view XML. If the
view needs to work in a system where the replicated schema has a different
name, the user must set up a synonym on the target system, OR rename the
schema reference in the XML for that landscape. This is a deployment concern,
not an authoring concern.

For cross-schema access from **ABAP** (not from a HANA view), use a CDS table
function with `$ABAP.schema(...)` — see `ref --topic abap-schema-mapping`.

---

## Step 6 — Test the view end-to-end

After a successful deploy:

```bash
# 6a. Confirm activation status
abapgit-agent hana view list --package <pkg>
# Expect: deploy_state=deployed, hot_status=A

# 6b. Preview the view's data
abapgit-agent hana view preview --file hana/<pkg>/<View>.calculationview
abapgit-agent hana view preview --file hana/<pkg>/<View>.calculationview --limit 20
abapgit-agent hana view preview --file hana/<pkg>/<View>.calculationview --where "COL = 'X'"
abapgit-agent hana view preview --file hana/<pkg>/<View>.calculationview --columns A,B,C
```

`hana view preview` runs `SELECT <columns> FROM "_SYS_BIC"."<pkg>/<view>" WHERE … LIMIT …`
via ADBC and renders the result. Use `--vertical` for wide views, `--json` for piping.

If you need to run raw SQL (e.g. a JOIN across multiple views), use any SQL client or
write a quick runner class — the activated view is at:

```sql
SELECT * FROM "_SYS_BIC"."<package_id>/<view_name>" LIMIT 10;
```

> **Authorisation note**: the view's deploy-time `applyPrivilegeType` setting governs ACL.
> The default `SQL_ANALYTIC_PRIVILEGE` requires the caller to hold an analytic-privilege
> grant. For test / scratch views that you only want to preview, deploy with
> `applyPrivilegeType="NONE"` to make the view freely queryable.

If the data is wrong (the rows, not the columns), the issue is in the view's
`<filter>` / `<keyMapping>` / SQL-script definition — NOT in deploy. Fix the
XML and redeploy with `--force`.

---

## `hot_status` reference

`hana view list` shows the `HOT_STATUS` field from `CTS_HOT_OBJECT`:

| Value | Meaning | What to do |
|---|---|---|
| `A` | Active in HANA — view is queryable via `_SYS_BIC` | Nothing (success state) |
| `I` | Inactive — local content uploaded but not yet activated | Run `hana view deploy --force` |
| `N` | New — content present but never activated | Run `hana view deploy --force` |
| `E` | Deploy error — last activation failed | Read the deploy response; fix the underlying error; redeploy `--force` |
| `D` | To be deleted | Out of scope for this CLI |

---

## What NOT to do

- **Don't `MODIFY cts_hot_object` directly** to "fix" stuck state. Use
  `deploy --force` to redeploy cleanly; the HTA API will reset state correctly.
- **Don't run `--sync-xml` after a failed pull/deploy** of a `.calculationview`
  / `.attributeview`. (These are not abapGit objects — `--sync-xml` doesn't
  apply to them. The HANA file lives in `hana/` and is not serialized through
  abapGit's class/DDLS serializers.)
- **Don't expect `inspect` or `syntax` to validate the XML**. They don't —
  they only check ABAP source. The only validator is HANA itself, via deploy.
- **Don't reuse a deployed view's name for an unrelated view**. Object key is
  `(PACKAGE_ID, OBJECT_NAME, OBJECT_SUFFIX)`. A deploy with the same triple
  REPLACES the existing view after activation. Rename when keeping both.

---

## Quick workflow

```
Need a new HANA view?
  ├── 1. hana schema fields …       (discover the table columns)
  ├── 1b. hana table preview …      (sample source data)
  ├── 2. hana view import …         (get a template from an existing view)
  ├── 3. cp template → rename → edit
  ├── 4. hana view deploy --file … --force
  │     └── on error: read deploy response top-to-bottom → fix → redeploy
  ├── 5. hana view list             (verify hot_status=A, deploy_state=deployed)
  └── 6. hana view preview --file …  (sample the data; --where to filter)
```
