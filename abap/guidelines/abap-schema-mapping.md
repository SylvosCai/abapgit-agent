---
layout: default
title: ABAP Schema Mapping ($ABAP.schema)
nav_order: 28
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# ABAP Schema Mapping (`$ABAP.schema`)

When ABAP code needs to read tables from a HANA schema **other than the local
ABAP schema** — a replicated schema, or the ABAP schema of a co-deployed
application — schema names cannot be hard-coded. They differ across systems:

- The local ABAP schema is conventionally `SAP<sid>`, where `<sid>` is the
  ABAP system ID — different in dev, test, and production
- Replicated schemas are typically named after the source system — also
  different across landscapes

The `$ABAP.schema( <logical_name> )` macro insulates the source code from these
landscape differences. The macro resolves to a physical schema at activation
time, based on a mapping maintained per system in transaction `DB_SCHEMA_MAP`.

---

## When you need this

- Reading from a **replicated HANA schema** (data brought into HANA from a
  remote source system via SLT, SDI, or similar)
- Reading from the **ABAP schema of a different co-deployed application** on
  the same HANA tenant
- Any logic that must compile against schema names that vary across the
  dev/test/prod landscape

## When you DON'T need this

- Reading from **your own application's ABAP schema** (the default). Use a
  plain CDS view entity — schema-less ABAP SQL resolves to the local schema
  automatically.
- Reading SAP standard tables that exist in every ABAP system. Same as above.

---

## Where it can — and can't — be used

| Context | Supported? |
|---|---|
| CDS table function (AMDP method body, `BY DATABASE FUNCTION FOR HDB`) | ✅ |
| AMDP procedure (`BY DATABASE PROCEDURE FOR HDB`) | ✅ |
| ABAP SQL `SELECT ... FROM ...` (anywhere outside an AMDP body) | ❌ |
| CDS view entity (`define view entity`) | ❌ |
| CDS view (`define view`, legacy) | ❌ |
| HANA repository view XML (`.calculationview` `<dataSources>`) | ❌ — use the physical schema name directly; HANA resolves it at activation |

**Practical consequence**: if you need to read a replicated table and turn the
result into something an ABAP `SELECT` (or a CDS view) can consume, you **must**
wrap the access in a CDS table function or AMDP procedure. There is no other
path.

---

## Syntax

```abap
SELECT ... FROM "$ABAP.schema( FRA_CORE_ERP )"."BKPF"
```

- The macro and the table name are each wrapped in double quotes
- The logical schema name is case-sensitive — uppercase by convention
- The table name follows HANA case rules (replicated tables are usually
  uppercase but you should verify with `hana schema tables --schema <physical_name>`)

In an AMDP body, joining two tables from the same logical schema:

```abap
METHOD get_data BY DATABASE FUNCTION FOR HDB
                LANGUAGE SQLSCRIPT
                OPTIONS READ-ONLY.

  RETURN SELECT
           h.mandt AS client,
           h.bukrs AS company_code,
           h.belnr AS document_number,
           h.gjahr AS fiscal_year,
           i.lifnr AS vendor
         FROM "$ABAP.schema( FRA_CORE_ERP )".bkpf AS h
         INNER JOIN "$ABAP.schema( FRA_CORE_ERP )".bseg AS i
           ON  h.mandt = i.mandt
           AND h.bukrs = i.bukrs
           AND h.belnr = i.belnr
           AND h.gjahr = i.gjahr
         WHERE h.mandt = SESSION_CONTEXT( 'CLIENT' );

ENDMETHOD.
```

---

## Setup checklist

1. **Define the logical schema** in transaction `DB_SCHEMA_MAP` (one entry per
   ABAP system — the logical name is consistent across landscapes by design)
2. **Map it to the physical HANA schema** for each landscape (dev, test, prod
   each have their own physical name)
3. **Reference it via `$ABAP.schema(<NAME>)`** in your table function or AMDP

The setup is a one-time-per-system activity, typically owned by Basis or the
team operating the replication. As an application developer authoring a table
function, your concern is step 3 — pick the agreed logical name and use it.

---

## Discovering the physical data model

Before writing the table function or AMDP, find out what's actually in the
mapped HANA schema:

```bash
# Find the physical schema name (your basis team can also tell you)
abapgit-agent hana schema list --filter '%CORE%'

# List tables available in it
abapgit-agent hana schema tables --schema SAP_FRA_CORE_S4 --filter 'BKPF'

# Get the exact column names and types
abapgit-agent hana schema fields --schema SAP_FRA_CORE_S4 --table BKPF

# Sample rows — works against both physical and logical schemas
abapgit-agent hana table preview --schema SAP_FRA_CORE_S4 --table BKPF --limit 5
abapgit-agent hana table preview --logical-schema FRA_CORE_ERP --table BKPF --limit 5
```

`hana table preview --logical-schema NAME` is the fastest way to **validate that
your `DB_SCHEMA_MAP` entry actually resolves** to a queryable physical schema.
If the command returns rows, the mapping is correct and your table function
will also work. If it returns `schema_not_mapped`, the basis team hasn't
configured the mapping on this system yet.

Use those exact column names (case-sensitive) in your `$ABAP.schema(...)`
SELECT — HANA will not auto-correct casing in a quoted identifier.

---

## Common errors

| Symptom | Likely cause | Fix |
|---|---|---|
| `Schema "$ABAP.schema( X )" is not mapped` at activation | No entry for logical schema `X` in `DB_SCHEMA_MAP` on this system | Have basis set up the mapping; logical name must be active in the system you're activating in |
| `Table "BKPF" not found in schema X_PHYSICAL` at runtime | Mapping is set, but the physical schema doesn't contain that table | Verify with `hana schema tables --schema <physical_name>`; you may be pointing at the wrong logical schema |
| `Use of "$ABAP.schema(...)" is not allowed here` at compile time | You're trying to use the macro outside of an AMDP body or table function | Move the access into a table function and `SELECT` from the table function in your CDS view / ABAP |
| Data element resolution errors during DDLS activation | Return-type columns reference DDIC data elements not in the local DDIC (only in the replicated schema) | Use `abap.*` primitive types (`abap.char(N)`, `abap.dec(N,M)`) in the table function's `returns { ... }` block — see `ref --topic table-function` |

---

## Example — table function reading from a replicated schema

```cds
@EndUserText.label: 'Vendor invoice header (replicated)'
@ClientHandling.type: #CLIENT_DEPENDENT
define table function Z_TF_VENDOR_INVOICES_FOREIGN
returns {
  client          : abap.clnt;
  company_code    : abap.char(4);
  document_number : abap.char(10);
  fiscal_year     : abap.numc(4);
  vendor          : abap.char(10);
}
implemented by method ZCL_TF_VENDOR_INVOICES_FOREIGN=>GET_DATA;
```

```abap
CLASS zcl_tf_vendor_invoices_foreign DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    CLASS-METHODS get_data FOR TABLE FUNCTION z_tf_vendor_invoices_foreign.
ENDCLASS.



CLASS ZCL_TF_VENDOR_INVOICES_FOREIGN IMPLEMENTATION.

  METHOD get_data BY DATABASE FUNCTION FOR HDB
                  LANGUAGE SQLSCRIPT
                  OPTIONS READ-ONLY.

    RETURN SELECT
             h.mandt AS client,
             h.bukrs AS company_code,
             h.belnr AS document_number,
             h.gjahr AS fiscal_year,
             i.lifnr AS vendor
           FROM "$ABAP.schema( FRA_CORE_ERP )".bkpf AS h
           INNER JOIN "$ABAP.schema( FRA_CORE_ERP )".bseg AS i
             ON  h.mandt = i.mandt
             AND h.bukrs = i.bukrs
             AND h.belnr = i.belnr
             AND h.gjahr = i.gjahr
           WHERE h.mandt = SESSION_CONTEXT( 'CLIENT' );

  ENDMETHOD.
ENDCLASS.
```

The same source code activates in dev, test, and prod — `FRA_CORE_ERP`
resolves to whatever physical HANA schema each landscape has mapped.

---

## See also

- **CDS Table Functions** (`ref --topic table-function`) — for `define table function` + AMDP details, return-type pitfalls, activation order, runner classes
- **HANA Catalog Discovery** (`ref --topic hana-views` step 1, or `abapgit-agent hana schema --help`) — to find which physical schema and tables exist in your system
- **abapGit XML — CDS** (`ref --topic abapgit-cds`) — for the DDLS XML metadata (`SOURCE_TYPE F`) and AMDP class XML
