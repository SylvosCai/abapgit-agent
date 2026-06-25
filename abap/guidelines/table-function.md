---
layout: default
title: CDS Table Functions
nav_order: 26
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# CDS Table Functions

A **CDS table function** is a CDS view whose body is computed by **AMDP**
(ABAP Managed Database Procedure) running on HANA. It exposes the result of
a SQLScript procedure as if it were a CDS view, so you can use it with
`SELECT` from ABAP.

Two artifacts are always required:

1. **DDLS** (`*.ddls.asddls`) — the signature: `define table function`,
   return-row columns, pointer to the implementing AMDP method
2. **AMDP class** (`*.clas.abap`) — `INTERFACES if_amdp_marker_hdb` and a
   `CLASS-METHODS x FOR TABLE FUNCTION <ddls>` method implemented
   `BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT`

→ For the exact XML metadata files (`SOURCE_TYPE F`, `CLSCCINCL X`) and
boilerplate `.asddls` / `.clas.abap` source: `abapgit-agent ref --topic abapgit-cds`,
section "CDS Table Function (DDLS, SOURCE_TYPE F)".

---

## When to use a table function vs. a view entity

| Need | Use |
|---|---|
| Pure CDS DDL is enough (joins, aggregations, conditions) | **View entity** (`define view entity`, `SOURCE_TYPE W`) |
| SQLScript constructs (window functions, recursive CTE, MERGE-like joins, dynamic SQL) | **Table function** |
| Access a table in a **different / replicated** HANA schema via `$ABAP.schema( <logical> )` | **Table function** (CDS view entities don't support `$ABAP.schema(...)`) |

---

## Logical schema — accessing replicated / foreign schemas

Table functions can read from replicated HANA schemas or schemas of
co-deployed ABAP applications via the `$ABAP.schema( ... )` macro:

```sql
SELECT ...
  FROM "$ABAP.schema( FRA_CORE_ERP )".rbkp
```

This is the **only way** to do cross-schema reads from ABAP — CDS view
entities and plain ABAP SQL don't support the macro. The mapping from logical
to physical schema is maintained per landscape in transaction `DB_SCHEMA_MAP`.

→ Full guide (setup, syntax, common errors, end-to-end example):
  `abapgit-agent ref --topic abap-schema-mapping`

---

## Return-column types — what to use

| Type expression | Use it for |
|---|---|
| `abap.clnt` | client / MANDT |
| `abap.char(N)` | fixed CHAR (e.g. BUKRS → `abap.char(4)`, LIFNR → `abap.char(10)`) |
| `abap.numc(N)` | numeric character (e.g. GJAHR → `abap.numc(4)`) |
| `abap.dats` | date `YYYYMMDD` |
| `abap.tims` | time `HHMMSS` |
| `abap.dec(N,M)` | decimal (e.g. invoice amount → `abap.dec(23,2)`) |
| `abap.cuky` | currency key (5-char) |
| `abap.int4` | 4-byte integer |
| `abap.string` | unbounded NVARCHAR |
| `mandt`, `bukrs`, `lifnr`, … | DDIC data elements — **only if active in the same package as the DDLS** |

### ⚠️ Data elements from replicated tables are NOT in the local DDIC

`abapgit-agent view --objects WERTV8 --type DTEL` may say the data element
exists somewhere in the system, but the DDLS activator only resolves data
elements that are either (a) standard SAP_BASIS types or (b) active in the
same ABAP package as your DDLS.

**Symptom**:
```
Entity Z_TF_MY_FUNCTION: Data element WERTV8 parameter/field AMOUNT does not exist or not active
```

**Fix** — use a primitive `abap.*` type:
```cds
amount : abap.dec(23,2);     // instead of: amount : wertv8;
vendor_reference : abap.char(16);   // instead of: xblnr1
```

Or — if you want strong typing and reuse — create your own data elements
under your project namespace and reference them. (This is what production
table function packages typically do: e.g. `FRA_ERP_VENDOR`,
`FRA_ERP_DOCUMENT_DATE`.)

---

## Activation order — DDLS depends on the AMDP class

The DDLS contains `implemented by method ZCL_X=>METHOD`, so the DDLS cannot
activate until the AMDP class is active. The AMDP class declares
`CLASS-METHODS m FOR TABLE FUNCTION z_tf_x`, but the class compiles against
the **signature** of the DDLS — which exists from the first pull onwards.

**Recommended sequence:**

```bash
git add . && git commit -m "..." && git push
abapgit-agent pull --files src/zcl_tf_x.clas.abap         # class first
abapgit-agent pull --files src/z_tf_x.ddls.asddls         # then DDLS
```

A single `pull --files src/zcl_tf_x.clas.abap,src/z_tf_x.ddls.asddls` often
works on the second attempt — first pass activates the class, second pass
activates the DDLS. Don't be alarmed by "Activation cancelled" on attempt 1
if a follow-up pull reports 1 activated.

---

## `inspect` is NOT reliable for table-function DDLS

```bash
abapgit-agent inspect --files src/z_tf_x.ddls.asddls
✅ DDLS Z_TF_X - Syntax check passed     # ← but activation FAILS
```

`inspect` runs a **syntactic** check on the DDLS source, not a full DDIC
activation. Data-element-not-found and `$ABAP.schema` resolution errors
**slip through**.

**To see the real activation error**, use one of these:

1. **Recommended** — read the pull log in JSON:
   ```bash
   abapgit-agent pull --files src/z_tf_x.ddls.asddls --json
   ```
   Look at the `LOG_MESSAGES` array.

2. Activate manually in ADT (right-click → activate). The error popup is
   the most detailed.

3. SE80 → DDLS → activate log.

---

## Common errors and fixes

| Error message | Cause | Fix |
|---|---|---|
| `Data element X parameter/field Y does not exist or not active` | DTEL X is not in the local DDIC | Replace with `abap.<primitive>(...)` (see type table above) |
| `'Z_TF_X' is not a table function` (from a runner / SELECT) | The DDLS is inactive | Pull the AMDP class first, then the DDLS |
| `Annotation for AbapCatalog.dataMaintenance does not exist` | Wrong `SOURCE_TYPE` in the DDLS XML | Set `<SOURCE_TYPE>F</SOURCE_TYPE>` |
| `Compilation was canceled` for the AMDP class | Class compiles against a DDLS signature that hasn't been deserialized yet | Re-pull — the second attempt picks up the now-deserialized DDLS |
| `Error updating where-used list` after pulling the AMDP class | The DDLS the class refers to is missing or inactive | Make sure the `.ddls.asddls` file is in the same commit and re-pull both |
| Class shows `create private` on the server after pull | The source line `class zcl_x implementation.` uses **lowercase** name — abapGit can't match it to the serializer output | Write the implementation header in UPPERCASE: `CLASS ZCL_X IMPLEMENTATION.` |
| Pull `--sync-xml` wipes the DDLS XML to a minimal stub | `--sync-xml` was run after a pull that **failed** to deserialize the DDLS; the serializer output is empty, and you overwrote your good XML | Never use `--sync-xml` after a failed pull. Restore the XML from git history first, fix the underlying activation error, re-pull, then `--sync-xml` |

---

## Example — duplicate vendor invoice detector

A complete two-file table function over a replicated `RBKP` table:

`src/z_tf_duplicate_invoices.ddls.asddls`:
```cds
@EndUserText.label: 'Duplicate Vendor Invoices'
@ClientHandling.type: #CLIENT_DEPENDENT
define table function Z_TF_DUPLICATE_INVOICES
returns {
  client            : abap.clnt;
  company_code      : abap.char(4);
  document_number   : abap.char(10);
  fiscal_year       : abap.numc(4);
  vendor            : abap.char(10);
  vendor_reference  : abap.char(16);
  amount            : abap.dec(23,2);
  currency          : abap.cuky;
  dup_count         : abap.int4;
}
implemented by method ZCL_TF_DUPLICATE_INVOICES=>GET_DUPLICATES;
```

`src/zcl_tf_duplicate_invoices.clas.abap` (excerpt):
```abap
METHOD get_duplicates BY DATABASE FUNCTION FOR HDB
                      LANGUAGE SQLSCRIPT
                      OPTIONS READ-ONLY.

  RETURN SELECT
           r.mandt                  AS client,
           r.bukrs                  AS company_code,
           r.belnr                  AS document_number,
           r.gjahr                  AS fiscal_year,
           r.lifnr                  AS vendor,
           r.xblnr                  AS vendor_reference,
           r.rmwwr                  AS amount,
           r.waers                  AS currency,
           CAST( d.cnt AS INTEGER ) AS dup_count
         FROM "$ABAP.schema( FRA_CORE_ERP )".rbkp AS r INNER JOIN
              ( SELECT mandt, lifnr, xblnr, rmwwr, waers,
                       COUNT(*) AS cnt
                FROM "$ABAP.schema( FRA_CORE_ERP )".rbkp
                WHERE xblnr <> '' AND mandt = SESSION_CONTEXT( 'CLIENT' )
                GROUP BY mandt, lifnr, xblnr, rmwwr, waers
                HAVING COUNT(*) > 1 ) AS d
            ON r.mandt = d.mandt AND
               r.lifnr = d.lifnr AND
               r.xblnr = d.xblnr AND
               r.rmwwr = d.rmwwr AND
               r.waers = d.waers
         WHERE r.mandt = SESSION_CONTEXT( 'CLIENT' );

ENDMETHOD.
```

Call it from regular ABAP:
```abap
SELECT vendor, vendor_reference, amount, currency, dup_count
  FROM z_tf_duplicate_invoices( )
  INTO TABLE @DATA(lt_dups)
  UP TO 100 ROWS.
```

---

## Workflow recap

```
Create/modify table function?
  └── ✅ Use:
        edit .ddls.asddls + .clas.abap (no syntax — inspect is unreliable for TFs)
        → [abaplint]
        → commit → push
        → pull --files <clas>.clas.abap        (activates AMDP)
        → pull --files <ddls>.ddls.asddls --sync-xml   (activates DDLS)
        → (verify) abapgit-agent run --class <runner_class>
```
