---
layout: default
title: CDS Views
nav_order: 9
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Creating CDS Views

**Searchable keywords**: CDS, DDL, DDLS, CDS view, @AbapCatalog, @AccessControl, association, projection, consumption, GROUP BY, aggregation, subquery, JOIN

## TOPICS IN THIS FILE
1. File Naming - line 96
2. DDL Source (.ddls.asddls) - line 107
3. Annotations - line 141
4. Associations - line 164
5. CDS Best Practices - line 194
   - Key Field Ordering (STRICT RULE) - line 198
   - Currency/Amount Field Aggregation - line 230
   - Choosing Currency Fields for Aggregation - line 255
6. CDS Test Doubles - see testing.md

## Creating CDS Views (DDLS)

CDS views (Data Definition Language Source) require specific file naming and structure for abapGit.

### CDS View vs View Entity: When to Use Which

**IMPORTANT**: When creating CDS views, use **View Entity** by default unless explicitly requested otherwise.

| User Request | Create Type | Why |
|--------------|-------------|-----|
| "Create CDS view" | CDS View Entity (modern) | Default for new development |
| "Create CDS view for..." | CDS View Entity (modern) | Recommended approach |
| "Create legacy CDS view" | CDS View (legacy) | Only if explicitly requested |
| "Create CDS view with sqlViewName" | CDS View (legacy) | Explicit legacy request |

### Key Differences

| Aspect | CDS View (Legacy) | CDS View Entity (Modern) |
|--------|-------------------|-------------------------|
| **Syntax** | `define view` | `define view entity` |
| **@AbapCatalog.sqlViewName** | ✅ Required | ❌ Not allowed (will fail) |
| **Creates SQL View** | Yes (DDLS + SQL view) | No (DDLS only) |
| **XML SOURCE_TYPE** | `V` | `W` |
| **ABAP Version** | 7.40+ | 7.55+ / S/4HANA Cloud |
| **Parameter Syntax** | `:param` or `$parameters.param` | `$parameters.param` only |
| **Use For** | Legacy systems, existing code | New development, S/4HANA |

### XML Metadata: The Key Difference

The XML metadata differs only in the `SOURCE_TYPE` field:

**CDS View Entity XML (RECOMMENDED - use by default):**
```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DDLS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DDLS>
    <DDLNAME>ZC_MY_ENTITY</DDLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>My CDS View Entity</DDTEXT>
    <SOURCE_TYPE>W</SOURCE_TYPE>
   </DDLS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**CDS View XML (Legacy - only if explicitly requested):**
```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DDLS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DDLS>
    <DDLNAME>ZC_MY_VIEW</DDLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>My CDS View</DDTEXT>
    <SOURCE_TYPE>V</SOURCE_TYPE>
   </DDLS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**SOURCE_TYPE values:**
- `W` = View Entity (modern, no SQL view created)
- `V` = View (legacy, creates SQL view)

---

## Creating CDS Views (DDLS)

CDS views (Data Definition Language Source) require specific file naming and structure for abapGit.

### File Naming

CDS views require **two files**:

| File | Description |
|------|-------------|
| `zc_my_view.ddls.asddls` | DDL source code |
| `zc_my_view.ddls.xml` | XML metadata |

**Important:** Do NOT use `.ddls.abap` extension - use `.ddls.asddls` for the source.

### DDL Source File (`.ddls.asddls`)

**CDS View Entity (RECOMMENDED - use by default):**
```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'My CDS View Entity'
define view entity ZC_My_Entity as select from tdevc
{
  key devclass as Devclass,
      parentcl as ParentPackage,
      ctext    as Description
}
where devclass not like '$%'
```

**CDS View (Legacy - only if explicitly requested):**
```abap
@AbapCatalog.sqlViewName: 'ZCMYVIEW'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'My CDS View'
define view ZC_My_View as select from tdevc
{
  key devclass as Devclass,
      parentcl as ParentPackage,
      ctext    as Description
}
where devclass not like '$%'
```

**Note the key differences:**
- View Entity: No `@AbapCatalog.sqlViewName`, uses `define view entity`
- View (legacy): Has `@AbapCatalog.sqlViewName`, uses `define view`

### Key Points

1. **Avoid reserved words** - Field names like `PACKAGE`, `CLASS`, `INTERFACE` are reserved in CDS. Use alternatives like `PackageName`, `ClassName`.

2. **Workflow for creating CDS views** — run: `abapgit-agent ref --topic workflow-detailed`

3. **System support** - CDS views require SAP systems with CDS capability (S/4HANA, SAP BW/4HANA, or ABAP 7.51+). Older systems will show error: "Object type DDLS is not supported by this system"

### Activating CDS Views

→ See `abapgit-agent ref --topic workflow-detailed` for the complete workflow.

## CDS View Entity Features

CDS View Entities are the modern replacement for CDS Views with enhanced features like **associations for OData navigation**.

### Associations in View Entities

```abap
@EndUserText.label: 'Package Hierarchy'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZC_Pkg_Hierarchy_VE
  as select from tdevc
  association [0..1] to tdevc as _Parent
    on _Parent.devclass = $projection.ParentPackage
{
  key devclass                           as PackageName,
      parentcl                           as ParentPackage,
      ctext                              as Description,
      dlvunit                           as SoftwareComponent,

      // Exposed associations
      _Parent
}
where devclass not like '$%'
```

### Key Points for View Entities

1. **Association syntax**: Use `$projection` to reference fields in the current entity
2. **Association cardinality**: `[0..1]`, `[1..1]`, `[0..n]`, `[1..n]`
3. **Expose associations**: Add the association name at the end of the SELECT to expose it for OData navigation
4. **Activation warnings**: Search help warnings are informational and don't block activation

---

## CDS Best Practices and Common Patterns

### No Inline Subqueries — Use JOIN + GROUP BY Instead

**CDS view entities do NOT support inline subqueries in JOIN.** This is a hard syntax error:

❌ **WRONG — inline subquery (syntax error)**:
```abap
define view entity ZC_MyView as select from zheader as Header
  inner join (                          // ← NOT SUPPORTED
    select docid, sum( amount ) as Total
    from zitems
    group by docid
  ) as Items on Items.docid = Header.docid ...
```

✅ **CORRECT — JOIN base tables directly, use GROUP BY on the outer view**:
```abap
define view entity ZC_MyView
  as select from zheader as Header
    inner join zitems as Item
      on Item.docid = Header.docid
{
  key Header.docid       as DocId,
      Header.description as Description,
      count(*)           as NumberOfItems,
      sum(Item.amount)   as TotalAmount
}
group by Header.docid, Header.description
```

**This works.** CDS supports JOIN + GROUP BY + aggregation functions in a single view. Only inline subqueries in the FROM/JOIN clause are unsupported.

---

### Key Field Ordering (STRICT RULE)

CDS views enforce strict key field ordering that differs from regular SQL:

❌ **WRONG - Non-key field between keys**:
```abap
{
  key Flight.carrid as Carrid,
      Airline.carrname as Carrname,  // ← breaks contiguity!
  key Flight.connid as Connid,
  key Flight.fldate as Fldate,
  ...
}
```

✅ **CORRECT - All keys first, then non-keys**:
```abap
{
  key Flight.carrid as Carrid,
  key Flight.connid as Connid,
  key Flight.fldate as Fldate,
      Airline.carrname as Carrname,  // ← after all keys
  ...
}
```

**Rules:**
1. All key fields MUST be at the beginning of the field list
2. Key fields MUST be contiguous (no non-key fields in between)
3. Key fields must be declared before any non-key fields

**Error message**: "Key must be contiguous and start at the first position"

**Why this rule exists**: CDS views are not just SQL - they represent data models with strict structural requirements for consistency across the system.

---

### Currency/Amount Field Aggregation

> **Design principle**: Prefer a **single CDS view** with JOIN + GROUP BY. Only split into multiple layered views when requirements specifically need reusable intermediate aggregations shared across different consumers.

When aggregating currency or amount fields in CDS views, use semantic annotations instead of complex casting:

❌ **WRONG - Complex casting (will fail)**:
```abap
cast(coalesce(sum(Booking.loccuram), 0) as abap.curr(15,2))
// Error: Data type CURR is not supported at this position
```

✅ **CORRECT - Semantic annotation + simple aggregation**:
```abap
@Semantics.amount.currencyCode: 'Currency'
sum(Booking.loccuram) as TotalRevenue,
currency             as Currency
```

**Key points:**
- Use `@Semantics.amount.currencyCode` to link amount to currency field
- Let the framework handle data typing automatically
- Don't over-engineer with casts or type conversions
- Keep it simple: annotation + aggregation function

**Reference**:
```bash
abapgit-agent ref "CDS aggregation"
# Check: zdemo_abap_cds_ve_agg_exp.ddls.asddls for working examples
```

---

### Choosing Currency Fields for Aggregation

**Understand your data model before aggregating currency fields** - not all currency fields can be safely summed:

❌ **Dangerous - Foreign currency (different keys per row)**:
```abap
sum(Booking.forcuram)  // FORCURAM has different FORCURKEY per booking!
// Problem: Can't safely sum USD + EUR + GBP without conversion
```

✅ **Safe - Local currency (shared key per group)**:
```abap
sum(Booking.loccuram)  // LOCCURAM shares LOCCURKEY per airline
// Safe: All bookings for one airline use the same currency
```

**Data Model Example (SBOOK table)**:
```
FORCURAM + FORCURKEY = Payment currency (USD, EUR, GBP - different per booking)
LOCCURAM + LOCCURKEY = Airline currency (one currency per airline)
```

**Analysis Steps:**
1. Identify all currency fields in source tables
2. Check which currency code field each amount uses
3. Verify currency code is constant within your aggregation groups
4. Choose the field with consistent currency per group

**Rule**: Only aggregate amounts that share the same currency code within your grouping (GROUP BY).

---

## CDS Syntax Reference

When working with CDS view syntax (arithmetic, built-in functions, aggregations, etc.):

1. Run `abapgit-agent ref --topic cds` to see available topics and example files
2. Check the example files in `abap-cheat-sheets/src/`:
   - `zdemo_abap_cds_ve_sel.ddls.asddls` - Arithmetic expressions, built-in functions (division, cast, etc.)
   - `zdemo_abap_cds_ve_agg_exp.ddls.asddls` - Aggregate expressions (SUM, AVG, COUNT)
   - `zdemo_abap_cds_ve_assoc.ddls.asddls` - Associations

**Note**: This requires `abap-cheat-sheets` to be in the reference folder (configured in `.abapGitAgent`).

---

## Selecting from CDS Views in Classes

### Best Practice: Use CDS View Entity as Type

```abap
" ✅ RECOMMENDED - Use view entity directly
TYPES ty_results TYPE STANDARD TABLE OF zc_my_view WITH DEFAULT KEY.

METHOD get_data.
  SELECT * FROM zc_my_view INTO TABLE @rt_results.
ENDMETHOD.
```

**Benefits**: No field mismatches, 33% less code, auto-sync with CDS changes.

---

### Alternative: Manual Structure Definition

Only when you need to hide/transform fields:

```abap
" Use data elements from underlying tables
TYPES: BEGIN OF ty_custom,
         carrierid TYPE s_carr_id,     " ✅ Data element
         connid    TYPE s_conn_id,     " ✅ NOT: TYPE c LENGTH 3
       END OF ty_custom.
```

**Find data elements**:
```bash
abapgit-agent view --objects SFLIGHT --type TABL
```

**Match field names**:
```bash
abapgit-agent preview --objects ZC_MY_VIEW --limit 1
```

**Calculated fields**: Use `TYPE decfloat34` for division/complex math.

---

## See Also
- **Unit Testing** (testing.md) - for CDS Test Double Framework
- **abapGit** (abapgit.md) - for CDS XML metadata templates
- **ABAP SQL** (sql.md) - for SQL functions used in CDS
