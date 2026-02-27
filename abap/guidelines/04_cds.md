---
layout: default
title: CDS Views
nav_order: 5
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Creating CDS Views

**Searchable keywords**: CDS, DDL, DDLS, CDS view, @AbapCatalog, @AccessControl, association, projection, consumption

## TOPICS IN THIS FILE
1. File Naming - line 7
2. DDL Source (.ddls.asddls) - line 18
3. Annotations - line 50
4. Associations - line 75
5. CDS Test Doubles - see 03_testing.md

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

2. **Workflow for creating CDS views** - See `../CLAUDE.md` for complete workflow guidance:
   - Independent CDS views: `syntax → commit → pull --files`
   - Dependent CDS views (with associations to NEW views): Create underlying view first, then dependent view
   - See CLAUDE.md section on "Working with dependent objects"

3. **System support** - CDS views require SAP systems with CDS capability (S/4HANA, SAP BW/4HANA, or ABAP 7.51+). Older systems will show error: "Object type DDLS is not supported by this system"

### Activating CDS Views

**For standard workflow, see `../CLAUDE.md`**

**CDS-specific notes:**
- Single independent DDLS file: `abapgit-agent pull --files src/zc_view.ddls.asddls`
- CDS views with associations to OTHER NEW views: Create target view first (see `../CLAUDE.md` for workflow)

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
- **Unit Testing** (03_testing.md) - for CDS Test Double Framework
- **abapGit** (08_abapgit.md) - for CDS XML metadata templates
- **ABAP SQL** (01_sql.md) - for SQL functions used in CDS
