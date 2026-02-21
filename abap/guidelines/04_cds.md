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

### File Naming

CDS views require **two files**:

| File | Description |
|------|-------------|
| `zc_my_view.ddls.asddls` | DDL source code |
| `zc_my_view.ddls.xml` | XML metadata |

**Important:** Do NOT use `.ddls.abap` extension - use `.ddls.asddls` for the source.

### DDL Source File (`.ddls.asddls`)

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

### XML Metadata File (`.ddls.xml`)

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DDLS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DDLS>
    <DDLNAME>ZC_MY_VIEW</DDLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>My CDS View</DDTEXT>
   </DDLS>
  </asx:values>
 </asx:abap>
</abapGit>
```

### Key Points

1. **Avoid reserved words** - Field names like `PACKAGE`, `CLASS`, `INTERFACE` are reserved in CDS. Use alternatives like `PackageName`, `ClassName`.

2. **Pull all files to activate** - When activating CDS views, use `abapgit-agent pull` (not single file) to ensure proper activation:
   ```bash
   abapgit-agent pull  # Pull all files
   ```

3. **System support** - CDS views require SAP systems with CDS capability (S/4HANA, SAP BW/4HANA, or ABAP 7.51+). Older systems will show error: "Object type DDLS is not supported by this system"

### Debugging Activation Errors

When pull shows generic errors like "Activation cancelled. Check the inactive objects":

1. **Check in ADT/Eclipse** - Open the DDL source in ADT and run syntax check for detailed errors
2. **Pull all files** - Sometimes `abapgit-agent pull` (all files) works better than single file for CDS views

## Creating CDS View Entities

CDS View Entities (`define view entity`) are the modern replacement for CDS Views with additional features like **associations for OData navigation**.

### Differences from CDS Views

| Aspect | CDS View | View Entity |
|--------|----------|-------------|
| Syntax | `define view` | `define view entity` |
| Associations | No | Yes (exposed for navigation) |
| OData/Navigation | Requires separate service | Auto-exposes associations |
| ABAP Version | 7.40+ | 7.55+ / S/4HANA Cloud |

### DDL Source File with Association

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

## See Also
- **Unit Testing** (03_testing.md) - for CDS Test Double Framework
- **abapGit** (08_abapgit.md) - for CDS XML metadata templates
- **ABAP SQL** (01_sql.md) - for SQL functions used in CDS
