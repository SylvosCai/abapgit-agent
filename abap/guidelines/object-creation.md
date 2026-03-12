---
layout: default
title: Object Creation
nav_order: 16
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Object Creation

### Create XML Metadata for Each ABAP Object

**Each ABAP object requires an XML metadata file for abapGit to understand how to handle it.**

| Object Type | ABAP File (if folder=/src/) | XML File | Details |
|-------------|------------------------------|----------|---------|
| Class | `src/zcl_*.clas.abap` | `src/zcl_*.clas.xml` | See `guidelines/abapgit.md` |
| Interface | `src/zif_*.intf.abap` | `src/zif_*.intf.xml` | See `guidelines/abapgit.md` |
| Program | `src/z*.prog.abap` | `src/z*.prog.xml` | See `guidelines/abapgit.md` |
| Table | `src/z*.tabl.abap` | `src/z*.tabl.xml` | See `guidelines/abapgit.md` |
| **CDS View Entity** | `src/zc_*.ddls.asddls` | `src/zc_*.ddls.xml` | **Use by default** - See `guidelines/cds.md` |
| CDS View (legacy) | `src/zc_*.ddls.asddls` | `src/zc_*.ddls.xml` | Only if explicitly requested - See `guidelines/cds.md` |

**IMPORTANT: When user says "create CDS view", create CDS View Entity by default.**

**Why:** Modern S/4HANA standard, simpler (no SQL view), no namespace conflicts.

**For complete XML templates, DDL examples, and detailed comparison:**
- **CDS Views**: `guidelines/cds.md`
- **XML templates**: `guidelines/abapgit.md`

---

### Local Classes (Test Doubles, Helpers)

When a class needs local helper classes or test doubles, use separate files:

| File | Purpose |
|------|---------|
| `zcl_xxx.clas.locals_def.abap` | Local class definitions |
| `zcl_xxx.clas.locals_imp.abap` | Local class implementations |

**XML Configuration**: Add `<CLSCCINCL>X</CLSCCINCL>` to the class XML to include local class definitions:

```xml
<VSEOCLASS>
  <CLSNAME>ZCL_XXX</CLSNAME>
  ...
  <CLSCCINCL>X</CLSCCINCL>
</VSEOCLASS>
```
