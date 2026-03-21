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

Replace `<name>` with the actual object name from this project's naming conventions
(`guidelines/objects.local.md`, or `guidelines/objects.md` as fallback).

| Object Type | ABAP Source File | XML File | Details |
|-------------|-----------------|----------|---------|
| Class | `<name>.clas.abap` | `<name>.clas.xml` | See `guidelines/abapgit.md` |
| Interface | `<name>.intf.abap` | `<name>.intf.xml` | See `guidelines/abapgit.md` |
| Program | `<name>.prog.abap` | `<name>.prog.xml` | See `guidelines/abapgit.md` |
| **CDS View Entity** | `<name>.ddls.asddls` | `<name>.ddls.xml` | **Use by default** - See `guidelines/cds.md` |
| CDS View (legacy) | `<name>.ddls.asddls` | `<name>.ddls.xml` | Only if explicitly requested - See `guidelines/cds.md` |
| Table (TABL) | *(none — XML-only)* | `<name>.tabl.xml` | See `guidelines/abapgit.md` |
| Structure (STRU) | *(none — XML-only)* | `<name>.stru.xml` | See `guidelines/abapgit.md` |
| Data Element (DTEL) | *(none — XML-only)* | `<name>.dtel.xml` | See `guidelines/abapgit.md` |
| Table Type (TTYP) | *(none — XML-only)* | `<name>.ttyp.xml` | See `guidelines/abapgit.md` |

> **XML-only objects (TABL, STRU, DTEL, TTYP)**: create only the `.xml` file — there is no `.abap` source file.
> After committing and pushing, pull with: `pull --files <folder>/<name>.tabl.xml --sync-xml`

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
| `<name>.clas.locals_def.abap` | Local class definitions |
| `<name>.clas.locals_imp.abap` | Local class implementations |

**XML Configuration**: Add `<CLSCCINCL>X</CLSCCINCL>` to the class XML to include local class definitions:

```xml
<VSEOCLASS>
  <CLSNAME>MY_CLASS_NAME</CLSNAME>
  ...
  <CLSCCINCL>X</CLSCCINCL>
</VSEOCLASS>
```
