---
layout: default
title: Object Creation
nav_order: 22
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
| Class (CLAS) | `<name>.clas.abap` | `<name>.clas.xml` | `ref --topic abapgit` |
| Interface (INTF) | `<name>.intf.abap` | `<name>.intf.xml` | `ref --topic abapgit` |
| Program (PROG) | `<name>.prog.abap` | `<name>.prog.xml` | `ref --topic abapgit` |
| CDS View Entity (DDLS) | `<name>.ddls.asddls` | `<name>.ddls.xml` | **Use by default** — `ref --topic abapgit` |
| CDS Access Control (DCLS) | `<name>.dcls.asdcls` | `<name>.dcls.xml` | `ref --topic abapgit` |
| Function Group (FUGR) | `<name>.fugr.abap` + includes | `<name>.fugr.xml` | `ref --topic abapgit` |
| Table (TABL) | *(none — XML-only)* | `<name>.tabl.xml` | `ref --topic abapgit-xml-only` |
| Structure (STRU) | *(none — XML-only)* | `<name>.tabl.xml` ⚠️ NOT `.stru.xml` | `ref --topic abapgit-xml-only` |
| Data Element (DTEL) | *(none — XML-only)* | `<name>.dtel.xml` | `ref --topic abapgit-xml-only` |
| Table Type (TTYP) | *(none — XML-only)* | `<name>.ttyp.xml` | `ref --topic abapgit-xml-only` |
| Domain (DOMA) | *(none — XML-only)* | `<name>.doma.xml` | `ref --topic abapgit-xml-only` |
| Message Class (MSAG) | *(none — XML-only)* | `<name>.msag.xml` | `ref --topic abapgit-xml-only` |

> **XML-only objects (TABL, STRU, DTEL, TTYP, DOMA, MSAG)**: create only the `.xml` file — there is no `.abap` source file.
> After committing and pushing, pull with: `pull --files <folder>/<name>.tabl.xml --sync-xml`

**IMPORTANT: When user says "create CDS view", create CDS View Entity (DDLS) by default.**

**Why:** Modern S/4HANA standard, simpler (no SQL view), no namespace conflicts.

**For complete XML templates, DDL examples, and detailed comparison:**
- **CDS Views + DCLS + FUGR**: `abapgit-agent ref --topic abapgit`
- **XML-only objects**: `abapgit-agent ref --topic abapgit-xml-only`

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
