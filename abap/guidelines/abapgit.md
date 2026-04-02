---
layout: default
title: abapGit XML Metadata
nav_order: 9
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# abapGit XML Metadata — CLAS, INTF, PROG

This guide covers objects that have **both a source file and an XML metadata file**.
For objects with no source file (XML only), see `abapgit-agent ref --topic abapgit-xml-only`.

## QUICK REFERENCE

```
File Type          | ABAP File                    | XML File
-------------------|------------------------------|-------------------
Class              | zcl_*.clas.abap              | zcl_*.clas.xml
Test Class         | zcl_*.clas.testclasses.abap  | zcl_*.clas.xml
Interface          | zif_*.intf.abap              | zif_*.intf.xml
Program            | z*.prog.abap                 | z*.prog.xml
CDS View (DDLS)    | zc_*.ddls.asddls             | zc_*.ddls.xml
CDS Access Control | zc_*.dcls.asdcls             | zc_*.dcls.xml
Function Group     | z*fugr*.abap (6 files)       | z*.fugr.xml
```

> **XML-only objects** (TABL, STRU, DTEL, TTYP, DOMA, MSAG) have **no ABAP source file** and are not covered here.
> See `abapgit-agent ref --topic abapgit-xml-only`.

> **CRITICAL: Always write XML files with a UTF-8 BOM (`\ufeff`) as the very first character**, before `<?xml ...`.
> Without the BOM, abapGit shows the object as **"M" (modified)** after every pull because the
> serializer always produces XML with BOM — and every byte matters.

> **CRITICAL: Only include fields that abapGit's serializer actually writes. Never add fields with
> default values.** Extra fields cause a permanent "M" (modified) diff. Follow the exact templates below.

**Searchable keywords**: class xml, interface xml, program xml, cds view xml, ddls, dcls, access control xml, function group xml, fugr, test class, exposure, serializer, abapgit

## Why XML Metadata?

abapGit needs XML files to:
- Know the object type and serializer to use
- Store object attributes (description, exposure, state, etc.)
- Handle object-specific configurations

## Field Presence Rules (CRITICAL)

abapGit's serializer **omits fields that have their default value**. Writing extra fields causes permanent
"M" (modified) status in abapGit UI. Follow these rules strictly:

### CLAS — Field Presence Rules

| Field | Include when | Omit when |
|---|---|---|
| `CLSNAME` | Always | — |
| `LANGU` | Always | — |
| `DESCRIPT` | Always | — |
| `CATEGORY` | Non-standard class (`40`=exception, `05`=test double) | Standard class (default `00`) — **omit** |
| `EXPOSURE` | **Never for CLAS** | **Always omit** — public (`2`) is the default and is never written |
| `STATE` | Always (`1`) | — |
| `CLSCCINCL` | `.clas.locals_def.abap` file exists | No local class files — **omit** |
| `FIXPT` | Always (`X`) | — |
| `UNICODE` | Always (`X`) | — |
| `WITH_UNIT_TESTS` | `.clas.testclasses.abap` file exists | No test class file — **omit** |
| `MSG_ID` | Class has a message class | No message class — **omit** |

**Field order**: `CLSNAME → LANGU → DESCRIPT → [CATEGORY] → STATE → [CLSCCINCL] → FIXPT → UNICODE → [WITH_UNIT_TESTS] → [MSG_ID]`

### INTF — Field Presence Rules

| Field | Include when | Omit when |
|---|---|---|
| `CLSNAME` | Always | — |
| `LANGU` | Always | — |
| `DESCRIPT` | Always | — |
| `EXPOSURE` | Always (`2`) | — (interfaces always have EXPOSURE, unlike classes) |
| `STATE` | Always (`1`) | — |
| `UNICODE` | Always (`X`) | — |

## Object Types and XML Templates

### Class (CLAS)

**Filename**: `src/zcl_my_class.clas.xml`

**Standard public class** (no local includes, no test class):

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_MY_CLASS</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description of the class</DESCRIPT>
    <STATE>1</STATE>
    <FIXPT>X</FIXPT>
    <UNICODE>X</UNICODE>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Class with test class** (`.clas.testclasses.abap` exists — add `WITH_UNIT_TESTS`):

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_MY_CLASS</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description of the class</DESCRIPT>
    <STATE>1</STATE>
    <FIXPT>X</FIXPT>
    <UNICODE>X</UNICODE>
    <WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Class with local includes** (`.clas.locals_def.abap` exists — add `CLSCCINCL` before `FIXPT`):

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_MY_CLASS</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description of the class</DESCRIPT>
    <STATE>1</STATE>
    <CLSCCINCL>X</CLSCCINCL>
    <FIXPT>X</FIXPT>
    <UNICODE>X</UNICODE>
    <WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Exception class** (`CATEGORY>40` — add `CATEGORY` after `DESCRIPT`):

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCX_MY_EXCEPTION</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>My exception</DESCRIPT>
    <CATEGORY>40</CATEGORY>
    <STATE>1</STATE>
    <CLSCCINCL>X</CLSCCINCL>
    <FIXPT>X</FIXPT>
    <UNICODE>X</UNICODE>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key rules**:
- ❌ **Never include `<EXPOSURE>`** in a CLAS XML — public (2) is the default and abapGit omits it
- ✅ Always include `<FIXPT>X</FIXPT>` and `<UNICODE>X</UNICODE>`
- ✅ Add `<WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>` only when `.clas.testclasses.abap` exists
- ✅ Add `<CLSCCINCL>X</CLSCCINCL>` only when `.clas.locals_def.abap` exists

**Local Class Files**:
| File | Purpose |
|------|---------|
| `zcl_xxx.clas.locals_def.abap` | Local class definitions |
| `zcl_xxx.clas.locals_imp.abap` | Local class implementations |

---

### Interface (INTF)

**Filename**: `src/zif_my_interface.intf.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_INTF" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOINTERF>
    <CLSNAME>ZIF_MY_INTERFACE</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description of the interface</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOINTERF>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key rules**:
- ✅ `<EXPOSURE>2</EXPOSURE>` is **always present** for interfaces (unlike classes where it is omitted)

---

### Program (PROG)

**Filename**: `src/zmy_program.prog.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_PROG" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>ZMY_PROGRAM</NAME>
    <SUBC>1</SUBC>
    <RLOAD>E</RLOAD>
    <FIXPT>X</FIXPT>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `NAME`: Program name
- `SUBC`: Program type (`1`=Executable, `I`=Include, `F`=Function Group, `M`=Module Pool, `S`=Subroutine Pool)
- `RLOAD`: `E`=External
- `FIXPT`: Fixed-point arithmetic (`X`=Yes) — include for executables
- `UCCHECK`: Unicode checks active (`X`=Yes)

**Note**: The serializer may also write a `<TPOOL>` section after `<PROGDIR>` if the program has a title text. You do not need to write this when creating new programs — abapGit will add it on the next pull if needed.

---

### CDS View / View Entity (DDLS)

**Files**: `src/zc_my_view.ddls.asddls` (source) + `src/zc_my_view.ddls.xml` (metadata)

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DDLS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DDLS>
    <DDLNAME>ZC_MY_VIEW</DDLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>My CDS View</DDTEXT>
    <SOURCE_TYPE>W</SOURCE_TYPE>
   </DDLS>
  </asx:values>
 </asx:abap>
</abapGit>
```

- `SOURCE_TYPE W` → View Entity (`define view entity`, modern — **use by default**)
- `SOURCE_TYPE V` → View (`define view` + `@AbapCatalog.sqlViewName`, legacy)

→ For DDL source syntax: `abapgit-agent ref --topic cds-abapgit`

---

### CDS Access Control (DCLS)

**Files**: `src/zc_my_view.dcls.asdcls` (source) + `src/zc_my_view.dcls.xml` (metadata)

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DCLS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DCLS>
    <DCLNAME>ZC_MY_VIEW</DCLNAME>
    <DCLLANGUAGE>E</DCLLANGUAGE>
    <DDTEXT>Access control for ZC_MY_VIEW</DDTEXT>
   </DCLS>
  </asx:values>
 </asx:abap>
</abapGit>
```

- `DCLNAME` matches the CDS view name
- Source file contains the DCL (`@MappingRole ... define role ...`)

---

### Function Group (FUGR)

FUGR serializes as multiple files (main XML + TOP include + SAPL include + one source file per FM). The format is straightforward for screenless function groups — Claude can generate it from scratch.

→ Full template and file-set rules: `abapgit-agent ref --topic abapgit-fugr`

---

## Important Notes

1. **ALWAYS push to git BEFORE running pull** - abapGit reads from git
2. **Check pull output** to verify objects were recognized by abapGit
3. **Use inspect AFTER pull** to check syntax on objects in ABAP
