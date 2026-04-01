---
layout: default
title: abapGit XML Metadata
nav_order: 9
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# abapGit Object XML Metadata

Each ABAP object requires an XML metadata file for abapGit to understand how to serialize/deserialize it. This guide provides templates for common object types.

## QUICK REFERENCE

```
File Type          | ABAP File                    | XML File
-------------------|------------------------------|-------------------
Class              | zcl_*.clas.abap              | zcl_*.clas.xml
Test Class         | zcl_*.clas.testclasses.abap | zcl_*.clas.xml
Interface          | zif_*.intf.abap             | zif_*.intf.xml
Program            | z*.prog.abap                | z*.prog.xml
CDS View (DDLS)    | zc_*.ddls.asddls            | zc_*.ddls.xml
Table (TABL)       | (no .abap file)             | z*.tabl.xml
Structure (STRU)   | (no .abap file)             | z*.stru.xml
Data Element (DTEL)| (no .abap file)             | z*.dtel.xml
Table Type (TTYP)  | (no .abap file)             | z*.ttyp.xml
```

> **XML-only objects (TABL, STRU, DTEL, TTYP)** have **no ABAP source file**. abapGit serializes
> them as XML metadata only. When using `--files`, pass the `.xml` file directly:
> `abapgit-agent pull --files abap/zmy_table.tabl.xml --sync-xml`

> **CRITICAL: Always write XML files with a UTF-8 BOM (`\ufeff`) as the very first character**, before `<?xml ...`.
> Without the BOM, abapGit shows the object as **"M" (modified)** after every pull because the
> serializer always produces XML with BOM — and every byte matters.

> **CRITICAL: Only include fields that abapGit's serializer actually writes. Never add fields with
> default values.** Extra fields cause a permanent "M" (modified) diff. Follow the exact templates below.

**Searchable keywords**: class xml, interface xml, table xml, cds xml, test class, exposure, serializer, abapgit

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

### Table (TABL)

**Filename**: `src/zmy_table.tabl.xml`

> **XML-only object** — no `.abap` source file. Pull with: `pull --files src/zmy_table.tabl.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZMY_TABLE</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <DDTEXT>Description of the table</DDTEXT>
    <CONTFLAG>A</CONTFLAG>
   </DD02V>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `TABNAME`: Table name
- `DDTEXT`: Description (**not** `DESCRIPT`)
- `TABCLASS`: `TRANSP`=Transparent (most common), `POOL`, `CLUSTER`
- `CONTFLAG`: Delivery class — `A`=Application, `C`=Customizing, `S`=System, `G`=Customizing protected

**Note**: When abapGit serializes an existing table it also writes `<DD09L>` (technical settings) and `<DD03P_TABLE>` (field definitions). These sections are generated automatically from the ABAP Dictionary on pull — you only need the `<DD02V>` header when creating a new table. After the first pull the XML will be expanded with those sections.

**`DD03P` field-level rules** (apply when editing an existing table XML that includes `<DD03P_TABLE>`):

| Rule | Detail |
|---|---|
| `SHLPORIGIN` | Include `<SHLPORIGIN>D</SHLPORIGIN>` on fields where the Dictionary provides a value help (e.g. fields with a domain that has fixed values or a search help). Omit on fields with no value help. |
| Field order for raw-type fields | For fields with no `ROLLNAME` (raw type, e.g. `CHAR`, `NUMC`), the serializer writes `<MASK>` **before** `<DDTEXT>`. For fields with a `ROLLNAME`, only `ROLLNAME` appears (no `MASK` or `DDTEXT`). |

**Why this matters**: Missing `SHLPORIGIN` or wrong `MASK`/`DDTEXT` order causes a permanent diff between git and the system-serialized XML.

#### Text Tables and Foreign Keys

A **text table** stores translatable texts for another table (the "main table"). It shares the same key fields as the main table plus a language field (`SPRAS` with data element `SPRAS`, or `LANGU` with data element `LANGU`).

**How to recognise a text table relationship in the XML:**
- `<DD09L>` has `<UEBERSETZ>X</UEBERSETZ>` — marks this table as a text table
- `<DD08V>` has `<FRKART>TEXT</FRKART>` — marks the foreign key as a text-table relationship (ordinary foreign keys omit this field)

**Three sections required in the text table XML:**

| Section | Purpose |
|---|---|
| `DD03P_TABLE` | Field definitions — same key fields as main table + language field + text fields |
| `DD05M_TABLE` | Foreign key field mappings — one entry per key field of the main table (excluding the language field) |
| `DD08V_TABLE` | Foreign key relationship — one entry with `FRKART>TEXT` |

**`DD03P` rules for the text table:**
- First key field (`MANDT`) must have `<CHECKTABLE>` pointing to the main table and `<SHLPORIGIN>P</SHLPORIGIN>`
- Language field (`SPRAS` or `LANGU`) gets `<SHLPORIGIN>D</SHLPORIGIN>` (added by serializer after activation — omit when writing manually)

**`DD05M` rules:**
- `FIELDNAME` = the anchor field in the text table (typically `MANDT` — the first key field)
- `FORTABLE` = text table name
- `FORKEY` = key field name in the main table
- `CHECKFIELD` = same as `FORKEY`
- `CHECKTABLE` = main table name
- `PRIMPOS` = position sequence (0001, 0002, …)
- `DOMNAME` / `DATATYPE` = domain and type of the check field (omit `DOMNAME` if unknown — serializer fills it in)

**`DD08V` rules:**
- `FIELDNAME` = same anchor field as used in `DD05M` (typically `MANDT`)
- `CHECKTABLE` = main table name
- `FRKART` = `TEXT` (text table) or omit (ordinary foreign key)
- `CARD` = `CN` (n:1 cardinality)
- `CARDLEFT` = `1`

**Example — text table `ZMY_TABLE_T` for main table `ZMY_TABLE` (keys: MANDT, ID1, ID2):**

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZMY_TABLE_T</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <CLIDEP>X</CLIDEP>
    <DDTEXT>My Table: Texts</DDTEXT>
    <CONTFLAG>C</CONTFLAG>
   </DD02V>
   <DD09L>
    <TABNAME>ZMY_TABLE_T</TABNAME>
    <AS4LOCAL>A</AS4LOCAL>
    <TABKAT>0</TABKAT>
    <TABART>APPL0</TABART>
    <UEBERSETZ>X</UEBERSETZ>
    <BUFALLOW>N</BUFALLOW>
   </DD09L>
   <DD03P_TABLE>
    <DD03P>
     <FIELDNAME>MANDT</FIELDNAME>
     <KEYFLAG>X</KEYFLAG>
     <ROLLNAME>MANDT</ROLLNAME>
     <CHECKTABLE>ZMY_TABLE</CHECKTABLE>
     <ADMINFIELD>0</ADMINFIELD>
     <NOTNULL>X</NOTNULL>
     <SHLPORIGIN>P</SHLPORIGIN>
     <COMPTYPE>E</COMPTYPE>
    </DD03P>
    <DD03P>
     <FIELDNAME>ID1</FIELDNAME>
     <KEYFLAG>X</KEYFLAG>
     <ROLLNAME>ZMY_ID1</ROLLNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <NOTNULL>X</NOTNULL>
     <COMPTYPE>E</COMPTYPE>
    </DD03P>
    <DD03P>
     <FIELDNAME>ID2</FIELDNAME>
     <KEYFLAG>X</KEYFLAG>
     <ROLLNAME>ZMY_ID2</ROLLNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <NOTNULL>X</NOTNULL>
     <COMPTYPE>E</COMPTYPE>
    </DD03P>
    <DD03P>
     <FIELDNAME>SPRAS</FIELDNAME>
     <KEYFLAG>X</KEYFLAG>
     <ROLLNAME>SPRAS</ROLLNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <NOTNULL>X</NOTNULL>
     <COMPTYPE>E</COMPTYPE>
    </DD03P>
    <DD03P>
     <FIELDNAME>DESCRIPTION</FIELDNAME>
     <ROLLNAME>ZMY_DESCRIPTION</ROLLNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <COMPTYPE>E</COMPTYPE>
    </DD03P>
   </DD03P_TABLE>
   <DD05M_TABLE>
    <DD05M>
     <FIELDNAME>MANDT</FIELDNAME>
     <FORTABLE>ZMY_TABLE_T</FORTABLE>
     <FORKEY>MANDT</FORKEY>
     <CHECKTABLE>ZMY_TABLE</CHECKTABLE>
     <CHECKFIELD>MANDT</CHECKFIELD>
     <PRIMPOS>0001</PRIMPOS>
     <DOMNAME>MANDT</DOMNAME>
     <DATATYPE>CLNT</DATATYPE>
    </DD05M>
    <DD05M>
     <FIELDNAME>MANDT</FIELDNAME>
     <FORTABLE>ZMY_TABLE_T</FORTABLE>
     <FORKEY>ID1</FORKEY>
     <CHECKTABLE>ZMY_TABLE</CHECKTABLE>
     <CHECKFIELD>ID1</CHECKFIELD>
     <PRIMPOS>0002</PRIMPOS>
     <DATATYPE>CHAR</DATATYPE>
    </DD05M>
    <DD05M>
     <FIELDNAME>MANDT</FIELDNAME>
     <FORTABLE>ZMY_TABLE_T</FORTABLE>
     <FORKEY>ID2</FORKEY>
     <CHECKTABLE>ZMY_TABLE</CHECKTABLE>
     <CHECKFIELD>ID2</CHECKFIELD>
     <PRIMPOS>0003</PRIMPOS>
     <DATATYPE>CHAR</DATATYPE>
    </DD05M>
   </DD05M_TABLE>
   <DD08V_TABLE>
    <DD08V>
     <FIELDNAME>MANDT</FIELDNAME>
     <CHECKTABLE>ZMY_TABLE</CHECKTABLE>
     <FRKART>TEXT</FRKART>
     <CARD>CN</CARD>
     <CARDLEFT>1</CARDLEFT>
    </DD08V>
   </DD08V_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key rules:**
- Always activate the main table **before** the text table — the foreign key check requires the main table to exist
- Language field can be `SPRAS` (data element `SPRAS`) or `LANGU` (data element `LANGU`) — both are recognised by the system
- For an **ordinary foreign key** (not a text table): same `DD05M`/`DD08V` structure, but omit `<FRKART>TEXT</FRKART>` from `DD08V`

---

### CDS View / View Entity (DDLS)

**Filename**: `src/zc_my_view.ddls.xml`

The XML format is identical for both types — only `SOURCE_TYPE` differs:

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

- `SOURCE_TYPE W` → View Entity (`define view entity`, modern, **use by default**)
- `SOURCE_TYPE V` → View (`define view` + `@AbapCatalog.sqlViewName`, legacy — only if explicitly requested)

→ For DDL source syntax and full guidance: `abapgit-agent ref --topic cds-abapgit`

---

### Data Element (DTEL)

**Filename**: `src/zmy_dtel.dtel.xml`

> **XML-only object** — no `.abap` source file. Pull with: `pull --files src/zmy_dtel.dtel.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD04V>
    <ROLLNAME>ZMY_DTEL</ROLLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>Description of data element</DDTEXT>
    <REPTEXT>Description</REPTEXT>
    <SCRTEXT_S>Short text</SCRTEXT_S>
    <SCRTEXT_M>Medium text</SCRTEXT_M>
    <SCRTEXT_L>Long description text</SCRTEXT_L>
    <DTELMASTER>E</DTELMASTER>
    <DATATYPE>CHAR</DATATYPE>
    <LENG>000010</LENG>
   </DD04V>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `ROLLNAME`: Data element name
- `DDTEXT`: Description (**not** `DESCRIPT`)
- `REPTEXT` / `SCRTEXT_S` / `SCRTEXT_M` / `SCRTEXT_L`: Field labels (report heading, short, medium, long)
- `DTELMASTER`: Language key for label master (`E` for English)
- `DATATYPE`: Data type (`CHAR`, `NUMC`, `DATS`, `TIMS`, `INT4`, etc.)
- `LENG`: Length padded to 6 digits (e.g. `000010` for 10 characters)

**Omit `<PARAMID>`** unless you specifically need a SET/GET parameter ID — the serializer omits it when empty, so including an empty tag causes a permanent diff.

---

### Structure (STRU)

**Filename**: `src/zmy_struct.stru.xml`

> **XML-only object** — no `.abap` source file. abapGit uses the same TABL serializer for STRU.
> Pull with: `pull --files src/zmy_struct.stru.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZMY_STRUCT</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>INTTAB</TABCLASS>
    <DDTEXT>Description of the structure</DDTEXT>
    <CONTFLAG>A</CONTFLAG>
   </DD02V>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key difference from TABL**: `TABCLASS` is `INTTAB` (internal table / structure) instead of `TRANSP`.

**Note**: Like TABL, after the first pull abapGit expands the XML with `<DD03P_TABLE>` field definitions. When creating a new structure you only need the `<DD02V>` header.

---

### Table Type (TTYP)

**Filename**: `src/zmy_ttyp.ttyp.xml`

> **XML-only object** — no `.abap` source file.
> Pull with: `pull --files src/zmy_ttyp.ttyp.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TTYP" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD40V>
    <TYPENAME>ZMY_TTYP</TYPENAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>Description of table type</DDTEXT>
    <ROWTYPE>ZMY_STRUCT</ROWTYPE>
    <ROWKIND>S</ROWKIND>
    <DATATYPE>TABLE_T</DATATYPE>
    <ACCESSMODE>T</ACCESSMODE>
    <KEYDEF>D</KEYDEF>
    <KEYKIND>N</KEYKIND>
   </DD40V>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `TYPENAME`: Table type name
- `ROWTYPE`: Row type (a structure or data element name)
- `ROWKIND`: Row kind — `S`=Structure/Type, `D`=Data element, `R`=Reference
- `DATATYPE`: Always `TABLE_T` for table types
- `ACCESSMODE`: Table kind — `T`=Standard, `S`=Sorted, `H`=Hashed
- `KEYDEF`: Key definition — `D`=Default (standard key), `K`=User-defined
- `KEYKIND`: Key uniqueness — `N`=Non-unique, `U`=Unique

**Note**: After the first pull abapGit may expand the XML with `<DD42V>` (component definitions) if key fields are explicitly defined.

---

## Important Notes

1. **ALWAYS push to git BEFORE running pull** - abapGit reads from git
2. **Check pull output** to verify objects were recognized by abapGit
3. **Use inspect AFTER pull** to check syntax on objects in ABAP
