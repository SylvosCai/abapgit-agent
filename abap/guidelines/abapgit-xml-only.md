---
layout: default
title: abapGit XML Metadata — XML-Only Objects
nav_order: 14
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# abapGit XML Metadata — XML-Only Objects (TABL, STRU, DTEL, TTYP, DOMA, MSAG)

XML templates for ABAP Dictionary objects that have no `.abap` source file — TABL, STRU, DTEL, TTYP, DOMA, and message classes (MSAG).

> **CRITICAL: Always write XML files with a UTF-8 BOM (`\ufeff`) as the very first character**, before `<?xml ...`.
> Without the BOM, abapGit shows the object as **"M" (modified)** after every pull.

> **CRITICAL: Only include fields that abapGit's serializer actually writes. Never add fields with
> default values.** Extra fields cause a permanent "M" (modified) diff.

> **For CLAS, INTF, PROG, DDLS, DCLS, FUGR** (have source files): see `abapgit-agent ref --topic abapgit`
> **For DDLS, DCLS** (CDS — have source files): also see `abapgit-agent ref --topic abapgit`

**Searchable keywords**: table xml, structure xml, data element xml, table type xml, domain xml, message class xml, tabl, stru, dtel, ttyp, doma, msag, dictionary

---

### Table (TABL)

**Filename**: `src/zmy_table.tabl.xml`

> Pull with: `pull --files src/zmy_table.tabl.xml`

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

**Note**: When abapGit serializes a table it also writes `<DD09L>` (technical settings) and `<DD03P_TABLE>` (field definitions). You only need the `<DD02V>` header when creating a new table; the rest is generated on pull.

**`DD03P` field-level rules** (apply when editing an existing table XML that includes `<DD03P_TABLE>`): include `<SHLPORIGIN>D</SHLPORIGIN>` on fields where the Dictionary provides value help (domain with fixed values or search help); omit otherwise. For raw-type fields (no `ROLLNAME`, e.g. `CHAR`, `NUMC`), the serializer writes `<MASK>` **before** `<DDTEXT>`. For fields with a `ROLLNAME`, only `ROLLNAME` appears. Wrong order or missing `SHLPORIGIN` causes a permanent diff.

#### Text Tables and Foreign Keys

A **text table** stores translatable texts for another table (the "main table"). It shares the same key fields plus a language field (`SPRAS`/`LANGU`).

**Recognition markers:** `<DD09L><UEBERSETZ>X</UEBERSETZ>` (text table) and `<DD08V><FRKART>TEXT</FRKART>` (text-table FK). Three required sections: `DD03P_TABLE` (fields), `DD05M_TABLE` (FK field mappings — one per main-table key, all anchored to `MANDT`), `DD08V_TABLE` (FK relationship).

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
    <!-- repeat for each remaining key field (ID2, etc.) -->
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
- Always activate the main table **before** the text table — foreign key check requires the main table to exist
- Language field can be `SPRAS` (data element `SPRAS`) or `LANGU` (data element `LANGU`)
- For an **ordinary foreign key**: same structure, but omit `<FRKART>TEXT</FRKART>` from `DD08V`

---

---

### Data Element (DTEL)

**Filename**: `src/zmy_dtel.dtel.xml`

> Pull with: `pull --files src/zmy_dtel.dtel.xml`

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

**Omit `<PARAMID>`** unless you specifically need a SET/GET parameter ID — the serializer omits it when empty.

---

### Structure (STRU)

**Filename**: `src/zmy_struct.stru.xml`

> abapGit uses the same TABL serializer for STRU.
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

**Note**: Like TABL, after the first pull abapGit expands the XML with `<DD03P_TABLE>` field definitions.

---

### Table Type (TTYP)

**Filename**: `src/zmy_ttyp.ttyp.xml`

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

**Note**: After the first pull abapGit may expand the XML with `<DD42V>` (key field definitions).

---

### Domain (DOMA)

**Filename**: `src/zmy_domain.doma.xml`

> Pull with: `pull --files src/zmy_domain.doma.xml`

abapGit uses the view `DD01V` as the root element (not `DD01L`). The serializer reads from the system view.

**Simple domain (no fixed values):**

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DOMA" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD01V>
    <DOMNAME>ZMY_DOMAIN</DOMNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DATATYPE>CHAR</DATATYPE>
    <LENG>000030</LENG>
    <OUTPUTLEN>000030</OUTPUTLEN>
    <DDTEXT>My domain description</DDTEXT>
   </DD01V>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Domain with fixed values** (add `<VALEXI>X</VALEXI>` and `<DD07V_TAB>`):

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DOMA" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD01V>
    <DOMNAME>ZMY_STATUS</DOMNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DATATYPE>CHAR</DATATYPE>
    <LENG>000001</LENG>
    <OUTPUTLEN>000001</OUTPUTLEN>
    <VALEXI>X</VALEXI>
    <DDTEXT>Status</DDTEXT>
   </DD01V>
   <DD07V_TAB>
    <DD07V>
     <VALPOS>0001</VALPOS>
     <DDLANGUAGE>E</DDLANGUAGE>
     <DOMVALUE_L>A</DOMVALUE_L>
     <DDTEXT>Active</DDTEXT>
    </DD07V>
    <DD07V>
     <VALPOS>0002</VALPOS>
     <DDLANGUAGE>E</DDLANGUAGE>
     <DOMVALUE_L>I</DOMVALUE_L>
     <DDTEXT>Inactive</DDTEXT>
    </DD07V>
   </DD07V_TAB>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `DOMNAME`: Domain name
- `DDTEXT`: Description (**not** `DESCRIPT`)
- `DATATYPE`: ABAP data type (`CHAR`, `NUMC`, `DATS`, `TIMS`, `INT4`, `STRG`, etc.)
- `LENG`: Length padded to 6 digits (omit for `STRG`/`INT4` — no fixed length)
- `OUTPUTLEN`: Output length (usually same as `LENG`; omit for `STRG`)
- `LOWERCASE`: `X` if lowercase input is allowed (omit if not)
- `VALEXI`: `X` if the domain has fixed values — required when `DD07V_TAB` is present
- `ENTITYTAB`: Value table name (check table for valid values — omit if none)

**Note**: After pull the serializer may add `<I18N_LANGS>`, `<DD01_TEXTS>`, `<DD07_TEXTS>` for translations — do not write these manually.

---

### Message Class (MSAG)

**Filename**: `src/zmy_msgs.msag.xml`

> Pull with: `pull --files src/zmy_msgs.msag.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_MSAG" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <T100A>
    <ARBGB>ZMY_MSGS</ARBGB>
    <MASTERLANG>E</MASTERLANG>
    <STEXT>My message class description</STEXT>
   </T100A>
   <T100>
    <T100>
     <SPRSL>E</SPRSL>
     <ARBGB>ZMY_MSGS</ARBGB>
     <MSGNR>000</MSGNR>
     <TEXT>Object &1 not found</TEXT>
    </T100>
    <T100>
     <SPRSL>E</SPRSL>
     <ARBGB>ZMY_MSGS</ARBGB>
     <MSGNR>001</MSGNR>
     <TEXT>Operation completed successfully</TEXT>
    </T100>
   </T100>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `T100A/ARBGB`: Message class name (same as filename stem)
- `T100A/MASTERLANG`: Master language (`E` for English)
- `T100A/STEXT`: Short description of the message class
- `T100/SPRSL`: Language key
- `T100/MSGNR`: 3-digit message number (`000`–`999`)
- `T100/TEXT`: Message text — use `&1`–`&4` for placeholders (serialized as `&amp;1`–`&amp;4` in XML)

**Note**: The `<T100>` wrapper contains repeated `<T100>` child elements — one per message.
