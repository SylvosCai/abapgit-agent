---
layout: default
title: abapGit XML Metadata — CDS Objects
nav_order: 16
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# abapGit XML Metadata — CDS Objects (DDLS, DCLS)

This file covers the XML metadata for **all CDS object types**: classic views,
view entities, table functions, and access controls. For the DDL *source*
syntax (joins, annotations, semantics), see `abapgit-agent ref --topic cds`.
For table-function authoring (logical schema, AMDP, activation order), see
`abapgit-agent ref --topic table-function`.

**Searchable keywords**: cds view xml, view entity xml, ddls xml, dcls xml,
table function xml, source_type, ddlname, dclname, amdp class xml, clsccincl,
abapgit, serializer

> **CRITICAL: Always write XML files with a UTF-8 BOM (`\ufeff`) as the very first character**, before `<?xml ...`.
> Without the BOM, abapGit shows the object as **"M" (modified)** after every pull because the
> serializer always produces XML with BOM — and every byte matters.

> **CRITICAL: Only include fields that abapGit's serializer actually writes. Never add fields with
> default values.** Extra fields cause a permanent "M" (modified) diff.

---

## CDS View / View Entity (DDLS)

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
- `SOURCE_TYPE F` → Table Function (`define table function`, AMDP-backed — see below)

→ For DDL source syntax: `abapgit-agent ref --topic cds`

---

## CDS Table Function (DDLS, SOURCE_TYPE F)

A table function is a CDS view whose body is computed by an **AMDP** method
running on HANA. It is the only way to access tables in a *replicated /
co-deployed* HANA schema (via `$ABAP.schema( <logical_schema> )`) — plain CDS
view entities don't support that syntax.

**Files** (four required):

```
src/z_tf_<name>.ddls.asddls    ← table function signature
src/z_tf_<name>.ddls.xml       ← DDLS metadata (SOURCE_TYPE = F)
src/zcl_tf_<name>.clas.abap    ← AMDP implementation class
src/zcl_tf_<name>.clas.xml     ← class metadata (CLSCCINCL = X)
```

**DDLS XML** — `SOURCE_TYPE F` is the only difference from a normal DDLS:

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DDLS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DDLS>
    <DDLNAME>Z_TF_MY_FUNCTION</DDLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>My Table Function</DDTEXT>
    <SOURCE_TYPE>F</SOURCE_TYPE>
   </DDLS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**DDLS source** — `implemented by method <CLAS>=>METHOD;`:

```cds
@EndUserText.label: 'My Table Function'
@ClientHandling.type: #CLIENT_DEPENDENT
define table function Z_TF_MY_FUNCTION
returns {
  client       : abap.clnt;
  vendor       : abap.char(10);
  amount       : abap.dec(23,2);
  currency     : abap.cuky;
}
implemented by method ZCL_TF_MY_FUNCTION=>GET_DATA;
```

**AMDP class XML** — `CLSCCINCL = X` is required even when there are NO local
class-include files (`.clas.locals_def.abap` etc.). The abapGit serializer
writes it for any class containing an AMDP method. Omitting it makes the
deserialized class drop its method declarations on the server.

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_TF_MY_FUNCTION</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>My Table Function AMDP</DESCRIPT>
    <STATE>1</STATE>
    <CLSCCINCL>X</CLSCCINCL>
    <FIXPT>X</FIXPT>
    <UNICODE>X</UNICODE>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**AMDP class source** — `INTERFACES if_amdp_marker_hdb`, the method declared
`FOR TABLE FUNCTION <ddls>`, and the implementation header in **uppercase class
name** (`CLASS ZCL_X IMPLEMENTATION.`) — that is what the abapGit serializer
produces; lowercase causes round-trip diffs:

```abap
CLASS zcl_tf_my_function DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.

    CLASS-METHODS get_data FOR TABLE FUNCTION z_tf_my_function.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TF_MY_FUNCTION IMPLEMENTATION.

  METHOD get_data BY DATABASE FUNCTION FOR HDB
                  LANGUAGE SQLSCRIPT
                  OPTIONS READ-ONLY.

    RETURN SELECT
             t.mandt AS client,
             t.lifnr AS vendor,
             t.dmbtr AS amount,
             t.waers AS currency
           FROM "$ABAP.schema( FRA_CORE_ERP )".rbkp AS t
           WHERE t.mandt = SESSION_CONTEXT( 'CLIENT' );

  ENDMETHOD.
ENDCLASS.
```

> **For everything else** (return-type choices, activation order, why `inspect`
> is unreliable for table functions, common errors): see
> `abapgit-agent ref --topic table-function`.

---

## CDS Access Control (DCLS)

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
- Pass the `.dcls.asdcls` file to `pull --files` (same convention as DDLS)
