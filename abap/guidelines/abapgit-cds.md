---
layout: default
title: abapGit XML Metadata — CDS and RAP Objects
nav_order: 16
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# abapGit XML Metadata — CDS and RAP Objects (DDLS, DCLS, BDEF, SRVD, DDLX)

This file covers CDS and RAP source/metadata conventions: classic views,
view entities, table functions, access controls, behavior/service definitions and metadata extensions. For the DDL *source*
syntax (joins, annotations, semantics), see `abapgit-agent ref --topic cds`.
For table-function authoring (logical schema, AMDP, activation order), see
`abapgit-agent ref --topic table-function`.

**Searchable keywords**: cds view xml, view entity xml, ddls xml, dcls xml,
table function xml, source_type, ddlname, dclname, amdp class xml, clsccincl,
abapgit, serializer, bdef xml, srvd xml, ddlx xml, behavior definition,
service definition, metadata extension

> **CRITICAL: Always write XML files with a UTF-8 BOM (`\ufeff`) as the very first character**, before `<?xml ...`.
> Without the BOM, abapGit shows the object as **"M" (modified)** after every pull because the
> serializer always produces XML with BOM — and every byte matters.

> **CRITICAL: Only include fields that abapGit's serializer actually writes. Never add fields with
> default values.** Extra fields cause a permanent "M" (modified) diff.

---

## RAP Source and Metadata

| Type | Native source | Metadata | Meaning |
|------|---------------|----------|---------|
| BDEF | `<name>.bdef.asbdef` | `<name>.bdef.xml` | Declarative behavior; procedural implementation is a separate CLAS |
| SRVD | `<name>.srvd.srvdsrv` | `<name>.srvd.xml` | Service entity exposure, not runtime binding publication |
| DDLX | `<name>.ddlx.asddlxs` | `<name>.ddlx.xml` | Metadata-extension annotations; not a CDS view entity |

Use XML from a verified abapGit serializer/export, not the DDLS templates below. Legacy `.srvd.assrvd` is accepted instead of canonical source. The source policy requires exactly one native source per identity; both SRVD suffixes together fail validation. DDLX older XML-embedded exports are not a substitute for the required external source in this agent workflow.

Prepare persistence/referenced DDLS before native `syntax`. A BDEF source check does not check or select its behavior implementation CLAS. `syntax` checks supplied source without saving/activating; available APIs, metadata language context and dependencies still matter. For selective `pull`, pass native source, not RAP XML. Source guards also apply on first pull and with conflict ignore. There is no automatic dependency graph, cascading deletion or inactive-view fallback.

Configured abaplint validates BDEF/DDLX parser rules (verified with 2.120.103), not SRVD source or full SAP semantics. SCI zero findings do not establish full RAP coverage. SRVB has no dedicated editing/publication support here. Namespace filename selectors decode `#namespace#name` and currently limit decoded RAP names to 30 characters; that is an agent limit, not a universal SAP limit.

### RAP XML Examples

These are generic `ZRAP_` starter examples based on verified serializer exports, with names, descriptions and name-bearing URIs replaced consistently. Preserve the XML BOM and serializer structure; available fields and language settings can differ by SAP/abapGit version. Use the project's configured package, not a package copied from an example.

For detailed RAP language and business-object guidance, use `abapgit-agent ref --topic rap` and `abapgit-agent ref --topic eml`, backed by the configured SAP reference material. The examples here cover basic object purpose and XML only, not a complete RAP application.

#### Behavior Definition (BDEF)

A BDEF declares the root entity's behavior. Its name matches the root DDLS: `ZRAP_ROOT`. A minimal managed CRUD definition can work without an implementation class; custom behavior may require a separate behavior-pool CLAS.

**Files**: `src/zrap_root.bdef.asbdef` (source) + `src/zrap_root.bdef.xml` (metadata). Prepare the referenced `ZRAP_ROOT` DDLS and persistence first.

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_BDEF" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <BDEF>
    <NAME>ZRAP_ROOT</NAME>
    <TYPE>BDEF/BDO</TYPE>
    <DESCRIPTION>Example RAP root behavior</DESCRIPTION>
    <DESCRIPTION_TEXT_LIMIT>60</DESCRIPTION_TEXT_LIMIT>
    <LANGUAGE>EN</LANGUAGE>
    <LINKS>
     <item>
      <HREF>./zrap_root/source/main/versions</HREF>
      <REL>http://www.sap.com/adt/relations/versions</REL>
      <TITLE>Historic versions</TITLE>
     </item>
     <item>
      <HREF>/sap/bc/adt/repository/informationsystem/abaplanguageversions?uri=%2Fsap%2Fbc%2Fadt%2Fbo%2Fbehaviordefinitions%2Fzrap_root</HREF>
      <REL>http://www.sap.com/adt/relations/informationsystem/abaplanguageversions</REL>
      <TYPE>application/vnd.sap.adt.nameditems.v1+xml</TYPE>
      <TITLE>Allowed ABAP language versions</TITLE>
     </item>
     <item>
      <HREF>./zrap_root/source/main</HREF>
      <REL>http://www.sap.com/adt/relations/source</REL>
      <TYPE>text/plain</TYPE>
      <TITLE>Source Content</TITLE>
     </item>
     <item>
      <HREF>./zrap_root/source/main</HREF>
      <REL>http://www.sap.com/adt/relations/source</REL>
      <TYPE>text/html</TYPE>
      <TITLE>Source Content (HTML)</TITLE>
     </item>
    </LINKS>
    <MASTER_LANGUAGE>EN</MASTER_LANGUAGE>
    <ABAP_LANGU_VERSION>X</ABAP_LANGU_VERSION>
    <SOURCE_URI>./zrap_root/source/main</SOURCE_URI>
    <SOURCE_TYPE>ABAP_SOURCE</SOURCE_TYPE>
    <SOURCE_FIXED_POINT_ARITHMETIC>true</SOURCE_FIXED_POINT_ARITHMETIC>
    <SOURCE_UNICODE_CHECKS_ACTIVE>true</SOURCE_UNICODE_CHECKS_ACTIVE>
   </BDEF>
  </asx:values>
 </asx:abap>
</abapGit>
```

`BDEF/BDO` is the native metadata type. The language/Unicode/fixed-point fields above come from the verified export; do not assume those values fit every target language setting.

#### Service Definition (SRVD)

A service definition exposes CDS entities, such as `ZRAP_ROOT`. It does not create or publish a runtime service binding.

**Files**: `src/zrap_service.srvd.srvdsrv` (source) + `src/zrap_service.srvd.xml` (metadata).

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_SRVD" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <SRVD>
    <NAME>ZRAP_SERVICE</NAME>
    <TYPE>SRVD/SRV</TYPE>
    <DESCRIPTION>Example RAP service</DESCRIPTION>
    <LANGUAGE>EN</LANGUAGE>
    <MASTER_LANGUAGE>EN</MASTER_LANGUAGE>
    <SOURCE_URI>./zrap_service/source/main</SOURCE_URI>
    <SOURCE_TYPE>ABAP_SOURCE</SOURCE_TYPE>
    <SOURCE_ORIGIN_DESCRIPTION>ABAP Development Tools</SOURCE_ORIGIN_DESCRIPTION>
    <SRVD_SOURCE_TYPE>S</SRVD_SOURCE_TYPE>
    <SRVD_SOURCE_TYPE_DESC>Definition</SRVD_SOURCE_TYPE_DESC>
   </SRVD>
  </asx:values>
 </asx:abap>
</abapGit>
```

#### Metadata Extension (DDLX)

A metadata extension contains annotations for a CDS entity, such as `ZRAP_ROOT`, which must allow metadata extensions. Its own name can differ from that entity's name: here it is `ZRAP_ANNOTATIONS`.

**Files**: `src/zrap_annotations.ddlx.asddlxs` (source) + `src/zrap_annotations.ddlx.xml` (metadata).

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DDLX" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DDLX>
    <METADATA>
     <NAME>ZRAP_ANNOTATIONS</NAME>
     <DESCRIPTION>Example RAP annotations</DESCRIPTION>
     <MASTER_LANGUAGE>EN</MASTER_LANGUAGE>
    </METADATA>
   </DDLX>
  </asx:values>
 </asx:abap>
</abapGit>
```

Keep the nested `DDLX/METADATA` structure. The annotations themselves belong in `.ddlx.asddlxs`, not in the DDLS XML template or as a replacement XML-embedded source.

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
