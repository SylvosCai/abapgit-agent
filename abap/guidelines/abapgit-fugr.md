---
layout: default
title: abapGit XML Metadata — Function Group (FUGR)
nav_order: 15
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# abapGit XML Metadata — Function Group (FUGR)

**Searchable keywords**: function group xml, fugr, function module, fm, abapgit, sapl, lzmy

> **CRITICAL: Always write XML files with a UTF-8 BOM (`\ufeff`) as the very first character**, before `<?xml ...`.
> Without the BOM, abapGit shows the object as **"M" (modified)** after every pull.

---

A FUGR with group name `ZMY_FUGR` and one FM `ZMY_MY_FUNCTION` requires these files:

| File | Purpose |
|---|---|
| `zmy_fugr.fugr.xml` | Main XML: group description, includes list, FM interfaces |
| `zmy_fugr.fugr.lzmy_fugrtop.abap` | TOP include source |
| `zmy_fugr.fugr.lzmy_fugrtop.xml` | TOP include metadata (`SUBC=I`) |
| `zmy_fugr.fugr.saplzmy_fugr.abap` | Main include source (lists all U-includes) |
| `zmy_fugr.fugr.saplzmy_fugr.xml` | Main include metadata (`SUBC=F`) |
| `zmy_fugr.fugr.lzmy_fugru01.abap` | FM implementation include (one per FM: U01, U02, …) |
| `zmy_fugr.fugr.zmy_my_function.abap` | FM source (one file per FM, named after FM in lowercase) |

> **7 files total for one FM** (6 without the U01 include causes a pull error — the SAPL source references `LZMY_FUGRU01` which abapGit can't find). For a second FM, add `lzmy_fugru02.abap` and `zmy_second_function.abap`.

---

### `zmy_fugr.fugr.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_FUGR" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <AREAT>My function group description</AREAT>
   <INCLUDES>
    <SOBJ_NAME>LZMY_FUGRTOP</SOBJ_NAME>
    <SOBJ_NAME>SAPLZMY_FUGR</SOBJ_NAME>
   </INCLUDES>
   <FUNCTIONS>
    <item>
     <FUNCNAME>ZMY_MY_FUNCTION</FUNCNAME>
     <SHORT_TEXT>Short description of the FM</SHORT_TEXT>
     <IMPORT>
      <RSIMP>
       <PARAMETER>IV_INPUT</PARAMETER>
       <TYP>STRING</TYP>
      </RSIMP>
      <RSIMP>
       <PARAMETER>IV_OPTIONAL</PARAMETER>
       <DEFAULT>&apos;&apos;</DEFAULT>
       <OPTIONAL>X</OPTIONAL>
       <TYP>FLAG</TYP>
      </RSIMP>
     </IMPORT>
     <EXPORT>
      <RSEXP>
       <PARAMETER>EV_RESULT</PARAMETER>
       <TYP>STRING</TYP>
      </RSEXP>
     </EXPORT>
     <EXCEPTION>
      <RSEXC>
       <EXCEPTION>ERROR</EXCEPTION>
      </RSEXC>
     </EXCEPTION>
    </item>
   </FUNCTIONS>
  </asx:values>
 </asx:abap>
</abapGit>
```

### `zmy_fugr.fugr.lzmy_fugrtop.abap`

```abap
FUNCTION-POOL zmy_fugr.            "MESSAGE-ID ..
```

### `zmy_fugr.fugr.lzmy_fugrtop.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>LZMY_FUGRTOP</NAME>
    <SUBC>I</SUBC>
    <FIXPT>X</FIXPT>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>
```

### `zmy_fugr.fugr.saplzmy_fugr.abap`

```abap
INCLUDE LZMY_FUGRTOP.
INCLUDE LZMY_FUGRU01.
```

One `INCLUDE` line per FM (`U01`, `U02`, …), plus `LZMY_FUGRTOP` at the top.

### `zmy_fugr.fugr.saplzmy_fugr.xml`

```xml
﻿<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>SAPLZMY_FUGR</NAME>
    <SUBC>F</SUBC>
    <RLOAD>E</RLOAD>
    <FIXPT>X</FIXPT>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>
```

### `zmy_fugr.fugr.zmy_my_function.abap`

```abap
FUNCTION zmy_my_function.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_INPUT) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_RESULT) TYPE  STRING
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  " implementation

ENDFUNCTION.
```

### `zmy_fugr.fugr.lzmy_fugru01.abap`

The FM implementation include — **always required**. The SAPL source references `INCLUDE LZMY_FUGRU01.` so this file must exist or the pull fails.

```abap
*----------------------------------------------------------------------*
***INCLUDE LZMY_FUGRU01.
*----------------------------------------------------------------------*
```

For a second FM, create `zmy_fugr.fugr.lzmy_fugru02.abap` with the same pattern (replacing `U01` with `U02`).

---

## Key Rules

- **Include naming**: TOP = `L<GROUP>TOP`, main = `SAPL<GROUP>`, FM implementation = `L<GROUP>U01` (U02, U03, …) — uppercase in XML and filenames
- **FM source file**: named after the FM in **lowercase** (`zmy_my_function.abap`, not `ZMY_MY_FUNCTION.abap`)
- **U-include file required**: `lzmy_fugru01.abap` must exist — the SAPL source references it; missing it causes a pull error
- **`INCLUDES` list** in the main XML: only lists TOP and SAPL includes — FM source includes are implicit
- **`REMOTE_CALL>R`**: add only for RFC-enabled FMs; omit for local FMs
- **Optional parameters**: include both `<OPTIONAL>X</OPTIONAL>` and `<DEFAULT>...</DEFAULT>`
- **`DOCUMENTATION` section**: omit entirely if no parameter descriptions needed — missing docs cause no diff
- **Screens/CUA**: omit `DYNPROS`/`CUA` sections for screenless function groups
- **Multiple FMs**: add one `<item>` per FM in `FUNCTIONS`; add one `INCLUDE` line per FM in the SAPL source
