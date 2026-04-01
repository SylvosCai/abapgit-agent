---
layout: default
title: view - View Definitions
nav_order: 2
parent: Explore Commands
---

# view Command Requirements

## Overview

View ABAP object source code directly from the ABAP system. This command retrieves and displays source code for objects that may not exist locally in your git repository, enabling developers to understand class definitions, method signatures, structure components, and data element types without pulling the entire object.

**This is the PRIMARY way to explore unfamiliar ABAP objects.**

## Use Cases

- **Understand unfamiliar code**: View a class definition from a dependency package
- **Check method signatures**: See method parameters and return types before calling
- **Inspect data structures**: View table or structure field definitions
- **Review interface definitions**: Check interface methods and constants
- **Quick reference**: Look up definitions without opening SE80 or ADT
- **View source includes**: View method implementations from where command references
- **View program source**: View full program or include source code

## Command

```bash
# View single object (auto-detect type from TADIR)
abapgit-agent view --objects ZCL_MY_CLASS
abapgit-agent view --objects ZIF_MY_INTERFACE
abapgit-agent view --objects ZMY_TABLE

# View with explicit type
abapgit-agent view --objects ZCL_MY_CLASS --type CLAS
abapgit-agent view --objects ZIF_MY_INT --type INTF
abapgit-agent view --objects ZMY_STRUCT --type STRU
abapgit-agent view --objects ZMY_TABLE --type TABL
abapgit-agent view --objects ZMY_DTEL --type DTEL
abapgit-agent view --objects ZMY_TTYP --type TTYP
abapgit-agent view --objects ZC_MY_CDS_VIEW --type DDLS

# View multiple objects
abapgit-agent view --objects ZCL_CLASS1,ZCL_CLASS2,ZIF_INTERFACE1

# View source include (from where command output)
abapgit-agent view --objects ZCL_ABGAGT_COMMAND_UNIT=======CM007

# View method implementation
abapgit-agent view --objects ZCL_ABGAGT_AGENT=============CM012

# View unit test class
abapgit-agent view --objects ZCL_MY_CLASS=============CCAU

# View program source
abapgit-agent view --objects ZMY_PROGRAM --type PROG

# View with FULL source (all sections, clean readable code)
# Use this to read and understand logic, or for AI code analysis
abapgit-agent view --objects ZCL_MY_CLASS --full

# View FULL source with dual line numbers (for setting breakpoints / debugging)
abapgit-agent view --objects ZCL_MY_CLASS --full --lines

# Lowercase names and types are supported
abapgit-agent view --objects zcl_my_class --type clas

# Output as JSON (for scripting/AI processing)
abapgit-agent view --objects ZCL_MY_CLASS --json

# Full source as JSON (clean sections array, no pre-rendered numbers)
abapgit-agent view --objects ZCL_MY_CLASS --full --json
abapgit-agent view --objects ZCL_MY_CLASS --full --lines --json
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Object must exist in the ABAP system

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--objects` | Yes | Comma-separated list of object names (e.g., `ZCL_MY_CLASS,ZIF_MY_INTERFACE`) |
| `--type` | No | Object type (CLAS, INTF, TABL, STRU, DTEL, TTYP, DDLS, STOB, PROG). Auto-detected from TADIR if not specified |
| `--full` | No | Return all sections (definition + all method implementations) as clean readable source. For CLAS: shows CU/CO/CP/CM*/CCDEF/CCIMP/CCAU sections. For INTF/PROG/DDLS: shows full source |
| `--lines` | No | Add dual line numbers to `--full` output: `G [N]  code` where G is assembled-source global line and [N] is include-relative. Also adds breakpoint hints to method headers. Only meaningful with `--full` |
| `--json` | No | Output raw JSON only (for scripting) |

---

## Tasks

### 1. Validate Parameters

- `--objects` must be specified
- Object names are converted to uppercase automatically

### 2. Load Configuration

Read `.abapGitAgent` for credentials

### 3. Fetch CSRF Token

```bash
GET /health (with X-CSRF-Token: fetch)
```

### 4. Make View Request

**Endpoint:** `POST /view`

**Request Body:**
```json
{
  "objects": ["ZCL_MY_CLASS", "ZIF_MY_INTERFACE"],
  "type": "CLAS"
}
```

### 5. Display Results

---

## Output

### Class Full Source (`--full`)

When `--full` is specified for a class, the output shows all sections (definition + all method implementations) as clean readable source:

```
  * ---- Section: Public Section (CU) ----
  CLASS zcl_my_class DEFINITION PUBLIC FINAL CREATE PUBLIC.
    PUBLIC SECTION.
      METHODS:
        constructor IMPORTING iv_host TYPE string,
        execute RETURNING VALUE(rv_result) TYPE string.
    PRIVATE SECTION.
      DATA mv_host TYPE string.
  ENDCLASS.

  * ---- Section: Protected Section (CO) ----
  ...
  * ---- Section: Private Section (CP) ----
  ...
  * ---- Method: CONSTRUCTOR (CM001) ----
  METHOD constructor.
    mv_host = iv_host.
  ENDMETHOD.
  * ---- Method: EXECUTE (CM002) ----
  METHOD execute.
    DATA lv_x TYPE i.
    lv_x = 1.
    rv_result = CONV string( lv_x ).
  ENDMETHOD.
  * ---- Section: Local Definitions (from .clas.locals_def.abap) ----
  ...
  * ---- Section: Unit Test (from .clas.testclasses.abap) ----
  ...
```

### Class Full Source with Line Numbers (`--full --lines`)

Adding `--lines` renders dual line numbers on every line and adds a ready-to-use breakpoint hint to each method header:

- **G** (left, no brackets): assembled-source global line → use directly with `debug set --objects ZCL_FOO:G` or `debug set --files src/zcl_foo.clas.abap:G`
- **[N]** (brackets): include-relative, restarts at 1 per method — useful for code navigation only
- **Method header hint**: automatically points to the first executable statement (skips `METHOD`, blank lines, comments, and `DATA`/`FINAL`/`TYPES`/`CONSTANTS` declarations, including multi-line `DATA:` blocks and inline `DATA(x) =` forms)

**Regular CM\* methods** use assembled-source global line numbers:

```
  * ---- Method: CONSTRUCTOR (CM001) — breakpoint: debug set --objects ZCL_MY_CLASS:14 ----
    13 [  1]    METHOD constructor.
    14 [  2]      mv_host = iv_host.
    15 [  3]    ENDMETHOD.
  * ---- Method: EXECUTE (CM002) — breakpoint: debug set --objects ZCL_MY_CLASS:19 ----
    17 [  1]    METHOD execute.
    18 [  2]      DATA lv_x TYPE i.
    19 [  3]      lv_x = 1.
    20 [  4]      rv_result = CONV string( lv_x ).
    21 [  5]    ENDMETHOD.
```

**Unit test methods (CCAU) and local class methods (CCIMP)** live in separate ADT sub-includes. Their sections show section-local line numbers and a `--include` hint per method:

```
  * ---- Section: Unit Test (from .clas.testclasses.abap) ----
  * ---- Method: SETUP — breakpoint: debug set --objects ZCL_MY_CLASS:12 --include testclasses ----
    10    METHOD setup.
    11      DATA lv_x TYPE i.
    12      mo_cut = NEW #( ).
    13    ENDMETHOD.

  * ---- Section: Local Implementations (from .clas.locals_imp.abap) ----
  * ---- Method: ZIF_FOO~DO_SOMETHING — breakpoint: debug set --objects ZCL_MY_CLASS:5 --include locals_imp ----
     3    METHOD zif_foo~do_something.
     4      DATA lv_x TYPE i.
     5      lv_x = iv_input.
     6    ENDMETHOD.
```

Line numbers in sub-include sections are **section-local** (matching the `.clas.testclasses.abap` / `.clas.locals_imp.abap` file), not assembled-source globals. The `--include` flag value mirrors the abapGit file suffix.

**How global line numbers are computed:** Node.js reads the local `.clas.abap` file (own classes) or fetches `/sap/bc/adt/oo/classes/<name>/source/main` from ADT (library classes), then scans for `METHOD <name>.` to determine each method's start line.

**Setting a breakpoint from the output:**

```bash
# Regular CM* method — copy hint directly from the method header
abapgit-agent debug set --objects ZCL_MY_CLASS:19

# Unit test method
abapgit-agent debug set --objects ZCL_MY_CLASS:12 --include testclasses

# Local class method
abapgit-agent debug set --objects ZCL_MY_CLASS:5 --include locals_imp
```

Non-CLAS types (INTF, PROG, DDLS): single section, section-local line numbers only (no `[N]` brackets).

#### Using `--full` vs `--full --lines`

| Goal | Command |
|---|---|
| Read and understand class logic | `view --objects ZCL_MY_CLASS --full` |
| Set a breakpoint in a method | `view --objects ZCL_MY_CLASS --full --lines` |
| AI code analysis / review | `view --objects ZCL_MY_CLASS --full` |

### Class Definition (default, public section only)

```
📖 ZCL_MY_CLASS (Class)
   Class ZCL_MY_CLASS in $PACKAGE_NAME

CLASS zcl_my_class DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_interface.

    METHODS:
      constructor
        IMPORTING
          !iv_name TYPE string OPTIONAL,
      get_value
        RETURNING
          VALUE(rv_result) TYPE string.
ENDCLASS.
```

### Interface Definition (INTF)

```
📖 ZIF_MY_INTERFACE (Interface)
   Interface ZIF_MY_INTERFACE in $PACKAGE_NAME

INTERFACE zif_my_interface PUBLIC.

  CONSTANTS:
    gc_value TYPE string VALUE 'test'.

  METHODS:
    process
      IMPORTING
        !iv_data TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool,
    get_status
      RETURNING
        VALUE(rv_status) TYPE string.

ENDINTERFACE.
```

### Table Definition (TABL)

```
📖 SFLIGHT (Table)
   Table SFLIGHT in SAPBC_DATAMODEL

TABLE SFLIGHT:
|------------------+-----+----------+----------+--------------------------------+--------------------------------------------------------------|
| Field            | Key | Type     |   Length | Data Elem                      | Description                                                  |
|------------------+-----+----------+----------+--------------------------------+--------------------------------------------------------------|
| MANDT            | X   | CLNT     |        3 | MANDT                          | Client                                                      |
| CARRID           | X   | CHAR     |        3 | S_CARR_ID                      | Airline Code                                                |
| CONNID           | X   | NUMC     |        4 | S_CONN_ID                      | Connection Number                                            |
| FLDATE           | X   | DATS     |        8 | S_DATE                         | Flight Date                                                 |
| PRICE            |     | CURR     |       16 | S_PRICE                        | Airfare                                                     |
| CURRENCY         |     | CUKY     |        5 | S_CURR                         | Airline Currency                                             |
|------------------+-----+----------+----------+--------------------------------+--------------------------------------------------------------|
```

### Structure Definition (STRU)

```
📖 SFLIGHT (Structure)
   Structure SFLIGHT in SAPBC_DATAMODEL

STRUCTURE SFLIGHT:
|------------------+-----+----------+----------+--------------------------------+--------------------------------------------------------------|
| Field            | Key | Type     |   Length | Data Elem                      | Description                                                  |
|------------------+-----+----------+----------+--------------------------------+--------------------------------------------------------------|
| MANDT            | X   | CLNT     |        3 | MANDT                          | Client                                                      |
| CARRID           | X   | CHAR     |        3 | S_CARR_ID                      | Airline Code                                                |
|------------------+-----+----------+----------+--------------------------------+--------------------------------------------------------------|
```

### Data Element Definition (DTEL)

```
📖 S_CARR_ID (Data Element)
   Airline Code

DATA ELEMENT S_CARR_ID:
┌────────────────────┬──────────────────────────────────────────┐
│ Property           │ Value                                    │
├────────────────────┼──────────────────────────────────────────┤
│ Data Type          │ CHAR                                     │
│ Length             │ 3                                        │
│ Description        │ Airline Code                             │
│ Domain             │ S_CARR_ID                                │
└────────────────────┴──────────────────────────────────────────┘
```

### Table Type Definition (TTYP)

```
📖 DDL2DDICWARNINGS (Table Type)
   Table Type DDL2DDICWARNINGS in SDDL_BASIC_FUNCTIONS

   Line Type: DDL2DDICERR
   Access Mode: STANDARD
   Key Definition: WITH KEY
```

### CDS View Definition (DDLS)

```
📖 ZC_MY_CDS_VIEW (CDS View)
   CDS View ZC_MY_CDS_VIEW in $PACKAGE

@AbapCatalog.sqlViewName: 'ZCMYVIEW'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'My CDS View'
define view ZC_MY_CDS_VIEW as select from tdevc
{
  key devclass as Devclass,
      parentcl as ParentPackage,
      ctext    as Description
}
where devclass not like '$%'
```

### Structured Object Definition (STOB)

STOB (Structured Object) objects are automatically generated by CDS views. The viewer uses the `DDDDLSRC02BT` table to find the corresponding DDLS name and displays the CDS view source.

```
📖 ZC_MY_ENTITY (Structured Object (from CDS View ZC_MY_CDS_VIEW))
   Structured Object ZC_MY_ENTITY in $PACKAGE (CDS View: ZC_MY_CDS_VIEW)

@EndUserText.label: 'My Entity'
@AbapCatalog.sqlViewName: 'ZCMYENTITY'
@ClientHandling.type: #CLIENT_DEPENDENT
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view ZC_MY_CDS_VIEW as select from my_table
{
    key id as Id,
    name as Name,
    status as Status
}
```

### Source Include / Program (PROG)

```
📖 ZCL_ABGAGT_COMMAND_UNIT=======CM007 (Program)
   Program ZCL_ABGAGT_COMMAND_UNIT=======CM007

METHOD constructor.
  super->constructor( ).
  mo_util = zcl_abgagt_util=>get_instance( ).
ENDMETHOD.
```

### Multiple Objects

```
📖 Viewing 3 Objects

1️⃣  ZCL_CLASS1 (Class)
    └─ Class ZCL_CLASS1 in $PACKAGE

2️⃣  ZIF_INTERFACE1 (Interface)
    └─ Interface ZIF_INTERFACE1 in $PACKAGE

3️⃣  ZMY_TABLE (Table)
    └─ Table ZMY_TABLE in $PACKAGE
```

### JSON Output (Pure Scripting)

```json
{
  "SUCCESS": true,
  "COMMAND": "VIEW",
  "MESSAGE": "Retrieved 1 object(s)",
  "OBJECTS": [
    {
      "NAME": "ZCL_MY_CLASS",
      "TYPE": "CLAS",
      "TYPE_TEXT": "Class",
      "DESCRIPTION": "Class ZCL_MY_CLASS in $PACKAGE",
      "SOURCE": "CLASS zcl_my_class DEFINITION PUBLIC.\n  ...",
      "DEFINITION": "",
      "COMPONENTS": []
    }
  ],
  "SUMMARY": {
    "TOTAL": 1,
    "BY_TYPE": ["CLAS"]
  },
  "ERROR": ""
}
```

### JSON Output with `--full` flag

When `--full` is specified, the response includes a `sections` array instead of `source`. Line number rendering is done client-side by Node.js from this clean JSON:

```json
{
  "SUCCESS": true,
  "COMMAND": "VIEW",
  "MESSAGE": "Retrieved 1 object(s)",
  "OBJECTS": [
    {
      "NAME": "ZCL_MY_CLASS",
      "TYPE": "CLAS",
      "TYPE_TEXT": "Class",
      "DESCRIPTION": "Class ZCL_MY_CLASS in $PACKAGE",
      "SECTIONS": [
        { "SUFFIX": "CU",    "DESCRIPTION": "Public Section",      "LINES": ["CLASS zcl_my_class DEFINITION ...", "  PUBLIC SECTION.", "  ..."] },
        { "SUFFIX": "CO",    "DESCRIPTION": "Protected Section",   "LINES": ["  PROTECTED SECTION.", "  ..."] },
        { "SUFFIX": "CP",    "DESCRIPTION": "Private Section",     "LINES": ["  PRIVATE SECTION.", "  DATA mv_host TYPE string.", "ENDCLASS."] },
        { "SUFFIX": "CM001", "DESCRIPTION": "Class Method", "METHOD_NAME": "CONSTRUCTOR", "LINES": ["METHOD constructor.", "  mv_host = iv_host.", "ENDMETHOD."] },
        { "SUFFIX": "CM002", "DESCRIPTION": "Class Method", "METHOD_NAME": "EXECUTE",     "LINES": ["METHOD execute.", "  DATA lv_x TYPE i.", "  lv_x = 1.", "ENDMETHOD."] },
        { "SUFFIX": "CCDEF", "DESCRIPTION": "Local Definitions",   "FILE": "locals_def",  "LINES": [] },
        { "SUFFIX": "CCIMP", "DESCRIPTION": "Local Implementations","FILE": "locals_imp", "LINES": ["CLASS lcl_helper IMPLEMENTATION.", "ENDCLASS."] },
        { "SUFFIX": "CCAU",  "DESCRIPTION": "Unit Test",           "FILE": "testclasses", "LINES": [] }
      ],
      "COMPONENTS": []
    }
  ],
  "SUMMARY": { "TOTAL": 1, "BY_TYPE": ["CLAS"] },
  "ERROR": ""
}
```

The `FILE` field (when present) signals that the section comes from a separate git file (`.clas.locals_def.abap`, `.clas.locals_imp.abap`, `.clas.testclasses.abap`). Empty sections (no source found) still appear with empty `LINES` arrays so the client knows they exist.

---

## Response Structure

### JSON Response Schema

```json
{
  "SUCCESS": boolean,
  "COMMAND": "VIEW",
  "MESSAGE": "string",
  "OBJECTS": [
    {
      "NAME": "string",
      "TYPE": "CLAS|INTF|TABL|STRU|DTEL|TTYP|DDLS|STOB|PROG",
      "TYPE_TEXT": "string",
      "DESCRIPTION": "string",
      "DOMAIN": "string",           // For DTEL
      "DOMAIN_TYPE": "string",      // For DTEL
      "DOMAIN_LENGTH": number,      // For DTEL
      "DOMAIN_DECIMALS": number,    // For DTEL
      "SOURCE": "string",           // Full ABAP source (CLAS/INTF/DDLS/STOB) — omitted when sections present
      "SECTIONS": [                 // Present only when --full is used (CLAS/INTF/PROG/DDLS)
        {
          "SUFFIX": "string",       // CU, CO, CP, CM001..CMxxx, CCDEF, CCIMP, CCAU
          "DESCRIPTION": "string",  // Human-readable section label
          "METHOD_NAME": "string",  // Present for CM* sections only
          "FILE": "string",         // Present for CCDEF/CCIMP/CCAU — name of separate git file
          "LINES": ["string"]       // Source lines (no line numbers — calculated client-side)
        }
      ],
      "NOT_FOUND": boolean,         // true if object does not exist
      "COMPONENTS": [              // For TABL/STRU
        {
          "FIELD": "string",
          "KEY": boolean,
          "TYPE": "string",
          "LENGTH": number,
          "DATAELEMENT": "string",
          "DESCRIPTION": "string"
        }
      ]
    }
  ],
  "SUMMARY": {
    "TOTAL": number,
    "BY_TYPE": ["string"]
  },
  "ERROR": "string"
}
```

---

## Error Handling

| Error | Message |
|-------|---------|
| Object not found | `Object not found: ZCL_NONEXISTENT` |
| Invalid object type | `Unsupported object type: INVALID` |

### Error Output

```
❌ Object not found: ZCL_NONEXISTENT
```

---

## Object Type Detection

### Auto-Detection Rules

| Object Name Pattern | Default Type |
|---------------------|--------------|
| `ZCL_*` or `zcl_*` | CLAS (Class) |
| `ZIF_*` or `zif_*` | INTF (Interface) |
| `ZTY_*` or `zty_*` | DTEL (Data Element) |
| `ZS__*` or `zs__*` | DTEL (Data Element) |
| Other `Z*` | CLAS (default fallback) |
| Source include (len >= 32) | PROG (treat as program) |

### Supported Object Types

| Type Code | Type Text | Description |
|-----------|-----------|-------------|
| `CLAS` | Class | Global ABAP class |
| `INTF` | Interface | Global interface |
| `TABL` | Table | Database table |
| `STRU` | Structure | Structure type |
| `DTEL` | Data Element | Data element/domain type |
| `TTYP` | Table Type | Table type definition |
| `DDLS` | CDS View | CDS View/Entity definition |
| `STOB` | Structured Object | CDS-generated structured object |
| `PROG` | Program | ABAP program/include source |

---

## Example

```bash
# View a class definition (public section)
abapgit-agent view --objects ZCL_ABGAGT_AGENT

# View FULL class source — clean, all sections (for reading / AI analysis)
abapgit-agent view --objects ZCL_ABGAGT_AGENT --full

# View FULL class source with line numbers (for setting breakpoints)
abapgit-agent view --objects ZCL_ABGAGT_AGENT --full --lines

# View interface
abapgit-agent view --objects ZIF_ABGAGT_COMMAND

# View table structure
abapgit-agent view --objects SFLIGHT --type TABL

# View data element
abapgit-agent view --objects S_CARR_ID --type DTEL

# View CDS view definition
abapgit-agent view --objects ZC_MY_CDS_VIEW --type DDLS

# View source include (from where command output)
abapgit-agent view --objects ZCL_ABGAGT_COMMAND_UNIT=======CM007

# View method implementation
abapgit-agent view --objects ZCL_ABGAGT_AGENT=============CM012

# View unit test class
abapgit-agent view --objects ZCL_MY_CLASS=============CCAU

# View program source
abapgit-agent view --objects ZMY_PROGRAM --type PROG

# View multiple objects
abapgit-agent view --objects ZCL_CONFIG,ZIF_LOGGER,ZCL_UTILS

# JSON for programmatic use
abapgit-agent view --objects ZCL_MY_CLASS --json

# Lowercase support
abapgit-agent view --objects zcl_my_class
abapgit-agent view --objects sflight --type tabl
```

---

## Implementation

### ABAP Tables Used

| Table | Purpose |
|-------|---------|
| **TADIR** | Object directory (devclass, object type) |
| **SEOCLASS** | Class/interface metadata |
| **SEOCOMPODF** | Component definitions |
| **DD02L** | Table/structure definitions |
| **DD03L** | Table/structure fields |
| **DD04L** | Data element definitions |
| **DD40L** | Table type definitions |
| **DDDDLSRC02BT** | STOB to DDLS mapping |

### Class Source Retrieval (CLAS)

```abap
" Get class info from TADIR
SELECT SINGLE obj_name, devclass FROM tadir
  INTO (lv_obj_name, lv_devclass)
  WHERE obj_name = iv_name
    AND object = 'CLAS'.

" Get public section source
CALL METHOD cl_oo_classname_service=>get_pubsec_name
  EXPORTING clsname = lv_name
  RECEIVING result = lv_prog.

READ REPORT lv_prog INTO lt_source.
```

### Interface Source Retrieval (INTF)

```abap
" Get interface info from TADIR
SELECT SINGLE obj_name, devclass FROM tadir
  INTO (lv_obj_name, lv_devclass)
  WHERE obj_name = iv_name
    AND object = 'INTF'.

" Get interface section source
CALL METHOD cl_oo_classname_service=>get_intfsec_name
  EXPORTING clsname = lv_name
  RECEIVING result = lv_prog.

READ REPORT lv_prog INTO lt_source.
```

### Table/Structure Retrieval (TABL/STRU)

```abap
" Get fields from DD03L
SELECT fieldname, keyflag AS key, datatype AS type, leng AS length,
       rollname AS dataelement, ddtext AS description
  FROM dd03l
  INTO TABLE lt_components
  WHERE tabname = iv_name
    AND as4local = 'A'
  ORDER BY position.
```

### Data Element Retrieval (DTEL)

```abap
" Get domain info from DD04V
SELECT SINGLE rollname, ddtext, datatype, leng, decimals
  FROM dd04v
  INTO (lv_domain, lv_desc, lv_type, lv_len, lv_decimals)
  WHERE rollname = iv_name.
```

### Table Type Retrieval (TTYP)

```abap
" Get TTYP details from DD40L
SELECT SINGLE rowtype accessmode keydef FROM dd40l
  INTO (lv_linetype, lv_tabprottype, lv_keydef)
  WHERE typename = iv_name
    AND as4local = 'A'.

" Convert codes to text:
" - Access mode: T=STANDARD, S=SORTED, H=HASHED
" - Key definition: D=WITH KEY, N=NO KEY
```

### CDS View Retrieval (DDLS)

```abap
" Use DDL handler to read CDS view source
lo_handler = cl_dd_ddl_handler_factory=>create( ).

" First try to read inactive version (get_state = 'M')
TRY.
    lo_handler->read(
      EXPORTING
        name       = lv_ddls_name
        get_state  = 'M'
      IMPORTING
        ddddlsrcv_wa = ls_ddlsrcv ).

    IF ls_ddlsrcv-source IS NOT INITIAL.
      lv_found = abap_true.
    ENDIF.

  CATCH cx_dd_ddl_check.
    " Ignore - will try active version
ENDTRY.

" If no inactive version, try active version
IF lv_found = abap_false.
  TRY.
      lo_handler->read(
        EXPORTING
          name       = lv_ddls_name
          get_state  = 'A'
        IMPORTING
          ddddlsrcv_wa = ls_ddlsrcv ).
    CATCH cx_dd_ddl_check.
      " Not found
  ENDTRY.
ENDIF.

" Source code is in ls_ddlsrcv-source
```

### Structured Object Retrieval (STOB)

STOB objects are automatically generated by CDS views. The viewer finds the corresponding DDLS name using the `DDDDLSRC02BT` table and delegates to the DDLS viewer.

```abap
" Find corresponding DDLS name from DDDDLSRC02BT
" STRUCOBJN = STOB name, DDLNAME = corresponding DDLS source
SELECT SINGLE ddlname FROM ddddlsrc02bt
  INTO lv_ddls_name
  WHERE strucobjn = lv_obj_name
    AND as4local = 'A'.

IF sy-subrc <> 0.
  " Try inactive version
  SELECT SINGLE ddlname FROM ddddlsrc02bt
    INTO lv_ddls_name
    WHERE strucobjn = lv_obj_name
      AND as4local = 'M'.
ENDIF.

" Delegate to DDLS viewer to get CDS view source
IF lv_ddls_name IS NOT INITIAL.
  lo_factory = zcl_abgagt_viewer_factory=>get_instance( ).
  lo_viewer = lo_factory->get_viewer( 'DDLS' ).
  ls_ddls_info = lo_viewer->get_info( CONV string( lv_ddls_name ) ).

  " Copy source from DDLS viewer result
  rs_info-source = ls_ddls_info-source.
ENDIF.
```

### Source Include Detection (Auto-detect from Where Command)

When the object name looks like a source code include (from `where` command output), the view command attempts to read it directly as a program:

```abap
" Auto-detect source include names (from where command output)
" Examples: ZCL_CLASS=============CM007, ZCL_CLASS=============CCAU
DATA lv_name_len TYPE i.
lv_name_len = strlen( iv_name ).

" If name looks like a source include (>= 32 chars), try to read directly
IF lv_name_len >= 32.
  READ REPORT iv_name INTO lt_source.
  IF sy-subrc = 0.
    " Success - this is a source include or program
    " Return as PROG type
    rs_info-name = iv_name.
    rs_info-type = 'PROG'.
    rs_info-type_text = 'Program'.
    CONCATENATE LINES OF lt_source INTO rs_info-source
      SEPARATED BY cl_abap_char_utilities=>newline.
    RETURN.
  ENDIF.
ENDIF.
```

### Program Source Retrieval (PROG)

For explicit PROG type requests or detected source includes:

```abap
" Program Source Retrieval (PROG)
" Source includes from where command can be read directly

READ REPORT iv_name INTO lt_source.
IF sy-subrc = 0.
  rs_info-name = iv_name.
  rs_info-type = 'PROG'.
  rs_info-type_text = 'Program'.
  rs_info-description = |Program { iv_name }|.
  CONCATENATE LINES OF lt_source INTO rs_info-source
    SEPARATED BY cl_abap_char_utilities=>newline.
ENDIF.
```

### Include Name Patterns

Source includes are identified by their name pattern (returned by `where` command):

| Include Type | Example | Description |
|-------------|---------|-------------|
| Method | `ZCL_CLASS=============CM007` | Method implementation (35 chars) |
| Test Class | `ZCL_CLASS=============CCAU` | Unit test class (34 chars) |
| Public Section | `ZCL_CLASS=============CU` | Public section (32 chars) |
| Protected | `ZCL_CLASS=============CO` | Protected section (32 chars) |
| Private | `ZCL_CLASS=============CP` | Private section (32 chars) |
| Local Impl | `ZCL_CLASS=============CCIMP` | Local implementations (34 chars) |
| Local Def | `ZCL_CLASS=============CCDEF` | Local definitions (34 chars) |
| Interface | `ZIF_INTERFACE============IU` | Interface section (32 chars) |

The class name portion is always padded to 30 characters with `=` signs.
