# view Command Requirements

## Overview

View ABAP object source code directly from the ABAP system. This command retrieves and displays source code for objects that may not exist locally in your git repository, enabling developers to understand class definitions, method signatures, structure components, and data element types without pulling the entire object.

## Use Cases

- **Understand unfamiliar code**: View a class definition from a dependency package
- **Check method signatures**: See method parameters and return types before calling
- **Inspect data structures**: View table or structure field definitions
- **Review interface definitions**: Check interface methods and constants
- **Quick reference**: Look up definitions without opening SE80 or ADT

## Command

```bash
# View single object (auto-detect type from name prefix)
abapgit-agent view --objects ZCL_MY_CLASS
abapgit-agent view --objects ZIF_MY_INTERFACE
abapgit-agent view --objects ZMY_TABLE

# View with explicit type
abapgit-agent view --objects ZCL_MY_CLASS --type CLAS
abapgit-agent view --objects ZIF_MY_INT --type INTF
abapgit-agent view --objects ZMY_STRUCT --type STRU
abapgit-agent view --objects ZMY_TABLE --type TABL
abapgit-agent view --objects ZMY_DTEL --type DTEL

# View multiple objects
abapgit-agent view --objects ZCL_CLASS1,ZCL_CLASS2,ZIF_INTERFACE1

# Output as JSON (for scripting/AI processing)
abapgit-agent view --objects ZCL_MY_CLASS --json

# Show only method definitions (classes)
abapgit-agent view --objects ZCL_MY_CLASS --type CLAS --methods
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Object must exist in the ABAP system

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--objects` | Yes | Comma-separated list of object names (e.g., `ZCL_MY_CLASS,ZIF_MY_INTERFACE`) |
| `--type` | No | Object type for all objects (CLAS, INTF, TABL, STRU, DTEL, FUGR, PROG). Auto-detected from name prefix if not specified |
| `--methods` | No | For CLAS/INTF: show only method definitions, skip full source |
| `--json` | No | Output raw JSON only (for scripting) |
| `--include-docs` | No | Include documentation/comments in output (default: false for source-only) |

---

## Tasks

### 1. Validate Parameters

- `--objects` must be specified
- Validate object names (1-40 characters, valid SAP naming)
- If `--type` specified, validate against supported types

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
  "type": "CLAS",
  "include_methods": true,
  "include_docs": false
}
```

### 5. Display Results

---

## Output

### Class Definition (Default)

```
📖 Viewing Object: ZCL_MY_CLASS (CLAS)

CLASS ZCL_MY_CLASS DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_amc_message.
    ALIASES:
      mo_cfg FOR if_bali_cfg_setter~set_option.

    METHODS:
      constructor
        IMPORTING
          !iv_name TYPE string OPTIONAL,
      get_config
        RETURNING
          VALUE(ro_result) TYPE REF TO if_bali_cfg_setter,
      set_config
        IMPORTING
          ! TO if_bali_cfg_setter.

io_config TYPE REF  PROTECTED SECTION.
    DATA mo_config TYPE REF TO if_bali_cfg_setter.

  PRIVATE SECTION.
    DATA mv_initialized TYPE abap_bool.
ENDCLASS.
```

### Class with Methods Only

```
📖 Viewing: ZCL_MY_CLASS (CLAS) - Methods Only

METHODS constructor
  IMPORTING !iv_name TYPE string OPTIONAL.

METHODS get_config
  RETURNING VALUE(ro_result) TYPE REF TO if_bali_cfg_setter.

METHODS set_config
  IMPORTING !io_config TYPE REF TO if_bali_cfg_setter
  RETURNING VALUE(rv_result) TYPE abap_bool.
```

### Interface Definition

```
📖 Viewing Object: ZIF_MY_INTERFACE (INTF)

INTERFACE zif_my_interface PUBLIC.

  CONSTANTS:
    gc_version TYPE string VALUE '1.0'.

  METHODS:
    process
      IMPORTING
        !iv_data TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool,
    get_status
      RETURNING
        VALUE(rv_status) TYPE string.

  EVENTS:
    processed.

ENDINTERFACE.
```

### Structure Definition

```
📖 Viewing Object: ZMY_STRUCT (STRU)

STRUCTURE ZMY_STRUCT:
┌─────────────────────────────────────────────────────────────┐
│ Component    │ Type             │ Description              │
├─────────────────────────────────────────────────────────────┤
│ MANDT        │ MANDT            │ Client                   │
│ ID           │ CHAR(10)         │ Record ID                │
│ CREATED_AT   │ TIMESTAMP        │ Creation Timestamp       │
│ CREATED_BY   │ SYUNAME          │ Created By               │
│ UPDATED_AT   │ TIMESTAMP        │ Last Updated             │
│ AMOUNT       │ CURR(15,2)       │ Amount in source currency│
│ CURRENCY     │ CUKY             │ Currency Code            │
└─────────────────────────────────────────────────────────────┘
```

### Table Definition

```
📖 Viewing Object: ZMY_TABLE (TABL)

TABLE ZMY_TABLE:
┌─────────────────────────────────────────────────────────────┐
│ Field       │ Key │ Type       │ Length │ Description       │
├─────────────────────────────────────────────────────────────┤
│ MANDT       │ X   │ MANDT      │   3    │ Client            │
│ ID          │ X   │ CHAR(10)   │  10    │ Primary Key       │
│ NAME        │     │ CHAR(50)   │  50    │ Object Name       │
│ STATUS      │     │ CHAR(1)    │   1    │ Active/Inactive   │
│ CREATED_AT  │     │ TIMESTAMP   │        │ Creation Date     │
└─────────────────────────────────────────────────────────────┘
Table Type: TRANSP
Delivery Class: A
```

### Data Element Definition

```
📖 Viewing Object: ZMY_DTEL (DTEL)

DATA ELEMENT ZMY_DTEL:
┌─────────────────────────────────────────────────────────────┐
│ Property          │ Value                                 │
├─────────────────────────────────────────────────────────────┤
│ Data Type         │ CHAR                                  │
│ Length            │ 10                                    │
│ Output Length     │ 10                                    │
│ Description       │ My Custom Field                       │
│ Domain            │ ZMY_DOMAIN                            │
└─────────────────────────────────────────────────────────────┘
```

### Multiple Objects

```
📖 Viewing 3 Objects

1️⃣  ZCL_MY_CLASS (CLAS)
    └─ Class with 3 methods, 2 interfaces

2️⃣  ZIF_MY_INTERFACE (INTF)
    └─ Interface with 2 methods, 1 event

3️⃣  ZMY_TABLE (TABL)
    └─ Table with 5 fields, 1 key
```

### JSON Output (Pure Scripting)

```json
{
  "success": "X",
  "command": "VIEW",
  "message": "Retrieved 3 object(s)",
  "objects": [
    {
      "name": "ZCL_MY_CLASS",
      "type": "CLAS",
      "type_text": "Class",
      "description": "My Custom Configuration Class",
      "source": "CLASS zcl_my_class DEFINITION PUBLIC.\n  ...",
      "methods": [
        {
          "name": "CONSTRUCTOR",
          "visibility": "PUBLIC",
          "parameters": [
            { "name": "IV_NAME", "type": "STRING", "pass": "IMPORTING", "optional": "X" }
          ],
          "return": null
        },
        {
          "name": "GET_CONFIG",
          "visibility": "PUBLIC",
          "parameters": [],
          "return": { "name": "RO_RESULT", "type": "REF TO IF_BALI_CFG_SETTER" }
        }
      ]
    }
  ],
  "summary": {
    "total": 3,
    "by_type": {
      "CLAS": 1,
      "INTF": 1,
      "TABL": 1
    }
  }
}
```

---

## Response Structure

### JSON Response Schema

```json
{
  "success": "X",
  "command": "VIEW",
  "message": "Retrieved N object(s)",
  "objects": [
    {
      "name": "STRING",
      "type": "CLAS|INTF|TABL|STRU|DTEL|FUGR|PROG",
      "type_text": "Class|Interface|Table|Structure|Data Element|Function Group|Program",
      "description": "Object description from SEOCLASS/SEOMEMBER/etc",
      "source": "Full ABAP source code (if requested)",
      "definition": "Parsed definition block (for CLAS/INTF)",
      "methods": [
        {
          "name": "METHOD_NAME",
          "visibility": "PUBLIC|PROTECTED|PRIVATE",
          "parameters": [
            {
              "name": "PARAM_NAME",
              "type": "ABAP Type",
              "pass": "IMPORTING|EXPORTING|CHANGING|RETURNING",
              "optional": true
            }
          ],
          "return": {
            "name": "RETURN_VALUE",
            "type": "ABAP Type"
          }
        }
      ],
      "components": [  // For TABL/STRU
        {
          "fieldname": "FIELD_NAME",
          "type": "TYPE_NAME",
          "key": true,
          "description": "Field description"
        }
      ]
    }
  ],
  "summary": {
    "total": 1,
    "by_type": {
      "CLAS": 1
    }
  }
}
```

---

## Error Handling

| Error | Message |
|-------|---------|
| Object not found | `Object not found: ZCL_NONEXISTENT (CLAS)` |
| Invalid object type | `Invalid object type: INVALID` |
| Access denied | `Access denied to object ZCL_MY_CLASS` |
| Multiple types for object | `Object ZCL_MY exists as both CLAS and INTf - specify --type` |

### Error Output

```
❌ Object not found: ZCL_NONEXISTENT

Error: Object ZCL_NONEXISTENT (CLAS) does not exist in the system.
       Check the object name and type.
```

```
❌ Ambiguous object name: ZCL_MY

Object ZCL_MY matches multiple types:
  - CLAS (ZCL_MY)
  - INTf (ZIF_MY)

Use --type to specify the correct type:
  abapgit-agent view --objects ZCL_MY --type CLAS
  abapgit-agent view --objects ZCL_MY --type INTF
```

---

## Object Type Detection

### Auto-Detection Rules

| Object Name Pattern | Default Type |
|---------------------|--------------|
| `ZCL_*` | CLAS (Class) |
| `ZIF_*` | INTF (Interface) |
| `ZTY_*` | DTEL (Data Element) |
| `ZMY_TABLE*` | TABL (Table) |
| `ZMY_STRUCT*` | STRU (Structure) |
| `ZMY_FGR*` | FUGR (Function Group) |
| `ZMY_PROG*` | PROG (Program) |
| Other `Z*` | Unknown - requires --type |

### Supported Object Types

| Type Code | Type Text | Description |
|-----------|-----------|-------------|
| `CLAS` | Class | Global ABAP class (ZCL_*) |
| `INTF` | Interface | Global interface (ZIF_*) |
| `TABL` | Table | Database table |
| `STRU` | Structure | Structure type |
| `DTEL` | Data Element | Data element/domain type |
| `FUGR` | Function Group | Function group |
| `PROG` | Program | Report/program |
| `ENQU` | Enqueue | Lock object |
| `TYPE` | Type Pool | Type pool |

---

## Example

```bash
# View a class definition
abapgit-agent view --objects ZCL_ABGAGT_AGENT

# View interface with methods only
abapgit-agent view --objects ZIF_ABGAGT_AGENT --methods

# View table structure
abapgit-agent view --objects ZABAPGIT_Tadir --type TABL

# View multiple objects
abapgit-agent view --objects ZCL_CONFIG,ZIF_LOGGER,ZCL_UTILS --type CLAS

# JSON for programmatic use
abapgit-agent view --objects ZCL_MY_CLASS --json | jq '.objects[0].methods'

# AI-friendly output (default includes metadata)
abapgit-agent view --objects ZCL_MY_CLASS
```

---

## AI-Friendly Metadata Format

### Inline Metadata Block (Default Mode)

```
<!-- AI_METADATA_START -->
{"objects":[{"name":"ZCL_MY_CLASS","type":"CLAS","methods":3,"description":"My Class"}],"total":1}
<!-- AI_METADATA_END -->
```

### Metadata Schema

| Field | Type | Description |
|-------|------|-------------|
| `objects` | array | List of viewed objects |
| `objects[].name` | string | Object name |
| `objects[].type` | string | Object type (CLAS, INTF, etc.) |
| `objects[].methods` | number | Number of methods (for CLAS/INTF) |
| `objects[].fields` | number | Number of fields (for TABL/STRU) |
| `objects[].description` | string | Object description |
| `total` | number | Total objects viewed |

---

## Implementation

### ABAP Tables Used

| Table | Purpose |
|-------|---------|
| **SEOCLASS** | Class definitions (CLAS) |
| **SEOMETADATAPO** | Class metadata |
| **SEOKEY** | Object key mapping |
| **SEOCOMPT** | Component definitions (methods, attributes) |
| **TADIR** | Object directory |
| **DD02L** | Table definitions |
| **DD03L** | Table fields |
| **DD04L** | Data element definitions |

### Class Source Retrieval

```abap
" Get class definition from source (SEOCLASS + SEOCOMPT)
SELECT SINGLE clsname, descript
  FROM seoclass
  INTO @DATA(ls_class)
  WHERE clsname = @iv_name.

" Get method definitions
SELECT cmpname, descript, expos, level
  FROM seocompodf
  INTO TABLE @DATA(lt_methods)
  WHERE clsname = @iv_name
    AND expos IN ('0','1','2')  " Public, Protected, Private
  ORDER BY expos, cmpname.

" Get source code (if needed)
SELECT SINGLE srdoc
  FROM seocsource
  INTO @DATA(lv_source)
  WHERE clsname = @iv_name.
```

### Interface Source Retrieval

```abap
" Get interface definition
SELECT SINGLE clsname, descript
  FROM seoclass
  INTO @DATA(ls_intf)
  WHERE clsname = @iv_name
    AND clstype = '2'.  " Interface

" Get interface methods
SELECT cmpname, descript, type
  FROM seocompodf
  INTO TABLE @DATA(lt_methods)
  WHERE clsname = @iv_name.
```

### Table/Structure Retrieval

```abap
" Get table/structure definition
SELECT SINGLE tabname, ddtext, as4user
  FROM dd02l
  INTO @DATA(ls_table)
  WHERE tabname = @iv_name.

" Get table fields
SELECT fieldname, position, keyflag, datatype, leng, ddtext
  FROM dd03l
  INTO TABLE @lt_fields
  WHERE tabname = @iv_name
    AND as4local = 'A'
  ORDER BY position.
```

### Data Element Retrieval

```abap
" Get data element definition
SELECT SINGLE rollname, ddtext, datatype, leng, outputlen
  FROM dd04l
  INTO @DATA(ls_dtel)
  WHERE rollname = @iv_name.
```

### Method Signature Parsing

```abap
" Methods contain signature in SEOCOMPT
" PABAP Type in field 'type'
" Parameter name in field 'pabap'

" For method parameters, query SEOSUBCODF
SELECT fname, type, sconame, parclsn, defaultvalue
  FROM seosubcodf
  INTO TABLE @DATA(lt_params)
  WHERE clsname = @iv_class
    AND cmpname = @iv_method.
```
