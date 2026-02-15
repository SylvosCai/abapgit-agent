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

# Lowercase names and types are supported
abapgit-agent view --objects zcl_my_class --type clas

# Output as JSON (for scripting/AI processing)
abapgit-agent view --objects ZCL_MY_CLASS --json
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Object must exist in the ABAP system

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--objects` | Yes | Comma-separated list of object names (e.g., `ZCL_MY_CLASS,ZIF_MY_INTERFACE`) |
| `--type` | No | Object type for all objects (CLAS, INTF, TABL, STRU, DTEL). Auto-detected from name prefix if not specified |
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

### Class Definition (CLAS)

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
      "TYPE": "CLAS|INTF|TABL|STRU|DTEL",
      "TYPE_TEXT": "string",
      "DESCRIPTION": "string",
      "DOMAIN": "string",           // For DTEL
      "DOMAIN_TYPE": "string",      // For DTEL
      "DOMAIN_LENGTH": number,      // For DTEL
      "DOMAIN_DECIMALS": number,    // For DTEL
      "SOURCE": "string",           // Full ABAP source (CLAS/INTF)
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

### Supported Object Types

| Type Code | Type Text | Description |
|-----------|-----------|-------------|
| `CLAS` | Class | Global ABAP class |
| `INTF` | Interface | Global interface |
| `TABL` | Table | Database table |
| `STRU` | Structure | Structure type |
| `DTEL` | Data Element | Data element/domain type |

---

## Example

```bash
# View a class definition
abapgit-agent view --objects ZCL_ABGAGT_AGENT

# View interface
abapgit-agent view --objects ZIF_ABGAGT_COMMAND

# View table structure
abapgit-agent view --objects SFLIGHT --type TABL

# View data element
abapgit-agent view --objects S_CARR_ID --type DTEL

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
