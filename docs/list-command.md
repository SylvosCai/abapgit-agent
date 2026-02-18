# list Command Requirements

## Overview

List ABAP objects in a package with filtering and pagination capabilities. This command provides a flat list of objects with support for filtering by type, name pattern, and pagination.

## Command

```bash
# List all objects in a package
abapgit-agent list --package $ZMY_PACKAGE

# Filter by object type
abapgit-agent list --package $ZMY_PACKAGE --type CLAS,INTF

# Filter by name pattern
abapgit-agent list --package $ZMY_PACKAGE --name ZCL_*

# Limit results
abapgit-agent list --package $ZMY_PACKAGE --limit 50

# Paginate results
abapgit-agent list --package $ZMY_PACKAGE --offset 100 --limit 50

# JSON output for scripting
abapgit-agent list --package $ZMY_PACKAGE --json
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Package must exist in the ABAP system

## Parameters

| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `--package` | Yes | - | Package name (e.g., `$ZMY_PACKAGE`, `ZMY_PACKAGE`) |
| `--type` | No | All types | Comma-separated object types (e.g., `CLAS,INTF,PROG`) |
| `--name` | No | - | Name pattern with wildcard support (e.g., `ZCL_*`) |
| `--limit` | No | 100 | Maximum objects to return (max: 1000) |
| `--offset` | No | 0 | Number of objects to skip |
| `--json` | No | false | Output raw JSON only |

---

## Tasks

### 1. Validate Parameters

- `--package` must be specified
- Package name must be valid (1-30 characters)
- `--limit` must be between 1 and 1000

### 2. Load Configuration

Read `.abapGitAgent` for credentials

### 3. Fetch CSRF Token

```bash
GET /health (with X-CSRF-Token: fetch)
```

### 4. Make List Request

**Endpoint:** `POST /list`

**Request Body:**
```json
{
  "package": "$ZMY_PACKAGE",
  "type": "CLAS,INTF",
  "name": "ZCL_*",
  "limit": 100,
  "offset": 0
}
```

### 5. Display Results

---

## Output

### Human-Readable Output

```
📋 Objects in $ZMY_PACKAGE (Total: 15)

  CLAS (5)
    ZCL_CLASS1
    ZCL_CLASS2
    ZCL_CLASS3
    ZCL_CLASS4
    ZCL_CLASS5

  INTF (2)
    ZIF_INTERFACE1
    ZIF_INTERFACE2

  PROG (3)
    ZPROG1
    ZPROG2
    ZPROG3

  TABL (5)
    ZTABLE1
    ZTABLE2
    ZTABLE3
    ZTABLE4
    ZTABLE5
```

### With Type Filter

```
📋 Objects in $ZMY_PACKAGE (CLAS only, Total: 5)

  CLAS (5)
    ZCL_CLASS1
    ZCL_CLASS2
    ZCL_CLASS3
    ZCL_CLASS4
    ZCL_CLASS5
```

### JSON Output

```json
{
  "SUCCESS": true,
  "COMMAND": "LIST",
  "PACKAGE": "$ZMY_PACKAGE",
  "TOTAL": 15,
  "LIMIT": 100,
  "OFFSET": 0,
  "OBJECTS": [
    { "TYPE": "CLAS", "NAME": "ZCL_CLASS1" },
    { "TYPE": "CLAS", "NAME": "ZCL_CLASS2" },
    { "TYPE": "CLAS", "NAME": "ZCL_CLASS3" },
    { "TYPE": "CLAS", "NAME": "ZCL_CLASS4" },
    { "TYPE": "CLAS", "NAME": "ZCL_CLASS5" },
    { "TYPE": "INTF", "NAME": "ZIF_INTERFACE1" },
    { "TYPE": "INTF", "NAME": "ZIF_INTERFACE2" },
    { "TYPE": "PROG", "NAME": "ZPROG1" },
    { "TYPE": "PROG", "NAME": "ZPROG2" },
    { "TYPE": "PROG", "NAME": "ZPROG3" },
    { "TYPE": "TABL", "NAME": "ZTABLE1" },
    { "TYPE": "TABL", "NAME": "ZTABLE2" },
    { "TYPE": "TABL", "NAME": "ZTABLE3" },
    { "TYPE": "TABL", "NAME": "ZTABLE4" },
    { "TYPE": "TABL", "NAME": "ZTABLE5" }
  ],
  "BY_TYPE": [
    { "TYPE": "CLAS", "COUNT": 5 },
    { "TYPE": "INTF", "COUNT": 2 },
    { "TYPE": "PROG", "COUNT": 3 },
    { "TYPE": "TABL", "COUNT": 5 }
  ],
  "ERROR": ""
}
```

**Response Fields:**

| Field | Type | Description |
|-------|------|-------------|
| `SUCCESS` | boolean | Whether the request succeeded |
| `COMMAND` | string | Command name ("LIST") |
| `PACKAGE` | string | Package name |
| `TOTAL` | number | Total objects matching filter |
| `LIMIT` | number | Requested limit |
| `OFFSET` | number | Requested offset |
| `OBJECTS` | array | List of objects [{TYPE, NAME}] |
| `BY_TYPE` | array | Object counts by type [{TYPE, COUNT}] |
| `ERROR` | string | Error message (empty if success) |

---

## Error Handling

| Error | Message |
|-------|---------|
| Package not specified | `Package parameter is required` |
| Package not found | `Package <name> does not exist` |
| Invalid type | `Invalid object type: <type>` |
| Limit too high | `Limit value too high (max: 1000)` |

### Error Output

```
❌ Package not found: $ZNONEXISTENT

Error: Package $ZNONEXISTENT does not exist in the system.
```

---

## Example

```bash
# List all objects
abapgit-agent list --package $ZMY_PACKAGE

# Filter by type
abapgit-agent list --package $ZMY_PACKAGE --type CLAS,INTF

# Filter by name pattern
abapgit-agent list --package $ZMY_PACKAGE --name ZCL_*

# Paginate
abapgit-agent list --package $ZMY_PACKAGE --limit 50 --offset 50

# JSON for scripting
abapgit-agent list --package $ZMY_PACKAGE --json > objects.json

# CI/CD: Count classes
CLASS_COUNT=$(abapgit-agent list --package $ZMY_PACKAGE --type CLAS --json | jq '.TOTAL')
echo "Package has $CLASS_COUNT classes"
```

---

## Implementation

### ABAP Tables Used

| Table | Purpose |
|-------|---------|
| **TDEVC** | Package definitions (validate package exists) |
| **TADIR** | Object directory (fetch objects in package) |

### Supported Object Types

| Type | Description |
|------|-------------|
| CLAS | Class |
| INTF | Interface |
| PROG | Program |
| FUGR | Function Group |
| TABL | Table |
| STRU | Structure |
| DTEL | Data Element |
| TTYP | Table Type |
| DDLS | CDS View |
| DDLX | CDS View Entity |

### Query Logic

```abap
" Validate package exists
SELECT SINGLE devclass FROM tdevc
  INTO lv_package
 WHERE devclass = iv_package.

" Get objects with filters
SELECT object obj_name FROM tadir
  INTO TABLE lt_objects
 WHERE devclass = iv_package
   AND object IN lt_types
   AND obj_name LIKE lv_name_pattern
 ORDER BY object obj_name
 LIMIT iv_limit
 OFFSET iv_offset.

" Get counts by type (for summary)
SELECT object COUNT(*) AS count FROM tadir
  INTO TABLE lt_counts
 WHERE devclass = iv_package
   AND object IN lt_types
 GROUP BY object.
```

### Files to Create

| File | Description |
|------|-------------|
| `zcl_abgagt_command_list.clas.abap` | Command implementation |
| `zcl_abgagt_command_list.clas.xml` | Class metadata |
| `zcl_abgagt_resource_list.clas.abap` | REST resource handler |
| `zcl_abgagt_resource_list.clas.xml` | Resource metadata |

### Files to Modify

| File | Description |
|------|-------------|
| `zif_abgagt_command.intf.abap` | Add LIST constant |
| `zcl_abgagt_cmd_factory.clas.abap` | Add LIST command mapping |
