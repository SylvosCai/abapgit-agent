# where Command

## Overview

Find where-used-list for ABAP objects (classes, interfaces, programs). This command searches for references to a given object across the ABAP system and returns the source code includes where the object is used.

This is particularly useful for:
- Understanding how a class/method is used in other places
- Finding usage (e.g., examples for API classes `CL_SUT_AUNIT_RUNNER`)
- Discovering which parameters to set when using a class
- Analyzing dependencies before making changes

**Workflow:** Use `where` to find where an object is used, then use `view` to examine the actual source code.

## Command

```bash
# Find where a class is used
abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER

# Find where an interface is used
abapgit-agent where --objects ZIF_ABGAGT_COMMAND

# Find where a program is used
abapgit-agent where --objects SAPLZWABGAGT --type PROG

# With object type explicitly specified
abapgit-agent where --objects ZCL_MY_CLASS --type CLAS
abapgit-agent where --objects ZMY_PROGRAM --type PROG

# Multiple objects
abapgit-agent where --objects ZCL_CLASS1,ZIF_INTERFACE1

# Limit results
abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --limit 50

# Paginate through large result sets
abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --offset 100
abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --limit 50 --offset 150

# JSON output
abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --json
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Object must exist in the ABAP system

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--objects` | Yes | Comma-separated list of object names to search |
| `--type` | No | Object type (CLAS, INTF, PROG). Auto-detected if not specified |
| `--limit` | No | Maximum number of results to return (default: 100, max: 500) |
| `--offset` | No | Number of results to skip for pagination (default: 0) |
| `--json` | No | Output raw JSON only (for scripting) |

---

## Output

### Basic Usage - Class Where-Used List

```
  Where-used list for 1 object(s)

  🔍 ZCL_SUT_AUNIT_RUNNER (CLAS)
     Found 3 reference(s):

  1. ZCL_ABGAGT_COMMAND_UNIT=======CM007 → CONSTRUCTOR (Class Method)
  2. ZCL_ABGAGT_REST_HANDLER======CM00E → RUN (Class Method)
  3. ZCL_ABGAGT_AGENT=============CM012 → EXECUTE (Class Method)
```

### Interface Where-Used List

```
  Where-used list for 1 object(s)

  🔍 ZIF_ABGAGT_COMMAND (INTF)
     Found 45 reference(s):

  1. ZCL_ABGAGT_CMD_FACTORY=========IU (Interface Section)
  2. ZCL_ABGAGT_COMMAND_PULL=======CU (Public Section)
  3. ZCL_ABGAGT_COMMAND_INSPECT====CU (Public Section)
...
```

### No References Found

```
  Where-used list for 1 object(s)

  ❌ ZCL_MY_CLASS (CLAS)
     No references found
```

### Multiple Objects

```
  Where-used list for 2 object(s)

  🔍 ZIF_ABGAGT_COMMAND (INTF)
     Found 3 reference(s):

  1. ZCL_ABGAGT_CMD_FACTORY=========IU (Interface Section)
...

  🔍 ZCL_ABGAGT_AGENT (CLAS)
     Found 7 reference(s):

  1. ZCL_ABGAGT_REST_HANDLER======CM001 → CONSTRUCTOR (Class Method)
...
```

### JSON Output

```json
{
  "SUCCESS": true,
  "COMMAND": "WHERE",
  "MESSAGE": "Found 347 references",
  "OBJECTS": [
    {
      "NAME": "ZCL_SUT_AUNIT_RUNNER",
      "TYPE": "CLAS",
      "REFERENCES": [
        {
          "OBJECT": "ZCL_ABGAGT_COMMAND_UNIT",
          "OBJECT_TYPE": "CLAS",
          "INCLUDE_NAME": "ZCL_ABGAGT_COMMAND_UNIT=======CM007",
          "METHOD_NAME": "CONSTRUCTOR",
          "INCLUDE_TYPE": "Class Method"
        }
      ],
      "COUNT": 100
    }
  ],
  "SUMMARY": {
    "TOTAL_OBJECTS": 1,
    "TOTAL_REFERENCES": 347
  },
  "PAGINATION": {
    "LIMIT": 100,
    "OFFSET": 0,
    "TOTAL": 347,
    "HAS_MORE": true,
    "NEXT_OFFSET": 100
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
  "COMMAND": "WHERE",
  "MESSAGE": "string",
  "OBJECTS": [
    {
      "NAME": "string",
      "TYPE": "CLAS|INTF|PROG",
      "REFERENCES": [
        {
          "OBJECT": "string",
          "OBJECT_TYPE": "string",
          "INCLUDE_NAME": "string",
          "METHOD_NAME": "string",
          "INCLUDE_TYPE": "string",
          "PACKAGE": "string"
        }
      ],
      "COUNT": number
    }
  ],
  "SUMMARY": {
    "TOTAL_OBJECTS": number,
    "TOTAL_REFERENCES": number
  },
  "PAGINATION": {
    "LIMIT": number,
    "OFFSET": number,
    "TOTAL": number,
    "HAS_MORE": boolean,
    "NEXT_OFFSET": number
  },
  "ERROR": "string"
}
```

### Field Mapping from AKB_WHERE_USED_LIST

| JSON Field | ABAP Field | Description |
|------------|------------|-------------|
| `OBJECT` | `OBJ_NAME` | Object using the reference |
| `OBJECT_TYPE` | `OBJ_TYPE` | Object type (CLAS, PROG) |
| `INCLUDE_NAME` | `SUB_NAME` | Source code include name |
| `METHOD_NAME` | (derived from TMDIR) | Method name for method includes |
| `INCLUDE_TYPE` | (derived from SUB_NAME) | Human-readable include type |
| `PACKAGE` | `APPL_PACKET` | Package where the using object resides |

### Include Type Mapping

The `INCLUDE_TYPE` field provides a human-readable description:

| Include Type | Description |
|-------------|-------------|
| `CM001-CM099`, `CM00A-CM99Z` | Class Method |
| `CCAU` | Unit Test |
| `CCIMP` | Local Implementations |
| `CCDEF` | Local Definitions |
| `CU` | Public Section |
| `CO` | Protected Section |
| `CP` | Private Section |
| `CI` | Local Interfaces |
| `CT` | Macros |
| `IU` | Interface Section |

---

## Key Behaviors

1. **Object Type Detection**: If `--type` is not specified, auto-detect from object name:
   - `ZCL_*` or `CL_*` → CLAS
   - `ZIF_*` or `IF_*` → INTF
   - `SAPL*` → PROG
   - Other → Queries TADIR, defaults to CLAS

2. **Method Name Extraction**: When the include is a method include (CM###), the method name is extracted from TMDIR based on the method index.

3. **Include Type**: Returns a human-readable description (e.g., "Class Method", "Public Section", "Unit Test").

4. **Limit Results**: Use `--limit` to restrict the number of references returned (default: 100, max: 500)

5. **Pagination**: Use `--offset` to skip results and paginate through large result sets. The response includes pagination metadata (`HAS_MORE`, `NEXT_OFFSET`) to help navigate through results.

   ```bash
   # First page
   abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --limit 100

   # Second page
   abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --offset 100

   # Third page
   abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --offset 200
   ```

---

## Error Handling

| Error | Message |
|-------|---------|
| Object not found | `Object not found: ZCL_NONEXISTENT` |
| Invalid object type | `Unsupported object type: <type>` |
| Access denied | `Access denied to where-used list for <object>` |
| No references found | `No references found` (displayed with ❌ icon) |

### Object Existence Check

Before calling `AKB_WHERE_USED_LIST`, the command verifies the object exists in TADIR:

```abap
" Check if object exists in TADIR
SELECT SINGLE object FROM tadir
  INTO lv_obj_type_check
  WHERE obj_name = iv_obj_name
    AND object = iv_obj_type.

IF sy-subrc <> 0.
  " Object not found - return error in response
  rs_info-error = |Object not found: { iv_obj_name }|.
  RETURN.
ENDIF.
```

### Error Response

When object doesn't exist, the response includes the error:

```json
{
  "SUCCESS": false,
  "COMMAND": "WHERE",
  "ERROR": "Object not found: ZCL_NONEXISTENT"
}
``` |

---

## Supported Object Types

| Type | Description | Where-Used Search Scope |
|------|-------------|------------------------|
| `CLAS` | Global ABAP class | Class, interface, program references |
| `INTF` | Global interface | Classes implementing the interface |
| `PROG` | Program | Programs that include/perform the program |

---

## Object Name Detection

### Auto-Detection Rules

| Object Name Pattern | Default Type |
|---------------------|--------------|
| `ZCL_*` or `zcl_*` | CLAS (Class) |
| `CL_*` or `cl_*` | CLAS (Class) |
| `ZIF_*` or `zif_*` | INTF (Interface) |
| `IF_*` or `if_*` | INTF (Interface) |
| `SAPL*` or `sapl*` | PROG (Program) |
| Other | Queries TADIR, defaults to CLAS |

---

## Implementation

### Function Module: AKB_WHERE_USED_LIST

This is the primary function module used to retrieve where-used information.

```abap
CALL FUNCTION 'AKB_WHERE_USED_LIST'
  EXPORTING
    obj_type = 'CLAS'              " trobjtype - Object type (CLAS, INTF, PROG, etc.)
    obj_name = 'ZCL_MY_CLASS'      " sobj_name - Object name
  IMPORTING
    references = lt_refs.          " Table of type AKB_EXCEPT_TYPE
```

### Result Table Structure (AKB_EXCEPT_TYPE)

| Field | Type | Description |
|-------|------|-------------|
| `OBJ_TYPE` | TROBJTYPE | Object type of the using object (CLAS, PROG) |
| `OBJ_NAME` | SOBJ_NAME | Name of the object using the reference |
| `SUB_NAME` | SOBJ_NAME | Source code include name |
| `APPL_PACKET` | DEVCLASS | Package |

### Include Name Patterns

The `SUB_NAME` field contains the source code include name. The class name portion (including padding) is **ALWAYS 30 characters**:

```
SUB_NAME = PAD(classname, 30, '=') + include_type
```

Examples:
- `ZCL_ABGAGT_AGENT=============CU` (30 chars + 2 = 32)
- `CL_GRCAUD_ACTION_NOTIFIER=====CU` (30 chars + 2 = 32)
- `ZCL_ABGAGT_COMMAND_UNIT=======CM007` (30 chars + 5 = 35)

---

## Notes

1. **Performance**: Where-used list can be slow for widely-used objects. Use `--limit` to restrict results.

2. **Cross-System**: Where-used list only searches the current ABAP system, not the entire SAP landscape.

3. **Inactive Objects**: Search includes both active and inactive objects.

4. **Method Filtering**: The output filters to only include CLAS and PROG types as referencing objects.
