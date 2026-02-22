# where Command Requirements

## Overview

Find where-used-list for ABAP objects (classes, interfaces, programs, methods, etc.). This command searches for references to a given object across the ABAP system and returns the source code includes where the object is used.

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

# Find where a method is used (specify containing class)
abapgit-agent where --objects ZCL_AGENT=>RUN --type CLAS

# Find where a program is used
abapgit-agent where --objects SAPLZWABGAGT --type PROG

# With object type explicitly specified
abapgit-agent where --objects ZCL_MY_CLASS --type CLAS
abapgit-agent where --objects ZMY_PROGRAM --type PROG

# Multiple objects
abapgit-agent where --objects ZCL_CLASS1,ZIF_INTERFACE1

# Limit results
abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --limit 50

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
| `--json` | No | Output raw JSON only (for scripting) |

---

## Tasks

### 1. Validate Parameters

- `--objects` must be specified
- Object type is optional - auto-detected from object name pattern if not provided
- Limit defaults to 100, maximum is 500

### 2. Load Configuration

Read `.abapGitAgent` for credentials

### 3. Fetch CSRF Token

```bash
GET /health (with X-CSRF-Token: fetch)
```

### 4. Make Where Request

**Endpoint:** `POST /where`

**Request Body:**
```json
{
  "objects": ["ZCL_SUT_AUNIT_RUNNER"],
  "type": "CLAS",
  "limit": 100
}
```

### 5. Display Results

---

## Output

### Basic Usage - Class Where-Used List

```
🔍 Where-used list for: ZCL_SUT_AUNIT_RUNNER (Class)

Found 3 references:

1️⃣  ZCL_ABGAGT_COMMAND_UNIT (Class)
    ├─ Include: ZCL_ABGAGT_COMMAND_UNIT=======CM007
    ├─ Method: CONSTRUCTOR
    └─ Type: Class Method

2️⃣  ZCL_ABGAGT_REST_HANDLER (Class)
    ├─ Include: ZCL_ABGAGT_REST_HANDLER======CM00E
    ├─ Method: RUN
    └─ Type: Class Method

3️⃣  ZCL_ABGAGT_AGENT (Class)
    ├─ Include: ZCL_ABGAGT_AGENT=============CM012
    ├─ Method: EXECUTE
    └─ Type: Class Method
```

### Interface Where-Used List

```
🔍 Where-used list for: ZIF_ABGAGT_COMMAND (Interface)

Found 45 references:

1️⃣  ZCL_ABGAGT_CMD_FACTORY (Class)
    ├─ Include: ZCL_ABGAGT_CMD_FACTORY=========IU
    └─ Type: Interface Section

2️⃣  ZCL_ABGAGT_COMMAND_PULL (Class)
    ├─ Include: ZCL_ABGAGT_COMMAND_PULL=======CU
    └─ Type: Public Section

3️⃣  ZCL_ABGAGT_COMMAND_INSPECT (Class)
    ├─ Include: ZCL_ABGAGT_COMMAND_INSPECT====CU
    └─ Type: Public Section
...
```

### Method Where-Used List

```
🔍 Where-used list for: ZCL_AGENT=>PULL (Method)

Found 2 references:

1️⃣  ZCL_ABGAGT_REST_HANDLER (Class)
    ├─ Include: ZCL_ABGAGT_REST_HANDLER======CM00D
    ├─ Method: PULL
    └─ Type: Class Method

2️⃣  ZCL_ABGAGT_REST_HANDLER (Class)
    ├─ Include: ZCL_ABGAGT_REST_HANDLER======CM00F
    ├─ Method: PUSH
    └─ Type: Class Method
```

### Program Where-Used List

```
🔍 Where-used list for: SAPLZWABGAGT (Program)

Found 4 references:

1️⃣  ZCL_ABGAGT_REST_HANDLER (Class)
    ├─ Include: ZCL_ABGAGT_REST_HANDLER======CM001
    ├─ Method: CONSTRUCTOR
    └─ Type: Class Method

2️⃣  ZCL_ABGAGT_AGENT (Class)
    ├─ Include: ZCL_ABGAGT_AGENT=============CM005
    ├─ Method: INITIALIZE
    └─ Type: Class Method
```

### Multiple Objects

```
🔍 Where-used list for: 2 objects

1️⃣  ZIF_ABGAGT_COMMAND (Interface)
    Found 3 references

2️⃣  ZCL_ABGAGT_AGENT (Class)
    Found 7 references
```

### JSON Output

```json
{
  "SUCCESS": true,
  "COMMAND": "WHERE",
  "MESSAGE": "Found 2 references for ZCL_SUT_AUNIT_RUNNER",
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
        },
        {
          "OBJECT": "ZCL_ABGAGT_REST_HANDLER",
          "OBJECT_TYPE": "CLAS",
          "INCLUDE_NAME": "ZCL_ABGAGT_REST_HANDLER======CM00E",
          "METHOD_NAME": "RUN",
          "INCLUDE_TYPE": "Class Method"
        }
      ],
      "COUNT": 2
    }
  ],
  "SUMMARY": {
    "TOTAL_OBJECTS": 1,
    "TOTAL_REFERENCES": 2
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
  "ERROR": "string"
}
```

### Field Mapping from AKB_WHERE_USED_LIST

| JSON Field | AKH_WHERE_USED_LIST Field | Description |
|------------|---------------------------|-------------|
| `OBJECT` | `OBJ_NAME` | Object using the reference |
| `OBJECT_TYPE` | `OBJ_` | Object type (CLAS, PROG) |
| `INCLUDE_NAME` | `SUB_NAME` | Source code include name |
| `METHOD_NAME` | (derived from INCLUDE_NAME) | Method name if include is a method |
| `INCLUDE_TYPE` | (derived from INCLUDE_NAME) | Human-readable include type |
| `PACKAGE` | `APPL_PACKET` | Package where the using object resides |

### Include Type Mapping

The `INCLUDE_TYPE` field provides a human-readable description of the include type:

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
   - `ZCL_*` → CLAS
   - `ZIF_*` → INTF
   - Other → CLAS (default fallback)

2. **Method Support**: Methods can be specified as `CLASSNAME=>METHODNAME` or just `METHODNAME` with `--type CLAS`

3. **Include Name**: Returns the full source code include name from `SUB_NAME`:
   - Class public section: `ZCL_CLASS=============CU`
   - Class protected section: `ZCL_CLASS=============CO`
   - Class private section: `ZCL_CLASS=============CP`
   - Class method: `ZCL_CLASS=============CM00N`
   - Class test class: `ZCL_CLASS=============CCAU`
   - Interface: `ZIF_INTERFACE============IU`

4. **Method Name**: When the include is a method include (CM###), the method name is extracted from TMDIR table based on the method index.

5. **Include Type**: Returns a human-readable description of the include type (e.g., "Class Method", "Public Section", "Unit Test").

6. **Limit Results**: Use `--limit` to restrict the number of references returned (default: 100)

---

## Error Handling

| Error | Message |
|-------|---------|
| Object not found | `Object not found: ZCL_NONEXISTENT` |
| Invalid object type | `Unsupported object type: <type>` |
| Access denied | `Access denied to where-used list for <object>` |
| No references found | `No references found for <object>` |

### Error Output

```
❌ Object not found: ZCL_NONEXISTENT
```

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
| `ZIF_*` or `zif_*` | INTF (Interface) |
| `SAPL*` or `sapl*` | PROG (Program) |
| Other | PROG (default) |

---

## Example

```bash
# Find where CL_SUT_AUNIT_RUNNER is used
abapgit-agent where --objects CL_SUT_AUNIT_RUNNER

# Find where an interface is implemented
abapgit-agent where --objects ZIF_ABGAGT_COMMAND

# Find where a method is called
abapgit-agent where --objects ZCL_ABGAGT_AGENT=>PULL --type CLAS

# Find where a program is included
abapgit-agent where --objects SAPLZWABGAGT --type PROG

# Limit results
abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --limit 20

# JSON for programmatic use
abapgit-agent where --objects ZCL_SUT_AUNIT_RUNNER --json
```

---

## Use Cases

### 1. Finding API Usage Examples

When you know a class exists (e.g., `CL_SUT_AUNIT_RUNNER`) but don't know how to use it:

```bash
abapgit-agent where --objects CL_SUT_AUNIT_RUNNER
```

Returns all places that use this class, showing:
- Which classes reference it
- The include name where it's used

Then use `view` to examine the source:

```bash
# View the include to see actual usage code
abapgit-agent view --objects ZCL_ABGAGT_COMMAND_UNIT=======CM007 --type PROG
```

### 2. Understanding Method Usage

```bash
# Find where a specific method is called
abapgit-agent where --objects ZCL_AGENT=>PULL --type CLAS
```

### 3. Finding Interface Implementations

```bash
# Find all classes that implement an interface
abapgit-agent where --objects ZIF_ABGAGT_COMMAND
```

---

## Workflow: Where + View

The typical workflow for understanding an unfamiliar ABAP object:

1. **Use `view`** to understand the object definition (method signatures, interface)
2. **Use `where`** to find where it's used in the system
3. **Use `view` again** with the include name to examine the actual usage code

Example:

```bash
# Step 1: Understand CL_SUT_AUNIT_RUNNER
abapgit-agent view --objects CL_SUT_AUNIT_RUNNER

# Step 2: Find how it's used
abapgit-agent where --objects CL_SUT_AUNIT_RUNNER

# Step 3: View a specific usage
abapgit-agent view --objects ZCL_ABGAGT_COMMAND_UNIT======CCAU --type PROG
```

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

### Alternative: RS_EU_CROSSREF

`AKB_WHERE_USED_LIST` internally calls `RS_EU_CROSSREF`, which can also be called directly:

```abap
CALL FUNCTION 'RS_EU_CROSSREF'
  EXPORTING
    object_type = 'CLAS'
    object_name = 'ZCL_MY_CLASS'
  TABLES
    references = lt_refs.
```

### Result Table Structure (AKB_EXCEPT_TYPE)

| Field | Type | Description |
|-------|------|-------------|
| `OBJ_` | TROBJTYPE | Object type of the using object (CLAS, PROG) |
| `OBJ_NAME` | SOBJ_NAME | Name of the object using the reference |
| `SUB_` | TROBJTYPE | Sub object type (METH, ATTR, TYPE) |
| `SUB_NAME` | SOBJ_NAME | Source code include name |
| `APPL` | CHAR(1) | Application area |
| `APPL_NAME` | STRING | Application name |
| `APPL_DLVUNIT` | DEVCLASS | Delivery unit |
| `APPL_PACKET` | DEVCLASS | Package |

### Include Name Patterns

The `SUB_NAME` field contains the source code include name.

#### Padding Rule

The class name portion (including padding) is **ALWAYS 30 characters**. The formula is:

```
SUB_NAME = PAD(classname, 30, '=') + include_type
```

| Class Name Length | Padding (= signs) | Include Type | Total |
|-----------------|------------------|--------------|-------|
| 15 | 15 `=` | CM001 (5) | 35 |
| 20 | 10 `=` | CU (2) | 32 |
| 22 | 8 `=` | CCAU (4) | 34 |
| 29 | 1 `=` | CM001 (5) | 35 |
| 30 | 0 `=` | CM001 (5) | 35 |

**When class name is 30 characters**: No padding needed, include type appended directly.

Examples:
- `ZCL_ABGAGT_AGENT=============CU` (15 + 13 + 2 = 30)
- `CL_GRCAUD_ACTION_NOTIFIER=====CU` (22 + 8 + 2 = 32)
- `CL_GRCAUD_V_USER_AUTH_R_AGNMNTCM001` (30 + 0 + 5 = 35, no padding)

#### Class Include Types

| Include Type | Description | Length |
|-------------|-------------|--------|
| `CU` | Public section | 2 |
| `CO` | Protected section | 2 |
| `CP` | Private section | 2 |
| `CM001-CM099` | Method implementations (01-99) | 5 |
| `CM00A-CM99Z` | Extended method numbers (A-Z, 01A-99Z) | 5 |
| `CCAU` | Test class (AUnit) | 4 |
| `CCIMP` | Local class implementations | 4 |
| `CCDEF` | Local class definitions | 4 |
| `CI` | Local interface/type definitions | 2 |
| `CT` | Macros | 2 |

#### Interface Include Types

| Include Type | Description | Length |
|-------------|-------------|--------|
| `IU` | Interface section | 2 |

#### Program Include Types

| Include Type | Description |
|-------------|-------------|
| (none) | Main program - include name is just the program name |

---

## Notes

1. **Performance**: Where-used list can be slow for widely-used objects. Use `--limit` to restrict results.

2. **Cross-System**: Where-used list only searches the current ABAP system, not the entire SAP landscape.

3. **Inactive Objects**: Search includes both active and inactive objects.

4. **Package Restriction**: By default searches all packages. Consider adding `--package` parameter in future if needed.
