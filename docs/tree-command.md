# tree Command Requirements

## Overview

Display the package hierarchy tree from an ABAP system, showing parent packages, sub-packages, and object counts.

## Command

```bash
# Basic usage
abapgit-agent tree --package $ZMY_PACKAGE

# With object breakdown
abapgit-agent tree --package $ZMY_PACKAGE --include-objects

# Limit depth
abapgit-agent tree --package $ZMY_PACKAGE --depth 3

# JSON output for scripting (pure machine-readable)
abapgit-agent tree --package $ZMY_PACKAGE --json
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Package must exist in the ABAP system

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--package` | Yes | ABAP package name (e.g., `$ZMY_PACKAGE`, `ZMY_PACKAGE`) |
| `--depth` | No | Maximum depth to traverse (default: 3) |
| `--include-objects` | No | Include object counts breakdown |
| `--json` | No | Output raw JSON only (for scripting) |

---

## Tasks

### 1. Validate Parameters

- `--package` must be specified
- Package name must be valid (1-30 characters, start with letter or `$`)

### 2. Load Configuration

Read `.abapGitAgent` for credentials

### 3. Fetch CSRF Token

```bash
GET /health (with X-CSRF-Token: fetch)
```

### 4. Make Tree Request

**Endpoint:** `POST /tree`

**Request Body:**
```json
{
  "package": "$ZMY_PACKAGE",
  "depth": 3,
  "include_objects": true
}
```

### 5. Display Results

---

## Output

### Basic Tree (Human + AI-Friendly)

```
🌳 Package Tree: $ZMAIN_PACKAGE

📦 $ZMAIN_PACKAGE (Main Package)
   ├─ 📦 $ZMAIN_SUB1 (Sub Package 1)
   │    ├─ 📦 $ZMAIN_SUB1_A (Sub Package 1A)
   │    └─ 📦 $ZMAIN_SUB1_B (Sub Package 1B)
   └─ 📦 $ZMAIN_SUB2 (Sub Package 2)
        └─ 📦 $ZMAIN_SUB2_A (Sub Package 2A)

📊 Summary
PACKAGES: 6
OBJECTS: 127

<!-- AI_METADATA_START -->
{"package":"$ZMAIN_PACKAGE","parent":"$ZSAP_BASE","total_packages":6,"total_objects":127}
<!-- AI_METADATA_END -->
```

### With Object Counts (Human + AI-Friendly)

```
🌳 Package Tree: $ZMAIN_PACKAGE

📦 $ZMAIN_PACKAGE (Main Package)
   ├─ 📦 $ZMAIN_SUB1 (Sub Package 1)
   │    ├─ 📦 $ZMAIN_SUB1_A (Sub Package 1A)
   │    └─ 📦 $ZMAIN_SUB1_B (Sub Package 1B)
   └─ 📦 $ZMAIN_SUB2 (Sub Package 2)
        └─ 📦 $ZMAIN_SUB2_A (Sub Package 2A)

📊 Summary
PACKAGES: 6
OBJECTS: 127
TYPES: CLAS=10 INTF=2 PROG=11 FUGR=1 TABL=3

<!-- AI_METADATA_START -->
{"package":"$ZMAIN_PACKAGE","parent":"$ZSAP_BASE","total_packages":6,"total_objects":127,"types":{"CLAS":10,"INTF":2,"PROG":11,"FUGR":1,"TABL":3}}
<!-- AI_METADATA_END -->
```

### With Parent Package (Human + AI-Friendly)

```
🌳 Package Tree: $ZMAIN_PACKAGE

⬆️  Parent: $ZSAP_BASE (SAP Base Package)

📦 $ZMAIN_PACKAGE (Main Package)
   └─ 📦 $ZMAIN_SUB1 (Sub Package 1)

📊 Summary
PACKAGES: 2
OBJECTS: 15
TYPES: CLAS=5 PROG=10

<!-- AI_METADATA_START -->
{"package":"$ZMAIN_PACKAGE","parent":"$ZSAP_BASE","total_packages":2,"total_objects":15,"types":{"CLAS":5,"PROG":10}}
<!-- AI_METADATA_END -->
```

### JSON Output (Pure Scripting)

```json
{
  "success": "X",
  "command": "TREE",
  "package": "$ZMAIN_PACKAGE",
  "message": "Tree retrieved successfully",
  "hierarchy": {
    "package": "$ZMAIN_PACKAGE",
    "description": "Main Package",
    "parent": {
      "package": "$ZSAP_BASE",
      "description": "SAP Base Package"
    },
    "subpackages": [
      {
        "package": "$ZMAIN_SUB1",
        "description": "Sub Package 1",
        "depth": 1,
        "object_count": 10,
        "objects": {
          "CLAS": 5,
          "INTF": 2,
          "PROG": 3
        },
        "subpackages": [
          {
            "package": "$ZMAIN_SUB1_A",
            "description": "Sub Package 1A",
            "depth": 2,
            "object_count": 3,
            "objects": {
              "CLAS": 2,
              "TABL": 1
            },
            "subpackages": []
          }
        ]
      }
    ],
    "total_subpackages": 5,
    "total_objects": 127
  },
  "summary": {
    "total_packages": 6,
    "total_objects": 127,
    "objects_by_type": {
      "CLAS": 10,
      "INTF": 2,
      "PROG": 11,
      "FUGR": 1,
      "TABL": 3
    }
  }
}
```

---

## AI-Friendly Metadata Format

### Inline Metadata Block

The default output includes an HTML-style comment block for reliable AI parsing:

```html
<!-- AI_METADATA_START -->
{"package":"$ZMAIN_PACKAGE","parent":"$ZSAP_BASE","total_packages":6,"total_objects":127,"types":{"CLAS":10,"INTF":2,"PROG":11}}
<!-- AI_METADATA_END -->
```

### Metadata Schema

| Field | Type | Description |
|-------|------|-------------|
| `package` | string | Root package name |
| `parent` | string | Parent package name (or null if root) |
| `total_packages` | number | Total packages including all subpackages |
| `total_objects` | number | Total objects in entire tree |
| `types` | object | Object counts by type (e.g., `{"CLAS":10}`) |

### Why This Format?

1. **Reliable extraction** - AI tools can find `<!-- AI_METADATA_START -->` and `<!-- AI_METADATA_END -->` markers
2. **Self-contained JSON** - No ambiguity in parsing
3. **Human invisible** - Hidden in terminal (comment syntax)
4. **Backwards compatible** - Existing human-readable output unchanged

---

## Response Structure

### JSON Mode

```json
{
  "success": "X",
  "command": "TREE",
  "package": "$ZMAIN_PACKAGE",
  "message": "Tree retrieved successfully",
  "hierarchy": {
    "package": "$ZMAIN_PACKAGE",
    "description": "Main Package",
    "parent": {
      "package": "$ZSAP_BASE",
      "description": "SAP Base Package"
    },
    "subpackages": [...],
    "total_subpackages": 5,
    "total_objects": 127
  },
  "summary": {
    "total_packages": 6,
    "total_objects": 127,
    "objects_by_type": {
      "CLAS": 10,
      "INTF": 2,
      "PROG": 11,
      "FUGR": 1,
      "TABL": 3
    }
  }
}
```

### AI Metadata (Default Mode)

```json
{
  "package": "$ZMAIN_PACKAGE",
  "parent": "$ZSAP_BASE",
  "total_packages": 6,
  "total_objects": 127,
  "types": {
    "CLAS": 10,
    "INTF": 2,
    "PROG": 11,
    "FUGR": 1,
    "TABL": 3
  }
}
```

---

## Error Handling

| Error | Message |
|-------|---------|
| Package not found | `Package <name> does not exist in the system` |
| Invalid package name | `Invalid package name: <name>` |
| Access denied | `Access denied to package information` |
| Depth too large | `Depth value too large (max: 10)` |

### Error Output

```
❌ Package not found: $ZNONEXISTENT

Error: Package $ZNONEXISTENT does not exist in the system.
```

---

## Example

```bash
# Basic usage
abapgit-agent tree --package $ZMY_PACKAGE

# With object breakdown
abapgit-agent tree --package $ZMY_PACKAGE --include-objects

# Shallow tree (depth 1)
abapgit-agent tree --package $ZMY_PACKAGE --depth 1

# JSON for scripting
abapgit-agent tree --package $ZMY_PACKAGE --json > tree.json

# AI-friendly extract
abapgit-agent tree --package $ZMY_PACKAGE | grep -oP '(?<=<!-- AI_METADATA_START -->).*(?=<!-- AI_METADATA_END -->)' | jq '.total_objects'

# CI/CD usage with jq
COUNT=$(abapgit-agent tree --package $ZMY_PACKAGE --json | jq '.summary.total_objects')
echo "Package contains $COUNT objects"
```

---

## Implementation

### ABAP Tables Used

| Table | Purpose |
|-------|---------|
| **TDEVC** | Package definitions (contains `PARENT_PACK` for hierarchy) |
| **TADIR** | Object directory (for object counts per package) |

### Query Logic

```abap
" Get direct parent
SELECT SINGLE devclass, parent_pack, as4text
  FROM tdevc
 INTO @DATA(ls_parent)
 WHERE devclass = @iv_package.

" Get sub-packages recursively (up to depth)
SELECT devclass, parent_pack, as4text
  FROM tdevc
 INTO TABLE @DATA(lt_subpackages)
 WHERE parent_pack = @iv_package
    OR parent_pack IN ( SELECT devclass FROM tdevc WHERE parent_pack = @iv_package ).

" Get object counts per package
SELECT devclass, object, COUNT(*)
  FROM tadir
 INTO TABLE @DATA(lt_counts)
 WHERE devclass = @iv_package
 GROUP BY devclass, object.
```
