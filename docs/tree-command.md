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

### Basic Tree

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
```

### With Object Counts

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
```

### With Parent Package

```
🌳 Package Tree: $ZMAIN_PACKAGE

⬆️  Parent: $ZSAP_BASE (SAP Base Package)

📦 $ZMAIN_PACKAGE (Main Package)
   └─ 📦 $ZMAIN_SUB1 (Sub Package 1)

📊 Summary
PACKAGES: 2
OBJECTS: 15
TYPES: CLAS=5 PROG=10
```

### JSON Output

```json
{
  "SUCCESS": true,
  "COMMAND": "TREE",
  "PACKAGE": "$ZMAIN_PACKAGE",
  "MESSAGE": "Tree retrieved successfully",
  "PARENT_PACKAGE": "$ZSAP_BASE",
  "NODES": [
    {
      "PACKAGE": "$ZMAIN_PACKAGE",
      "PARENT": "",
      "DESCRIPTION": "$ZMAIN_PACKAGE",
      "DEPTH": 0,
      "OBJECT_COUNT": 11
    },
    {
      "PACKAGE": "$ZMAIN_SUB1",
      "PARENT": "$ZMAIN_PACKAGE",
      "DESCRIPTION": "Sub Package 1",
      "DEPTH": 1,
      "OBJECT_COUNT": 10
    },
    {
      "PACKAGE": "$ZMAIN_SUB1_A",
      "PARENT": "$ZMAIN_SUB1",
      "DESCRIPTION": "Sub Package 1A",
      "DEPTH": 2,
      "OBJECT_COUNT": 3
    }
  ],
  "TOTAL_PACKAGES": 6,
  "TOTAL_OBJECTS": 127,
  "OBJECTS": [
    { "OBJECT": "CLAS", "COUNT": 10 },
    { "OBJECT": "INTF", "COUNT": 2 },
    { "OBJECT": "PROG", "COUNT": 11 },
    { "OBJECT": "FUGR", "COUNT": 1 },
    { "OBJECT": "TABL", "COUNT": 3 }
  ],
  "ERROR": ""
}
```

**Response Fields:**

| Field | Type | Description |
|-------|------|-------------|
| `SUCCESS` | boolean | Whether the request succeeded |
| `COMMAND` | string | Command name ("TREE") |
| `PACKAGE` | string | Root package name |
| `MESSAGE` | string | Status message |
| `PARENT_PACKAGE` | string | Parent package (empty if root) |
| `NODES` | array | Flat list of all packages with parent refs |
| `TOTAL_PACKAGES` | number | Total packages in tree |
| `TOTAL_OBJECTS` | number | Total objects in tree |
| `OBJECTS` | array | Object counts by type [{OBJECT, COUNT}] |
| `ERROR` | string | Error message (empty if success) |

**Node Structure:**

Each entry in `NODES` array:

| Field | Type | Description |
|-------|------|-------------|
| `PACKAGE` | string | Package name |
| `PARENT` | string | Parent package name |
| `DESCRIPTION` | string | Package description |
| `DEPTH` | number | Depth in tree (0 = root) |
| `OBJECT_COUNT` | number | Objects in this package |

**Note:** The `NODES` array is flat - children are identified by `PARENT` reference. The CLI builds the tree display by grouping nodes by depth and parent.

---

## Response Structure

### JSON Mode

```json
{
  "SUCCESS": true,
  "COMMAND": "TREE",
  "PACKAGE": "$ZMAIN_PACKAGE",
  "MESSAGE": "Tree retrieved successfully",
  "PARENT_PACKAGE": "$ZSAP_BASE",
  "NODES": [...],
  "TOTAL_PACKAGES": 6,
  "TOTAL_OBJECTS": 127,
  "OBJECTS": [
    { "OBJECT": "CLAS", "COUNT": 10 },
    { "OBJECT": "INTF", "COUNT": 2 }
  ],
  "ERROR": ""
}
```

See [JSON Output](#json-output-pure-scripting) for full schema.

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
COUNT=$(abapgit-agent tree --package $ZMY_PACKAGE --json | jq '.TOTAL_OBJECTS')
echo "Package contains $COUNT objects"
```

---

## Implementation

### ABAP Tables Used

| Table | Purpose |
|-------|---------|
| **TDEVC** | Package definitions (contains `PARENTCL` for hierarchy) |
| **TADIR** | Object directory (for object counts per package) |

### Query Logic

```abap
" Get root package with parent
SELECT SINGLE devclass parentcl FROM tdevc
  INTO ls_package
 WHERE devclass = lv_package.

" Get direct sub-packages recursively (up to depth)
SELECT devclass parentcl FROM tdevc
  INTO TABLE lt_direct_subs
 WHERE parentcl = iv_parent.

" Get object count per package
SELECT COUNT(*) FROM tadir
  INTO lv_count
 WHERE devclass = iv_package
   AND object NOT IN ('DEVC', 'PACK').

" Get object counts by type
SELECT object COUNT(*) AS count FROM tadir
  INTO TABLE lt_counts
 WHERE devclass = iv_package
   AND object NOT IN ('DEVC', 'PACK')
 GROUP BY object.
```
