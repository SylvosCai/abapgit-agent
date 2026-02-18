# inspect Command Requirements

## Overview

Inspect ABAP source file(s) for syntax errors and issues.

## Command

```bash
# Inspect single file
abapgit-agent inspect --files zcl_my_class.clas.abap

# Inspect multiple files
abapgit-agent inspect --files zcl_my_class.clas.abap,zcl_other.clas.abap

# With path
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# With Code Inspector variant
abapgit-agent inspect --files abap/zcl_my_class.clas.abap --variant ALL_CHECKS

# With no variant (uses default SAP standard checks)
abapgit-agent inspect --files abap/zcl_my_class.clas.abap --variant EMPTY
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Files must exist in the filesystem

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--files` | Yes | Comma-separated list of files to inspect |
| `--variant` | No | Code Inspector variant name (e.g., `ALL_CHECKS`, `EMPTY`) |

---

## Tasks

### 1. Validate Parameters

- `--files` must be specified
- Files must exist

### 2. Load Configuration

Read `.abapGitAgent` for credentials

### 3. Fetch CSRF Token

```bash
GET /health (with X-CSRF-Token: fetch)
```

### 4. Make Inspect Request

**Endpoint:** `POST /inspect`

**Request Body:**
```json
{
  "files": ["ZCL_MY_CLASS.CLASS.ABAP"],
  "variant": "ALL_CHECKS"
}
```

### 5. Display Results

---

## Output

### Success (No Errors)

```
  Inspect for 1 file(s)

  ✅ zcl_my_class.clas.abap - Syntax check passed (0 errors)
```

### With Errors

```
  Inspect for 1 file(s)

  ❌ Syntax check failed (2 error(s)):

  Errors:
────────────────────────────────────────────────────────────
  Line 15, Column 10:
    "ZMYCLASS" is not a type

  Line 20, Column 5:
    Field "LV_VAR" is unknown
```

---

## Response Structure

```json
{
  "success": "X",
  "object_type": "CLAS",
  "object_name": "ZCL_MY_CLASS",
  "error_count": 2,
  "errors": [
    {
      "line": "15",
      "column": "10",
      "text": "\"ZMYCLASS\" is not a type",
      "word": "ZMYCLASS"
    }
  ]
}
```

---

## Error Handling

| Error | Message |
|-------|---------|
| File not found | `File not found: <path>` |
| Invalid file format | `Invalid file format: <file>` |
| No --files specified | `Error: --files parameter required` |

---

## File Format

Same as pull command - files are parsed to extract object type and name:

| File | Object Type | Object Name |
|------|-------------|-------------|
| `zcl_my_class.clas.abap` | CLAS | ZCL_MY_CLASS |
| `zif_my_intf.intf.abap` | INTF | ZIF_MY_INTF |

---

## Example

```bash
# Syntax check
abapgit-agent inspect --files zcl_my_class.clas.abap

# Multiple files
abapgit-agent inspect --files abap/zcl_my_class.clas.abap,abap/zcl_other.clas.abap

# With Code Inspector variant
abapgit-agent inspect --files abap/zcl_my_class.clas.abap --variant ALL_CHECKS
```

## Use Case

Use `inspect` when:
- Pull shows "Error updating where-used list" (syntax error)
- You need detailed error messages with line numbers
- Debugging activation failures

```
abapgit-agent pull
   ❌ CLAS ZCL_MY_CLASS: Error updating where-used list

abapgit-agent inspect --files abap/zcl_my_class.clas.abap
   ❌ Line 15, Column 10: "ZMYCLASS" is not a type
```
