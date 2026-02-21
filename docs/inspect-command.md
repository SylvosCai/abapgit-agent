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

  ✅ CLAS ZCL_MY_CLASS - Syntax check passed
```

### With Warnings

```
  Inspect for 1 file(s)

  ⚠️  CLAS ZCL_MY_CLASS - Syntax check passed with warnings (2):

  Warnings:
  ─────────────────────────────────────────────────────────────
    Method: MY_METHOD
    Line 000049:
      Include: ZCL_MY_CLASS========CM002
      The exception CX_DD_DDL_READ is not caught or declared in the RAISING clause of"MY_METHOD".
```

### With Errors

```
  Inspect for 1 file(s)

  ❌ CLAS ZCL_MY_CLASS - Syntax check failed (1 error(s)):

  Errors:
  ─────────────────────────────────────────────────────────────
    Method: MY_METHOD
    Line 000021, Column 12:
      Include: ZCL_MY_CLASS========CM002
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
      "word": "ZMYCLASS",
      "sobjname": "ZCL_MY_CLASS========CM002",
      "method_name": "MY_METHOD"
    }
  ],
  "warnings": [
    {
      "line": "49",
      "message": "The exception CX_DD_DDL_READ is not caught...",
      "sobjname": "ZCL_MY_CLASS========CM002",
      "method_name": "MY_METHOD"
    }
  ],
  "infos": [
    {
      "line": "10",
      "message": "Information message",
      "sobjname": "ZCL_MY_CLASS========CM001",
      "method_name": "CONSTRUCTOR"
    }
  ]
}
```

---

## Key Behaviors

1. **Multiple files in one request** - All files are sent in a single API call
2. **CDS View validation** - Uses `CL_DD_DDL_HANDLER_FACTORY` to validate CDS views
3. **Method name extraction** - For classes, extracts method name from TMDIR based on include number (CM00X)
4. **Separate warnings and info** - Warnings ('W') and Information ('I') are displayed in separate sections
5. **Sorted results** - Errors, warnings, and info are sorted by method name and line number ascending

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
   ❌ CLAS ZCL_MY_CLASS - Syntax check failed (1 error(s)):
      Method: MY_METHOD
      Line 000021, Column 12:
        Field "LV_VAR" is unknown
```
