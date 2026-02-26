---
layout: default
title: syntax - Source Syntax Check
nav_order: 3
parent: Development Commands
---

# syntax Command Requirements

## Overview

Check syntax of ABAP source files directly WITHOUT pull/activation. Reads source from local files and checks in ABAP system memory.

**Key difference from `inspect`:**
- `syntax` - Checks LOCAL source code BEFORE commit (no pull needed)
- `inspect` - Checks ACTIVATED code AFTER pull (uses Code Inspector)

## Command

```bash
# Check single file
abapgit-agent syntax --files src/zcl_my_class.clas.abap

# Check multiple files
abapgit-agent syntax --files src/zcl_my_class.clas.abap,src/zcl_other.clas.abap

# With ABAP Cloud (BTP) stricter rules
abapgit-agent syntax --files src/zcl_my_class.clas.abap --cloud

# Output as JSON
abapgit-agent syntax --files src/zcl_my_class.clas.abap --json
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Files must exist in the filesystem
- Supported object types: CLAS, INTF, PROG

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--files` | Yes | Comma-separated list of files to check |
| `--cloud` | No | Use ABAP Cloud syntax check (stricter, for BTP) |
| `--json` | No | Output raw JSON |

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

### 4. Read Source Files

Read source code from local filesystem

### 5. Make Syntax Request

**Endpoint:** `POST /syntax`

**Request Body:**
```json
{
  "objects": [
    {
      "type": "CLAS",
      "name": "ZCL_MY_CLASS",
      "source": "CLASS zcl_my_class DEFINITION...",
      "locals_def": "CLASS lcl_helper DEFINITION...",
      "locals_imp": "CLASS lcl_helper IMPLEMENTATION..."
    }
  ],
  "uccheck": "X"
}
```

### 6. Display Results

---

## Output

### Success (No Errors)

```
  Syntax check for 1 file(s)

✅ CLAS ZCL_MY_CLASS - Syntax check passed

✅ All 1 object(s) passed syntax check
```

### With ABAP Cloud Mode

```
  Syntax check for 1 file(s)
  Mode: ABAP Cloud

✅ CLAS ZCL_MY_CLASS - Syntax check passed

✅ All 1 object(s) passed syntax check
```

### With Errors

```
  Syntax check for 1 file(s)

❌ CLAS ZCL_MY_CLASS - Syntax check failed (1 error(s))

Errors:
────────────────────────────────────────────────────────────
  Line 9:
    The statement "UNKNOWN_STATEMENT" is invalid. Check the spelling.

❌ 1 of 1 object(s) have syntax errors
```

### Multiple Files with Errors

```
  Syntax check for 3 file(s)

❌ CLAS ZCL_CLASS1 - Syntax check failed (1 error(s))

Errors:
────────────────────────────────────────────────────────────
  Line 15:
    Field "LV_UNDEFINED" is unknown.

❌ INTF ZIF_INTERFACE1 - Syntax check failed (1 error(s))

Errors:
────────────────────────────────────────────────────────────
  Line 10:
    Unable to interpret "UNKNOWN_KEYWORD".

✅ PROG ZPROGRAM1 - Syntax check passed

❌ 2 of 3 object(s) have syntax errors
```

---

## Response Structure

```json
{
  "SUCCESS": true,
  "COMMAND": "SYNTAX",
  "MESSAGE": "All 1 object(s) passed syntax check",
  "RESULTS": [
    {
      "OBJECT_TYPE": "CLAS",
      "OBJECT_NAME": "ZCL_MY_CLASS",
      "SUCCESS": true,
      "ERROR_COUNT": 0,
      "ERRORS": [],
      "WARNINGS": [],
      "MESSAGE": "Syntax check passed"
    }
  ]
}
```

**With errors:**

```json
{
  "SUCCESS": false,
  "COMMAND": "SYNTAX",
  "MESSAGE": "1 of 1 object(s) have syntax errors",
  "RESULTS": [
    {
      "OBJECT_TYPE": "CLAS",
      "OBJECT_NAME": "ZCL_MY_CLASS",
      "SUCCESS": false,
      "ERROR_COUNT": 1,
      "ERRORS": [
        {
          "LINE": 9,
          "TEXT": "The statement \"UNKNOWN_STATEMENT\" is invalid.",
          "WORD": "UNKNOWN_STATEMENT"
        }
      ],
      "WARNINGS": [],
      "MESSAGE": "The statement \"UNKNOWN_STATEMENT\" is invalid."
    }
  ]
}
```

---

## Key Behaviors

1. **Line numbers match source file** - Error line numbers correspond to VS Code line numbers
2. **Local classes auto-detected** - Companion `.locals_def.abap` and `.locals_imp.abap` files are automatically included
3. **First error only** - ABAP's `SYNTAX-CHECK` stops at the first error per file
4. **No warnings** - Only syntax errors are reported (use `inspect` for warnings)
5. **No database writes** - Source is checked in memory only

---

## Supported Object Types

| Type | Description | File Extension |
|------|-------------|----------------|
| CLAS | Class | `.clas.abap` |
| INTF | Interface | `.intf.abap` |
| PROG | Program | `.prog.abap` |

**Note:** For other types (DDLS, FUGR, TABL, etc.), use `pull` then `inspect`.

---

## Error Handling

| Error | Message |
|-------|---------|
| File not found | `File not found: <path>` |
| Invalid file format | `Invalid file format: <file>` |
| No --files specified | `Error: --files parameter required` |
| Unsupported object type | `Unsupported object type: <type>. Use 'pull' command instead.` |

---

## File Format

Files are parsed to extract object type and name:

| File | Object Type | Object Name |
|------|-------------|-------------|
| `zcl_my_class.clas.abap` | CLAS | ZCL_MY_CLASS |
| `zif_my_intf.intf.abap` | INTF | ZIF_MY_INTF |
| `zmy_program.prog.abap` | PROG | ZMY_PROGRAM |
| `zcl_my_class.clas.locals_def.abap` | CLAS | ZCL_MY_CLASS (local defs) |
| `zcl_my_class.clas.locals_imp.abap` | CLAS | ZCL_MY_CLASS (local imps) |

---

## Example

```bash
# Basic syntax check
abapgit-agent syntax --files src/zcl_my_class.clas.abap

# Multiple files
abapgit-agent syntax --files src/zcl_my_class.clas.abap,src/zif_my_intf.intf.abap

# ABAP Cloud mode (stricter)
abapgit-agent syntax --files src/zcl_my_class.clas.abap --cloud

# JSON output
abapgit-agent syntax --files src/zcl_my_class.clas.abap --json
```

## Use Case

Use `syntax` when:
- You want to check code BEFORE committing to git
- You want faster feedback than pull + inspect
- You're developing CLAS, INTF, or PROG objects

```
# Workflow: Check locally BEFORE commit
vim src/zcl_my_class.clas.abap

abapgit-agent syntax --files src/zcl_my_class.clas.abap
   ❌ CLAS ZCL_MY_CLASS - Syntax check failed (1 error(s)):
      Line 9:
        The statement "UNKNOWN_STATEMENT" is invalid.

# Fix the error locally, then check again
abapgit-agent syntax --files src/zcl_my_class.clas.abap
   ✅ CLAS ZCL_MY_CLASS - Syntax check passed

# Now safe to commit and push
git add . && git commit -m "Fix class" && git push
abapgit-agent pull --files src/zcl_my_class.clas.abap
```

## Comparison with inspect

| Aspect | `syntax` | `inspect` |
|--------|----------|-----------|
| When to use | BEFORE commit | AFTER pull |
| Requires pull? | No | Yes |
| Checks | Local source | Activated code |
| Uses | SYNTAX-CHECK statement | Code Inspector (SCI) |
| Warnings | No | Yes |
| Object types | CLAS, INTF, PROG | All (including DDLS) |
| Speed | Fast | Slower |
