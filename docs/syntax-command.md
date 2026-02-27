---
layout: default
title: syntax - Source Syntax Check
nav_order: 2
parent: Development Commands
---

# syntax Command Requirements

## Overview

Check syntax of ABAP source files directly WITHOUT pull/activation. Reads source from local files and checks in ABAP system memory.

**Key difference from `inspect`:**
- `syntax` - Checks LOCAL source code BEFORE commit (no pull needed)
- `inspect` - Checks ACTIVATED code AFTER pull (uses Code Inspector)

**Auto-detection:** When checking any class file (main, locals_def, locals_imp, or testclasses), the command automatically detects and includes ALL companion files for a complete syntax check.

## Command

```bash
# Check single file - auto-detects all companions
abapgit-agent syntax --files src/zcl_my_class.clas.abap

# Check test classes - auto-detects main class and local files
abapgit-agent syntax --files src/zcl_my_class.clas.testclasses.abap

# Check local implementations - auto-detects main, locals_def, testclasses
abapgit-agent syntax --files src/zcl_my_class.clas.locals_imp.abap

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
- For class files: Companion files are auto-detected if they exist

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

### Success with Auto-Detection

```
  Syntax check for 1 file(s)

  Auto-detected: zcl_my_class.clas.abap
  Auto-detected: zcl_my_class.clas.locals_def.abap
  Auto-detected: zcl_my_class.clas.locals_imp.abap

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

### With Errors (Single File)

```
  Syntax check for 1 file(s)

❌ CLAS ZCL_MY_CLASS - Syntax check failed (1 error(s))

Errors:
────────────────────────────────────────────────────────────
  Line 9:
    The statement "UNKNOWN_STATEMENT" is invalid. Check the spelling.

❌ 1 of 1 object(s) have syntax errors
```

### With Errors (Multiple Files - Shows Which File)

```
  Syntax check for 1 file(s)

  Auto-detected: zcl_my_class.clas.abap
  Auto-detected: zcl_my_class.clas.locals_def.abap
  Auto-detected: zcl_my_class.clas.locals_imp.abap

❌ CLAS ZCL_MY_CLASS - Syntax check failed (1 error(s))

Errors:
────────────────────────────────────────────────────────────
  In: Local implementations (zcl_my_class.clas.locals_imp.abap)
  Line 24:
    "." expected after "LV_RESULT".

❌ 1 of 1 object(s) have syntax errors
```

### Multiple Objects with Errors

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

**With errors (includes file location):**

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
          "LINE": 24,
          "TEXT": "\".\" expected after \"LV_RESULT\".",
          "WORD": "LV_RESULT",
          "INCLUDE": "locals_imp"
        }
      ],
      "WARNINGS": [],
      "MESSAGE": "\".\" expected after \"LV_RESULT\"."
    }
  ]
}
```

---

## Key Behaviors

1. **Line numbers match source file** - Error line numbers correspond to the actual line in the source file
2. **All companion files auto-detected** - When any class file is checked, ALL companion files are automatically included:
   - Main class (`.clas.abap`)
   - Local definitions (`.clas.locals_def.abap`)
   - Local implementations (`.clas.locals_imp.abap`)
   - Test classes (`.clas.testclasses.abap`)
3. **Error location displayed** - Shows exactly which file contains the error (e.g., "In: Local implementations (zcl_my_class.clas.locals_imp.abap)")
4. **Test classes fully supported** - `.testclasses.abap` files can be checked by providing just the testclasses file
5. **First error only** - ABAP's `SYNTAX-CHECK` stops at the first error per file
6. **No warnings** - Only syntax errors are reported (use `inspect` for warnings)
7. **No database writes** - Source is checked in memory only
7. **No database writes** - Source is checked in memory only

---

## Supported Object Types

| Type | Description | File Extension |
|------|-------------|----------------|
| CLAS | Class | `.clas.abap` |
| CLAS | Local definitions | `.clas.locals_def.abap` |
| CLAS | Local implementations | `.clas.locals_imp.abap` |
| CLAS | Test classes | `.clas.testclasses.abap` |
| INTF | Interface | `.intf.abap` |
| PROG | Program | `.prog.abap` |

**Note:**
- For class files, ALL companion files are automatically detected and included
- For other types (DDLS, FUGR, TABL, etc.), use `pull` then `inspect`

---

## Auto-Detection

When checking any class file, the command automatically detects and includes ALL companion files:

| You Provide | Auto-Detects (if exist) |
|-------------|-------------------------|
| `.clas.abap` | locals_def, locals_imp, testclasses |
| `.clas.locals_def.abap` | main, locals_imp, testclasses |
| `.clas.locals_imp.abap` | main, locals_def, testclasses |
| `.clas.testclasses.abap` | main, locals_def, locals_imp |

**Example:**
```bash
# You provide only testclasses file
abapgit-agent syntax --files src/zcl_my_class.clas.testclasses.abap

# Command auto-detects and includes:
  Auto-detected: zcl_my_class.clas.abap
  Auto-detected: zcl_my_class.clas.locals_def.abap
  Auto-detected: zcl_my_class.clas.locals_imp.abap
```

---

## Error Location Display

When errors occur in classes with multiple files, the output shows exactly which file contains the error:

| Include Type | Display Format |
|--------------|----------------|
| Main class | `In: Main class (zcl_my_class.clas.abap)` |
| Local definitions | `In: Local definitions (zcl_my_class.clas.locals_def.abap)` |
| Local implementations | `In: Local implementations (zcl_my_class.clas.locals_imp.abap)` |
| Test classes | `In: Test classes (zcl_my_class.clas.testclasses.abap)` |

This makes it immediately clear which file to edit.

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
| `zcl_my_class.clas.testclasses.abap` | CLAS | ZCL_MY_CLASS (test classes) |

---

## Examples

### Check Any Class File

```bash
# Check main class - auto-detects all companions
abapgit-agent syntax --files src/zcl_my_class.clas.abap

# Check test classes - auto-detects main and local files
abapgit-agent syntax --files src/zcl_my_class.clas.testclasses.abap

# Check local implementations - auto-detects all others
abapgit-agent syntax --files src/zcl_my_class.clas.locals_imp.abap

# All produce the same complete syntax check
```

### Multiple Independent Objects

```bash
# Check multiple unrelated files
abapgit-agent syntax --files src/zcl_class1.clas.abap,src/zcl_class2.clas.abap,src/zif_intf.intf.abap
```

### ABAP Cloud Mode

```bash
# Use stricter ABAP Cloud syntax rules
abapgit-agent syntax --files src/zcl_my_class.clas.abap --cloud
```

### JSON Output

```bash
# Get raw JSON response
abapgit-agent syntax --files src/zcl_my_class.clas.abap --json
```

---

## Use Case

Use `syntax` when:
- You want to check code BEFORE committing to git
- You want faster feedback than pull + inspect
- You're developing CLAS, INTF, or PROG objects
- You want to identify exactly which file has the error

### Typical Workflow

```bash
# Edit any class file (main, locals, or testclasses)
vim src/zcl_my_class.clas.locals_imp.abap

# Check syntax (auto-detects all companion files)
abapgit-agent syntax --files src/zcl_my_class.clas.locals_imp.abap

   Syntax check for 1 file(s)

   Auto-detected: zcl_my_class.clas.abap
   Auto-detected: zcl_my_class.clas.locals_def.abap
   Auto-detected: zcl_my_class.clas.testclasses.abap

❌ CLAS ZCL_MY_CLASS - Syntax check failed (1 error(s))

Errors:
────────────────────────────────────────────────────────────
  In: Local implementations (zcl_my_class.clas.locals_imp.abap)
  Line 24:
    "." expected after "LV_RESULT".

# Fix the error in zcl_my_class.clas.locals_imp.abap at line 24
vim src/zcl_my_class.clas.locals_imp.abap +24

# Check again
abapgit-agent syntax --files src/zcl_my_class.clas.locals_imp.abap
   ✅ CLAS ZCL_MY_CLASS - Syntax check passed

# Now safe to commit and push
git add . && git commit -m "Fix local implementation" && git push
abapgit-agent pull --files src/zcl_my_class.clas.abap
```

---

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
