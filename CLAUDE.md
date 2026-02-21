# ABAP Git Agent - CLI Tool Development

This is the **abapgit-agent** CLI tool project - a Node.js application for pulling and activating ABAP code from git repositories.

## Quick Reference

```bash
# Quick commands
abapgit-agent pull                              # Pull and activate all
abapgit-agent pull --files file.clas.abap      # Pull specific file(s)
abapgit-agent inspect --files file.clas.abap    # Syntax check
abapgit-agent unit --files test.clas.testclasses.abap  # Run tests
abapgit-agent preview --objects TABLE           # Preview table data
abapgit-agent view --objects OBJ               # View object definition
abapgit-agent tree --package $PACKAGE          # Show package hierarchy
abapgit-agent ref "PATTERN"                    # Search ABAP reference (cheat sheets + guidelines)
```

## When Working on Unfamiliar ABAP Topics

**IMPORTANT**: When working on unfamiliar ABAP syntax, patterns, or APIs, ALWAYS use the `ref` command first:

```bash
# Search for a specific pattern (searches SAP cheat sheets + custom guidelines)
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "FILTER #"
abapgit-agent ref "VALUE #("

# Browse by topic
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql
abapgit-agent ref --topic internal-tables

# List all available topics
abapgit-agent ref --list-topics
```

This ensures you use correct ABAP syntax rather than guessing.

## Common Tasks

**Making code changes:**
```bash
1. Edit ABAP file(s)
2. git add && git commit && git push
3. abapgit-agent pull --files file.clas.abap
4. Check for errors in output
5. If "Error updating where-used list" → use inspect for details
```

**Exploring unknown tables:**
```bash
abapgit-agent preview --objects SFLIGHT              # See sample data
abapgit-agent view --objects SFLIGHT --type TABL    # See structure
```

## IMPORTANT: When Working on ABAP Files

When making changes to files in the `abap/` folder, **always read `abap/CLAUDE.md` first**.

This file contains critical ABAP development rules including:
- Never run inspect before commit/push/pull
- Always push to git BEFORE running pull
- Use inspect AFTER pull to check syntax
- ABAP naming conventions

## Project Structure

```
abapgit-agent/
├── bin/
│   └── abapgit-agent        # CLI entry point
├── src/
│   ├── agent.js             # Main agent class
│   ├── abap-client.js       # REST client for ABAP communication
│   ├── config.js            # Configuration management
│   ├── server.js            # HTTP server
│   └── logger.js            # Logging utilities
├── abap/                    # ABAP backend components
│   ├── zcl_abapgit_agent*.clas.abap    # Main agent class
│   ├── zif_abapgit_agent.intf.abap     # Agent interface
│   ├── zcl_abgagt_cmd_factory.clas.abap # Command factory
│   ├── zcl_abgagt_command_*.clas.abap   # Command implementations
│   ├── zif_abgagt_command.intf.abap     # Command interface
│   ├── zcl_abgagt_resource_*.clas.abap  # REST resource handlers
│   └── CLAUDE.md            # ABAP project guidelines
└── tests/
```

## ABAP Architecture

### Call Stack
```
CLI (bin/abapgit-agent)
    ↓
REST Client (src/abap-client.js)
    ↓
ABAP REST Handler (ZCL_ABGAGT_REST_HANDLER)
    ↓
Resource: ZCL_ABGAGT_RESOURCE_PULL → ZCL_ABGAGT_CMD_FACTORY → ZCL_ABGAGT_COMMAND_PULL → ZCL_ABGAGT_AGENT
Resource: ZCL_ABGAGT_RESOURCE_INSPECT → ZCL_ABGAGT_CMD_FACTORY → ZCL_ABGAGT_COMMAND_INSPECT → ZCL_ABGAGT_AGENT
Resource: ZCL_ABGAGT_RESOURCE_UNIT → ZCL_ABGAGT_CMD_FACTORY → ZCL_ABGAGT_COMMAND_UNIT → ZCL_ABGAGT_AGENT
Resource: ZCL_ABGAGT_RESOURCE_TREE → ZCL_ABGAGT_CMD_FACTORY → ZCL_ABGAGT_COMMAND_TREE → ZCL_ABGAGT_AGENT
```

### ABAP Objects

| Object | Description |
|--------|-------------|
| `ZCL_ABGAGT_AGENT` | Main agent - handles pull, inspect, unit operations |
| `ZCL_ABGAGT_CMD_FACTORY` | Command factory - creates command instances dynamically |
| `ZCL_ABGAGT_COMMAND_PULL` | Pull command - implements ZIF_ABGAGT_COMMAND |
| `ZCL_ABGAGT_COMMAND_INSPECT` | Inspect command - implements ZIF_ABGAGT_COMMAND |
| `ZCL_ABGAGT_COMMAND_UNIT` | Unit command - implements ZIF_ABGAGT_COMMAND |
| `ZCL_ABGAGT_COMMAND_TREE` | Tree command - displays package hierarchy |
| `ZIF_ABGAGT_COMMAND` | Command interface with constants |
| `ZIF_ABGAGT_CMD_FACTORY` | Factory interface |

## CLI Commands

```bash
# Pull and activate from current git repo
abapgit-agent pull

# Pull specific files only (fast - recommended for iterative development)
abapgit-agent pull --files <file1>,<file2>,...

# Pull from specific branch
abapgit-agent pull --branch <branch>

# Pull from specific URL
abapgit-agent pull --url <git-url>

# Syntax check an ABAP file (IMPORTANT for debugging activation errors)
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Run unit tests for ABAP test classes
abapgit-agent unit --files abap/zcl_my_test.clas.testclasses.abap

# Display package hierarchy tree
abapgit-agent tree --package $MY_PACKAGE

# View ABAP object definitions from ABAP system
abapgit-agent view --objects ZCL_MY_CLASS
abapgit-agent view --objects ZIF_MY_INTERFACE --type INTF
abapgit-agent view --objects ZCL_CLASS1,ZCL_CLASS2 --json

# Preview table/CDS view data
abapgit-agent preview --objects SFLIGHT
abapgit-agent preview --objects ZC_MY_CDS_VIEW --type DDLS
abapgit-agent preview --objects SFLIGHT --columns CARRID,CONNID,PRICE --limit 20
abapgit-agent preview --objects SFLIGHT --where "CARRID = 'AA'"
abapgit-agent preview --objects SFLIGHT --vertical
abapgit-agent preview --objects SFLIGHT --compact

# Health check
abapgit-agent health

# Check configuration
abapgit-agent status
```
Note: All ABAP commands automatically check CLI/ABAP API version compatibility.

## Pull Command

### Description
Pull and activate ABAP objects from git repository.

### Rule: Pull All Changed Files Together

**CRITICAL**: When multiple ABAP files are changed, pull them ALL together in a single command:

```bash
# WRONG - Pull files one by one (may cause activation issues)
abapgit-agent pull --files zcl_class1.clas.abap
abapgit-agent pull --files zcl_class2.clas.abap
abapgit-agent pull --files zif_interface1.intf.abap

# CORRECT - Pull all changed files together
abapgit-agent pull --files zcl_class1.clas.abap,zcl_class2.clas.abap,zif_interface1.intf.abap
```

**Why?** ABAP objects often have dependencies on each other. Pulling separately can cause activation errors if dependent objects haven't been activated yet. Pulling together ensures the ABAP system processes all changes atomically.

### Usage
```bash
# Auto-detect git remote and branch from current directory
abapgit-agent pull

# Pull specific files only
abapgit-agent pull --files zcl_my_class.clas.abap,zif_my_intf.intf.abap

# Pull from specific branch
abapgit-agent pull --branch develop

# Pull from specific URL (useful for CI/CD)
abapgit-agent pull --url https://github.com/org/my-repo.git

# Combined options
abapgit-agent pull --branch develop --files src/zcl_my_class.clas.abap
```

### File Format
Files are parsed to extract `(obj_type, obj_name)`:
- `zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS
- `zif_my_intf.intf.abap` → INTF, ZIF_MY_INTF
- `src/zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS (subdirectory support)

### Output
```
✅ Pull completed successfully!
   Job ID: CAIS20260208115649
   Message: Pull completed successfully

📋 Pull Log (N messages):
───────────────────────────────────────────────────────────────────────────────
Icon │ Object                      │ Message
...

📦 Activated Objects (N):
───────────────────────────────────────────────────────────────────────────────
✅ CLAS ZCL_MY_CLASS
...

❌ Failed Objects Log (M entries):
───────────────────────────────────────────────────────────────────────────────
❌ CLAS ZCL_MY_CLASS: Error message text
Exception: Exception details
```

### Key Behaviors
1. **Activated Objects** - Only includes objects that completed successfully (no errors in log)
2. **Failed Objects Log** - Shows all error messages (duplicates allowed for multiple errors per object)
3. **Error Details** - When errors occur, displays error detail section at the top

## Health Check

### Description
Check if the ABAP REST API is healthy.

### Usage
```bash
abapgit-agent health
```

### Output
```json
{
  "status": "healthy",
  "abap": "connected",
  "version": "1.0.0"
}
```

## Inspect Command

### Description
Run syntax check for ABAP objects. Supports both regular ABAP objects (classes, interfaces, programs) and CDS views (DDLS).

### Usage
```bash
# Inspect single file
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Inspect multiple files
abapgit-agent inspect --files abap/zcl_class1.clas.abap,abap/zcl_class2.clas.abap

# Inspect CDS view
abapgit-agent inspect --files abap/zc_my_view.ddls.asddls

# Inspect mixed file types (DDLS + CLAS)
abapgit-agent inspect --files abap/zc_my_view.ddls.asddls,abap/zcl_my_class.clas.abap

# Inspect with specific Code Inspector variant
abapgit-agent inspect --files abap/zcl_my_class.clas.abap --variant ALL_CHECKS

# Inspect with no variant (uses default SAP standard checks)
abapgit-agent inspect --files abap/zcl_my_class.clas.abap --variant EMPTY
```

### Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--files` | Yes | Comma-separated list of ABAP files to inspect |
| `--variant` | No | Code Inspector variant name (e.g., `ALL_CHECKS`, `EMPTY`) |

### Supported Object Types

| Type | Description | Validation Method |
|------|-------------|------------------|
| CLAS | Class | Code Inspector (SCI) |
| INTF | Interface | Code Inspector (SCI) |
| PROG | Program | Code Inspector (SCI) |
| FUGR | Function Group | Code Inspector (SCI) |
| DDLS | CDS View/Entity | DDL Handler (CL_DD_DDL_HANDLER_FACTORY) |

### Output

**Passed:**
```
✅ CLAS ZCL_MY_CLASS - Syntax check passed
```

**With Warnings:**
```
⚠️  DDLS ZC_MY_VIEW - Syntax check passed with warnings (4):

Warnings:
────────────────────────────────────────────────────────────
  Line 9 : ParentPackage
  Line 11 : SoftwareComponent
```

**Failed:**
```
❌ DDLS ZC_MY_VIEW - Syntax check failed (1 error(s)):

Errors:
────────────────────────────────────────────────────────────
  Line 21, Column 12: Error message text
```

### Key Behaviors

1. **Multiple files in one request** - All files are sent in a single API call for better performance
2. **CDS View validation** - Uses `CL_DD_DDL_HANDLER_FACTORY` to validate CDS views
3. **Check inactive version first** - For CDS views, checks the inactive version first (`get_state = 'M'`), then falls back to active version
4. **Detailed error messages** - Uses `get_errors()` and `get_warnings()` methods from the exception to get detailed information
5. **Per-object results** - Returns results for each object individually

### File Format
Files are parsed to extract `(obj_type, obj_name)`:
- `zcl_my_class.clas.abap` → CLAS, ZCL_MY_CLASS
- `zc_my_view.ddls.asddls` → DDLS, ZC_MY_VIEW

## Unit Command

### Description
Run AUnit tests for ABAP test classes and display detailed results including failed test methods with error messages.

### Usage
```bash
# Run unit tests for a single test class file
abapgit-agent unit --files abap/zcl_my_test.clas.testclasses.abap

# Run unit tests for multiple test class files
abapgit-agent unit --files abap/zcl_test1.clas.testclasses.abap,abap/zcl_test2.clas.testclasses.abap

# Run unit tests for a specific package
abapgit-agent unit --package $MY_PACKAGE
```

### File Format
The command accepts test class files (`.clas.testclasses.abap`):
- `zcl_my_test.clas.testclasses.abap` → CLAS, ZCL_MY_TEST
- `src/tests/zcl_my_test.clas.testclasses.abap` → CLAS, ZCL_MY_TEST (with path)

### Output
```
✅ ZCL_MY_TEST - All tests passed
   Tests: 10 | Passed: 10 | Failed: 0
```

When tests fail:
```
❌ ZCL_MY_TEST - Tests failed
   Tests: 10 | Passed: 8 | Failed: 2
   ✗ ZCL_MY_TEST=>TEST_METHOD_1: Error description
   ✗ ZCL_MY_TEST=>TEST_METHOD_2: Another error

Failed Tests:
────────────────────────────────────────────────────────────────────────────────
   ✗ ZCL_MY_TEST=>TEST_METHOD_1
     Error: Expected X but got Y
```

### Error Details
When a test fails, the output includes:
- **Test Class**: The class containing the failed test
- **Method**: The failed test method name (with `=>` notation)
- **Error Kind**: Type of error (e.g., 'ERROR', 'FAILURE')
- **Error Text**: Detailed error message from AUnit

### Response JSON Structure
```json
{
  "success": "X",
  "message": "2 of 10 tests failed",
  "test_count": 10,
  "passed_count": 8,
  "failed_count": 2,
  "errors": [
    {
      "class_name": "ZCL_MY_TEST",
      "method_name": "TEST_METHOD_1",
      "error_kind": "ERROR",
      "error_text": "Expected X but got Y"
    }
  ]
}
```

### Implementation
The unit command uses `CL_SUT_AUNIT_RUNNER` to execute AUnit tests:
- `CL_SUT_AUNIT_RUNNER=>S_CREATE` - Create test runner
- `run()` - Execute tests
- `str_results` - Get test statistics (cnt_testmethods, cnt_ok_methods, cnt_error_methods)
- `tab_objects` - Get detailed results with nested structure:
  - `TAB_TESTCLASSES` → `TAB_METHODS` → `STR_ERROR` → `STR_ERROR_CORE`

## Tree Command

### Description
Display the package hierarchy tree from an ABAP system, showing parent packages, sub-packages, and object counts.

### Usage
```bash
# Basic usage
abapgit-agent tree --package $MY_PACKAGE

# With object type breakdown
abapgit-agent tree --package $MY_PACKAGE --include-types

# Limit depth (default: 3, max: 10)
abapgit-agent tree --package $MY_PACKAGE --depth 2

# JSON output for scripting
abapgit-agent tree --package $MY_PACKAGE --json
```

### Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--package` | Yes | ABAP package name (e.g., `$ZMY_PACKAGE`, `ZMY_PACKAGE`) |
| `--depth` | No | Maximum depth to traverse (default: 3, max: 10) |
| `--include-types` | No | Include object type counts (e.g., CLAS=10 INTF=2) |
| `--json` | No | Output raw JSON only (for scripting) |

### Output

**Human-readable with AI metadata:**
```
🌳 Package Tree: $ZMAIN_PACKAGE

📦 $ZMAIN_PACKAGE (Main Package)
   ├─ 📦 $ZMAIN_SUB1 (Sub Package 1)
   │    ├─ 📦 $ZMAIN_SUB1_A (Sub Package 1A)
   │    └─ 📦 $ZMAIN_SUB1_B (Sub Package 1B)
   └─ 📦 $ZMAIN_SUB2 (Sub Package 2)

📊 Summary
PACKAGES: 4
OBJECTS: 127

<!-- AI_METADATA_START -->
{"package":"$ZMAIN_PACKAGE","parent":"$ZSAP_BASE","total_packages":4,"total_objects":127}
<!-- AI_METADATA_END -->
```

**With object breakdown:**
```
📊 Summary
PACKAGES: 4
OBJECTS: 127
TYPES: CLAS=10 INTF=2 PROG=11 FUGR=1 TABL=3
```

### JSON Output
```json
{
  "SUCCESS": true,
  "COMMAND": "TREE",
  "PACKAGE": "$ZMAIN_PACKAGE",
  "PARENT_PACKAGE": "$ZSAP_BASE",
  "NODES": [
    {
      "PACKAGE": "$ZMAIN_PACKAGE",
      "PARENT": "",
      "DESCRIPTION": "$ZMAIN_PACKAGE",
      "DEPTH": 0,
      "OBJECT_COUNT": 11
    }
  ],
  "TOTAL_PACKAGES": 4,
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

### Error Handling

| Error | Message |
|-------|---------|
| Package not found | `Package <name> does not exist in the system` |
| Invalid package name | `Invalid package name: <name>` |
| Access denied | `Access denied to package information` |
| Depth too large | `Depth value too large (max: 10)` |

## View Command

### Description
View ABAP object definitions directly from the ABAP system without pulling from git. **This is the PRIMARY way to explore unfamiliar ABAP objects** - tables, structures, classes, interfaces, and data elements.

**When you encounter an unknown table or structure, use this command instead of guessing!**

### Explore Unknown Tables/Structures

```bash
# View table fields - see all columns, keys, and descriptions
abapgit-agent view --objects ZMY_TABLE --type TABL

# View structure components
abapgit-agent view --objects ZMY_STRUCT --type STRU

# View data element type information
abapgit-agent view --objects ZMY_DTEL --type DTEL

# View CDS view definition
abapgit-agent view --objects ZC_MY_CDS_VIEW --type DDLS

# View class interface and methods
abapgit-agent view --objects ZCL_UNKNOWN_CLASS

# View interface definition
abapgit-agent view --objects ZIF_UNKNOWN_INTERFACE
```

### When to Use View Command

AI assistant SHOULD call `view` command when:

- User asks to "check", "look up", or "explore" an unfamiliar object
- Working with a table/structure and you don't know the field names/types
- Calling a class/interface method and you don't know the parameters
- User provides an object name that may not exist in the git repository
- You need to verify an object exists before using it

**Example workflow:**
```
User: "Check if SFLIGHT table has a PRICE field"

Assistant: <calls `abapgit-agent view --objects SFLIGHT --type TABL`>
→ Shows table structure with all fields including PRICE
```

### Usage
```bash
# View single object (auto-detect type from TADIR)
abapgit-agent view --objects ZCL_MY_CLASS
abapgit-agent view --objects ZIF_MY_INTERFACE

# View with explicit type
abapgit-agent view --objects ZCL_MY_CLASS --type CLAS
abapgit-agent view --objects ZIF_MY_INT --type INTF
abapgit-agent view --objects ZMY_TABLE --type TABL

# View multiple objects
abapgit-agent view --objects ZCL_CLASS1,ZCL_CLASS2,ZIF_INTERFACE1

# JSON output (for scripting/AI processing)
abapgit-agent view --objects ZCL_MY_CLASS --json
```

### Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--objects` | Yes | Comma-separated list of object names (e.g., `ZCL_MY_CLASS,ZIF_MY_INTERFACE`) |
| `--type` | No | Object type (CLAS, INTF, TABL, STRU, DTEL, TTYP, DDLS). Auto-detected from TADIR if not specified |
| `--json` | No | Output raw JSON only (for scripting) |

### Supported Object Types

| Type | Description |
|------|-------------|
| CLAS | Class |
| INTF | Interface |
| TABL | Table |
| STRU | Structure |
| DTEL | Data Element |
| TTYP | Table Type |
| DDLS | CDS View/Entity |

**Note:** Object type is automatically detected from TADIR. Use `--type` only when you know the type and want to override auto-detection.

### Output

**Human-readable:**
```
📖 ZCL_MY_CLASS (Class)
   Class ZCL_MY_CLASS in $PACKAGE

CLASS zcl_my_class DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_my_interface.
    METHODS: constructor,
      get_value RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.
```

**JSON Output:**
```json
{
  "success": true,
  "command": "VIEW",
  "message": "Retrieved object(s)",
  "objects": [
    {
      "name": "ZCL_MY_CLASS",
      "type": "CLAS",
      "type_text": "Class",
      "description": "Class ZCL_MY_CLASS in $PACKAGE",
      "source": "CLASS zcl_my_class DEFINITION PUBLIC...",
      "not_found": false
    }
  ],
  "summary": { "total": 1 }
}
```

**Table Type Output:**
```
📖 ZMY_TTYP (Table Type)
   Table Type ZMY_TTYP in $PACKAGE

   Line Type: ZMY_STRUCTURE
   Access Mode: STANDARD
   Key Definition: WITH KEY
```

**CDS View Output:**
```
📖 ZC_MY_CDS_VIEW (CDS View)
   CDS View ZC_MY_CDS_VIEW in $PACKAGE

@AbapCatalog.sqlViewName: 'ZCMYVIEW'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'My CDS View'
define view ZC_MY_CDS_VIEW as select from tdevc
{
  key devclass as Devclass,
      parentcl as ParentPackage,
      ctext    as Description
}
where devclass not like '$%'
```

### Error Handling

| Error | Message |
|-------|---------|
| Object not found | `Object <name> not found` |
| Invalid object type | `Unsupported object type: <type>` |

## Preview Command

### Description
Preview data from ABAP tables or CDS views directly from the ABAP system. This command retrieves sample data rows to help developers understand table/view contents without needing to query manually.

**This is the PRIMARY way to explore table and CDS view DATA.**

### Usage
```bash
# Preview table data (auto-detect type)
abapgit-agent preview --objects SFLIGHT

# Preview CDS view data
abapgit-agent preview --objects ZC_MY_CDS_VIEW --type DDLS

# Preview with explicit type
abapgit-agent preview --objects SFLIGHT --type TABL

# Preview with row limit
abapgit-agent preview --objects SFLIGHT --limit 20

# Preview with WHERE clause filter
abapgit-agent preview --objects SFLIGHT --where "CARRID = 'AA'"

# Preview specific columns only
abapgit-agent preview --objects SFLIGHT --columns CARRID,CONNID,FLDATE,PRICE

# Vertical format (for wide tables)
abapgit-agent preview --objects SFLIGHT --vertical

# JSON output (for scripting/AI processing)
abapgit-agent preview --objects SFLIGHT --json
```

### Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--objects` | Yes | Comma-separated list of table/view names |
| `--type` | No | Object type (TABL, DDLS). Auto-detected from TADIR if not specified |
| `--limit` | No | Maximum rows to return (default: 10, max: 100) |
| `--where` | No | WHERE clause filter (e.g., `CARRID = 'AA'`) |
| `--columns` | No | Comma-separated column names to display |
| `--vertical` | No | Show data in vertical format (one field per line) |
| `--compact` | No | Truncate values to fit columns |
| `--json` | No | Output raw JSON only |

### Output

**Default (first 6 columns shown with indicator):**
```
📊 Preview: SFLIGHT (Table)

┌──────────┬────────┬──────────┬───────────┬─────────┬─────────┐
│ CARRID   │ CONNID │ FLDATE   │ PRICE     │ CURRENCY│ PLANETYPE│
├──────────┼────────┼──────────┼───────────┼─────────┼─────────┤
│ AA       │ 0017   │ 20240201 │    422.94 │ USD     │ 747-400 │
│ AA       │ 0017   │ 20240202 │    422.94 │ USD     │ 747-400 │
└──────────┴────────┴──────────┴───────────┴─────────┴─────────┘

Showing 2 of 10 rows
⚠️  Note: 3 more columns hidden (SEATSMAX, SEATSOCC, PAYMENTSUM)
   Use --columns to select specific columns
   Use --json for full data
```

**With WHERE Filter:**
```
📊 Preview: SFLIGHT (filtered)

┌──────────┬────────┬──────────┬─────────┐
│ CARRID   │ CONNID │ FLDATE   │ PRICE   │
├──────────┼────────┼──────────┼─────────┤
│ AA       │ 0017   │ 20240201 │  422.94 │
│ AA       │ 0017   │ 20240202 │  422.94 │
└──────────┴────────┴──────────┴─────────┘

WHERE: CARRID = 'AA'
```

**Vertical Format (for wide tables):**
```
📊 Preview: SFLIGHT (1 of 10 rows, vertical)

Row 1:
────────────────────────────────────────────────────────────
  CARRID:      AA
  CONNID:      0017
  FLDATE:      20240201
  PRICE:       422.94
  CURRENCY:    USD
  PLANETYPE:   747-400
  SEATSMAX:    400
  SEATSOCC:    350
  PAYMENTSUM:  145000
────────────────────────────────────────────────────────────
```

### JSON Output
```json
{
  "SUCCESS": true,
  "COMMAND": "PREVIEW",
  "OBJECTS": [
    {
      "NAME": "SFLIGHT",
      "TYPE": "TABL",
      "TYPE_TEXT": "Table",
      "ROW_COUNT": 2,
      "TOTAL_ROWS": 10,
      "ROWS": [
        { "CARRID": "AA", "CONNID": "0017", "FLDATE": "20240201", "PRICE": "422.94", ... }
      ],
      "FIELDS": [
        { "FIELD": "CARRID", "TYPE": "CHAR", "LENGTH": 3 },
        { "FIELD": "PRICE", "TYPE": "CURR", "LENGTH": 16 }
      ],
      "COLUMNS_DISPLAYED": 6,
      "COLUMNS_HIDDEN": ["SEATSMAX", "SEATSOCC", "PAYMENTSUM"]
    }
  ],
  "SUMMARY": { "TOTAL_OBJECTS": 1, "TOTAL_ROWS": 2 },
  "ERROR": ""
}
```

### Error Handling

| Error | Message |
|-------|---------|
| Table not found | `Table not found: Z_NONEXISTENT` |
| CDS View not found | `CDS View not found: Z_NONEXISTENT` |
| Access denied | `Access denied to table: SFLIGHT` |
| Invalid WHERE clause | `Invalid WHERE clause: <reason>` |

### Auto-Detection Rules

| Object Name Pattern | Default Type |
|---------------------|--------------|
| `ZC_*` or `zc_*` | DDLS (CDS View) |
| Other | TABL (Table) |

## Status Check

### Description
Check if ABAP integration is configured for the current repository.

### Usage
```bash
abapgit-agent status
```

### Output
```
✅ ABAP Git Agent is ENABLED
   Config location: /path/to/repo/.abapGitAgent
```

Or if not configured:
```
❌ ABAP Git Agent is NOT configured
```

## Configuration

### File-based (.abapGitAgent)
Create `.abapGitAgent` in repository root:
```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "gitUsername": "git-username",
  "gitPassword": "git-token"
}
```

### Configuration Options

| Option | Description | Default |
|--------|-------------|---------|
| `host` | SAP system hostname | Required |
| `sapport` | SAP port (usually 443) | 443 |
| `client` | SAP client number | 100 |
| `user` | SAP username | Required |
| `password` | SAP password | Required |
| `language` | SAP language | EN |
| `gitUsername` | Git username/token | Optional |
| `gitPassword` | Git password/token | Optional |
| `transport` | Default transport request for pull | Optional |

### Environment Variables
```bash
export ABAP_HOST="your-sap-system.com"
export ABAP_PORT=443
export ABAP_CLIENT="100"
export ABAP_USER="TECH_USER"
export ABAP_PASSWORD="your-password"
export ABAP_LANGUAGE="EN"
export GIT_USERNAME="git-username"
export GIT_PASSWORD="git-token"
export ABAP_TRANSPORT="DEVK900001"
```

### Transport Request Precedence

When running `pull` command, the transport request is determined in this order:

| Priority | Source | Example |
|----------|--------|---------|
| 1 | CLI `--transport` argument | `--transport DEVK900001` |
| 2 | Config file `transport` | `"transport": "DEVK900001"` |
| 3 | Environment variable `ABAP_TRANSPORT` | `export ABAP_TRANSPORT="DEVK900001"` |
| 4 (default) | Not set | abapGit creates/uses default |

## Troubleshooting

**Pull fails with "Error updating where-used list":**
- This means SYNTAX ERROR in the object
- Run `abapgit-agent inspect --files <file>` for detailed errors

**Pull shows "Failed Objects" > 0:**
- Objects failed to activate - check the error messages
- Fix syntax errors and pull again

**Preview/View command fails:**
- Check table/view exists in the ABAP system
- Verify credentials in `.abapGitAgent`

**"Table or view not found":**
- Table may not exist or may have different name
- Use `view` command to check available tables

**Terminal width issues with preview:**
- Use `--columns` to specify exact columns
- Use `--vertical` for wide tables

## Development Workflow

### Exploring Unknown ABAP Objects

**Check package structure:**
```bash
abapgit-agent tree --package $MY_PACKAGE
```

**Before working with an unfamiliar table, structure, class, or interface:**

```bash
# Don't guess! Use view command to explore:

# Check table structure
abapgit-agent view --objects ZMY_TABLE --type TABL

# Check structure components
abapgit-agent view --objects ZMY_STRUCT --type STRU

# Check class methods and interface
abapgit-agent view --objects ZCL_UNKNOWN_CLASS

# Check interface definition
abapgit-agent view --objects ZIF_UNKNOWN_INTERFACE

# Check data element type
abapgit-agent view --objects ZMY_DTEL --type DTEL

# JSON output for programmatic use
abapgit-agent view --objects ZMY_TABLE --type TABL --json
```

### CLI Tool Development

1. Make changes to CLI code (JavaScript)
2. Test locally:
   ```bash
   node bin/abapgit-agent --help
   node bin/abapgit-agent <command> --help
   ```
3. Test against real ABAP system (requires configured `.abapGitAgent`)
4. Commit and push

### ABAP Backend Development

For ABAP backend development workflow, see `/abap/CLAUDE.md`.

## For ABAP Code Generation

**NOTE**: This file is for developing the CLI tool itself. For guidelines on **generating ABAP code** for abapGit repositories, see `/abap/CLAUDE.md`. Copy that file to your ABAP repository root when setting up new projects.
