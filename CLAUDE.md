# ABAP Git Agent - CLI Tool Development

This is the **abapgit-agent** CLI tool project - a Node.js application for pulling and activating ABAP code from git repositories.

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

# Health check
abapgit-agent health

# Check configuration
abapgit-agent status
```

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

# With object breakdown by type
abapgit-agent tree --package $MY_PACKAGE --include-objects

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
| `--include-objects` | No | Include object counts breakdown by type |
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
| `--type` | No | Object type (CLAS, INTF, TABL, STRU, DTEL). Auto-detected from TADIR if not specified |
| `--json` | No | Output raw JSON only (for scripting) |

### Supported Object Types

| Type | Description |
|------|-------------|
| CLAS | Class |
| INTF | Interface |
| TABL | Table |
| STRU | Structure |
| DTEL | Data Element |

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

### Error Handling

| Error | Message |
|-------|---------|
| Object not found | `Object <name> not found` |
| Invalid object type | `Unsupported object type: <type>` |

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
```

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
2. Test locally: `node bin/abapgit-agent pull`
3. Test against real ABAP system
4. Commit and push

### ABAP Backend Development

1. Make changes to ABAP backend (abap/ folder)
2. Pull only changed files (faster):
   ```bash
   abapgit-agent pull --files abap/zcl_my_class.clas.abap
   ```
   Or pull all files:
   ```bash
   abapgit-agent pull
   ```
3. Commit and push
4. Deploy changes via abapGit to your SAP system

### Fast Iteration Workflow

For quick ABAP code changes:
1. Make small change to ABAP file
2. `git add <file> && git commit -m "fix"`
3. `abapgit-agent pull --files <file>` (seconds, not minutes)
4. Verify activation results
5. Repeat until done

## Creating CDS Views

For guidelines on creating CDS views and CDS view entities, see **ABAP Code Generation** below.

## For ABAP Code Generation

**NOTE**: This file is for developing the CLI tool itself. For guidelines on **generating ABAP code** for abapGit repositories, see `/abap/CLAUDE.md`. Copy that file to your ABAP repository root when setting up new projects.

## ABAP Unit Tests

### File Structure
- Main class: `zcl_xxx.clas.abap`
- Test class: `zcl_xxx.clas.testclasses.abap`
- No separate XML needed for test classes

### XML Configuration
Add `<WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>` to main class XML file:
```xml
<VSEOCLASS>
  <CLSNAME>ZCL_ABGAGT_UTIL</CLSNAME>
  ...
  <WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>
</VSEOCLASS>
```

### Test Class Naming
- Short name: `ltcl_<name>` (e.g., `ltcl_util`, not `ltcl_abgagt_util_test`)
- Must include `FINAL` keyword
- **CRITICAL: Class name MUST NOT exceed 30 characters!**
  - Example: `ltcl_cmd_inspect` (18 chars) is OK
  - `ltcl_zcl_abgagt_command_infect` (32 chars) is NOT OK

### Test Class Declaration
```abap
CLASS ltcl_util DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
```

### Test Methods
- Must have `FOR TESTING` keyword
- Can optionally include `RAISING <exception>`
- **CRITICAL: Method names MUST NOT exceed 30 characters!**
  - Example: `test_exec_multi_files` (20 chars) is OK
  - `test_exec_multiple_files` (25 chars) is OK
  - `test_exec_multiple_files_with_long_name` (40 chars) is NOT OK

### Setup Method
```abap
METHOD setup.
  mo_util = zcl_abgagt_util=>get_instance( ).
ENDMETHOD.
```

### Assertions
Use `CL_ABAP_UNIT_ASSERT` class:
- `assert_equals( act = lv_act exp = lv_exp msg = 'message' )`
- `assert_initial( act = lv_act msg = 'message' )`
- `assert_not_initial( act = lv_act msg = 'message' )`

### Method Call Format
- Instance methods: `mo_object->method( )`
- Class/static methods: `zcl_class=>method( )`

### DATA() vs VALUE #() Rule
**When assigning to a typed variable, ABAP can infer the type from the target.**

**CORRECT:**
```abap
" When target has explicit type, VALUE #() infers from it
DATA lt_so_class TYPE RANGE OF seoaliases-clsname.
lt_so_class = VALUE #( ( sign = 'I' option = 'EQ' low = lv_value ) ).

" Or append with inline VALUE
APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_value ) TO lt_so_class.
```

**WRONG (untyped variable):**
```abap
DATA(ls_range) = VALUE #( sign = 'I' option = 'EQ' low = lv_value ).  " Type unknown!
```

### Example Test Class
```abap
*----------------------------------------------------------------------*
*       CLASS ltcl_util DEFINITION
*----------------------------------------------------------------------*
CLASS ltcl_util DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS setup.
    DATA mo_util TYPE REF TO zcl_abgagt_util.

    METHODS parse_class_file FOR TESTING.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_util IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_util IMPLEMENTATION.

  METHOD setup.
    mo_util = zcl_abgagt_util=>get_instance( ).
  ENDMETHOD.

  METHOD parse_class_file.
    DATA lv_file TYPE string VALUE 'zcl_my_class.clas.abap'.
    DATA lv_obj_type TYPE string.
    DATA lv_obj_name TYPE string.

    mo_util->zif_abgagt_util~parse_file_to_object(
      EXPORTING iv_file = lv_file
      IMPORTING ev_obj_type = lv_obj_type
                ev_obj_name = lv_obj_name ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_obj_type
      exp = 'CLAS'
      msg = 'Object type should be CLAS' ).
  ENDMETHOD.

ENDCLASS.
```
