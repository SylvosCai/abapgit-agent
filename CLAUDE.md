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
│   ├── command-client.js    # Command-based API client
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
Resource: ZCL_ABGAGT_RESOURCE_COMMAND → ZCL_ABGAGT_CMD_FACTORY → Command (for testing)
```

### ABAP Objects

| Object | Description |
|--------|-------------|
| `ZCL_ABGAGT_AGENT` | Main agent - handles pull, inspect, unit operations |
| `ZCL_ABGAGT_CMD_FACTORY` | Command factory - creates command instances dynamically |
| `ZCL_ABGAGT_COMMAND_PULL` | Pull command - implements ZIF_ABGAGT_COMMAND |
| `ZCL_ABGAGT_COMMAND_INSPECT` | Inspect command - implements ZIF_ABGAGT_COMMAND |
| `ZCL_ABGAGT_COMMAND_UNIT` | Unit command - implements ZIF_ABGAGT_COMMAND |
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

# Health check
abapgit-agent health

# Check configuration
abapgit-agent status
```

## Pull Command

### Description
Pull and activate ABAP objects from git repository.

### Usage
```bash
# Auto-detect git remote and branch from current directory
abapgit-agent pull

# Pull specific files only
abapgit-agent pull --files zcl_my_class.clas.abap,zif_my_intf.intf.abap

# Pull from specific branch
abapgit-agent pull --branch develop

# Pull from specific URL (useful for CI/CD)
abapgit-agent pull --url https://github.com/I045696/my-repo.git

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
  "gitPassword": "git-token",
  "useCommandApi": false
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
| `useCommandApi` | Use new `/command` endpoint | false |

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

### Feature Flag: useCommandApi

The `useCommandApi` option controls whether to use the new command-based API (`/command` endpoint) or the legacy endpoints (`/pull`, `/syntax-check`, `/unit`):

- `false` (default): Use legacy endpoints - stable, production-tested
- `true`: Use new command API - enables command factory pattern for extensibility

The new command API supports the same operations but routes them through a command factory pattern:
- `PULL` - Pull and activate ABAP objects
- `INSPECT` - Syntax check ABAP objects
- `UNIT` - Run unit tests

## Development Workflow

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

### Test Class Declaration
```abap
CLASS ltcl_util DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
```

### Test Methods
- Must have `FOR TESTING` keyword
- Can optionally include `RAISING <exception>`

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
