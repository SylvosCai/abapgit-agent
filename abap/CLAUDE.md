# ABAP Project Guidelines - Template

This file provides guidelines for **generating ABAP code** in abapGit repositories.

**Use this file as a template**: Copy it to your ABAP repository root when setting up new projects with Claude Code.

## Quick Reference

```bash
# After editing ABAP files:
git add . && git commit -m "feat: description" && git push
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# If pull fails with syntax error:
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Explore tables/views:
abapgit-agent preview --objects ZTABLE
abapgit-agent view --objects ZTABLE --type TABL
abapgit-agent tree --package $MY_PACKAGE
```

## Common Workflow

1. Generate/edit ABAP code
2. Push to git: `git add . && git commit && git push`
3. Activate in ABAP: `abapgit-agent pull --files file.clas.abap`
4. Check for errors - fix if needed
5. Repeat

## ABAP SQL Best Practices

When writing ABAP SQL (Open SQL) queries, follow these rules:

### 1. Host Variables - Use @ Prefix

Use `@` prefix for host variables in ABAP SQL:

```abap
" Correct
SELECT * FROM tadir WHERE devclass = @lv_package.

" Wrong - no @ prefix
SELECT * FROM tadir WHERE devclass = lv_package.
```

### 2. Range Tables for IN Clauses

When filtering with `IN`, use a range table with `@` prefix:

```abap
" Define range table
DATA lt_type_range TYPE RANGE OF tadir-object.
ls_type-sign = 'I'.
ls_type-option = 'EQ'.
ls_type-low = 'CLAS'.
APPEND ls_type TO lt_type_range.

" Use with @ prefix
SELECT object, obj_name FROM tadir
  WHERE object IN @lt_type_range
  INTO TABLE @lt_objects.
```

### 3. SELECT Statement Clause Order

The correct sequence is:
```
SELECT → FROM → WHERE → ORDER BY → INTO → UP TO → OFFSET
```

```abap
SELECT object, obj_name FROM tadir
  WHERE devclass = @lv_package
    AND object IN @lt_type_range
  ORDER BY object, obj_name
  INTO TABLE @lt_objects
  UP TO @lv_limit ROWS
  OFFSET @lv_offset.
```

### 4. Fixed Point Arithmetic (FIXPT)

For numeric operations in ABAP SQL (especially with UP TO/OFFSET), enable FIXPT in the class XML:

```xml
<VSEOCLASS>
  <FIXPT>X</FIXPT>
</VSEOCLASS>
```

### 5. Field Separation

Always separate fields with commas in SELECT:

```abap
" Correct
SELECT object, obj_name FROM tadir ...

" Wrong - missing comma
SELECT object obj_name FROM tadir ...
```

## ABAP Syntax Validation

This is an ABAP project. **Do not attempt local syntax validation** - ABAP code can only be validated in an SAP system.

**To validate ABAP code:**

1. After generating code, push changes to git
2. Pull only changed files (fast):
   ```bash
   abapgit-agent pull --files abap/zcl_my_class.clas.abap
   ```
   Or pull all files:
   ```bash
   abapgit-agent pull
   ```
3. Review activation results carefully
4. **"Error updating where-used list" = SYNTAX ERROR** - This is NOT a warning!
5. If Failed Objects > 0, there are syntax errors - fix them before proceeding

## Inspect Command (Syntax Check)

Use the `inspect` command to perform syntax validation on ABAP objects and CDS views.

### Usage
```bash
# Syntax check single file
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Syntax check multiple files
abapgit-agent inspect --files abap/zcl_class1.clas.abap,abap/zcl_class2.clas.abap

# Syntax check CDS view
abapgit-agent inspect --files abap/zc_my_view.ddls.asddls
```

### Supported Object Types

| Type | Description | Validation Method |
|------|-------------|------------------|
| CLAS | Class | Code Inspector (SCI) |
| INTF | Interface | Code Inspector (SCI) |
| PROG | Program | Code Inspector (SCI) |
| DDLS | CDS View/Entity | DDL Handler |

### CDS Views (DDLS) Validation

For CDS views, the inspect command uses `CL_DD_DDL_HANDLER_FACTORY`:
- Checks **inactive version first** (`get_state = 'M'`)
- Falls back to active version if no inactive version exists
- Uses `get_errors()` and `get_warnings()` methods for detailed error information

### Examples

**Passed:**
```
✅ CLAS ZCL_MY_CLASS - Syntax check passed
```

**With Warnings:**
```
⚠️  DDLS ZC_MY_VIEW - Syntax check passed with warnings (4):
  Line 9 : ParentPackage
  Line 11 : SoftwareComponent
```

**Failed:**
```
❌ DDLS ZC_MY_VIEW - Syntax check failed (1 error(s)):
  Line 21, Column 12: Error message text
```

## Fast Iteration Workflow

For quick ABAP code changes:

```bash
# 1. Make small change to ABAP file
# 2. Commit and push to git
git add abap/zcl_my_class.clas.abap
git commit -m "fix: ..."
git push    # CRITICAL: Push BEFORE pulling!

# 3. Pull only changed file (seconds, not minutes)
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# 4. Check for syntax errors (if pull shows "Error updating where-used list")
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# 5. Repeat until done
```

### Transport Request Support

When working with development packages that require transport requests:

```bash
# Pull with specific transport request
abapgit-agent pull --files abap/zcl_my_class.clas.abap --transport DEVK900001

# Pull all files with transport request
abapgit-agent pull --transport DEVK900001
```

If `--transport` is not specified, abapGit will use its default behavior (create a new transport or use the default transport for the user).

**Important:** Only run `abapgit-agent pull` when ABAP code has actually changed. If you're only modifying JavaScript, JSON, or markdown files, skip the pull step.

**Syntax Check Command:**
When pull shows "Error updating where-used list", use:
```bash
abapgit-agent inspect --files abap/zcl_my_class.clas.abap
```
This will show detailed syntax errors including line numbers and error messages.

## View ABAP Object Definitions

Use the `view` command to inspect ABAP object definitions (classes, interfaces, tables, data elements) directly from the ABAP system. **Very useful when working with unfamiliar tables** - shows complete structure including fields, types, and descriptions.

```bash
# View a table definition
abapgit-agent view --objects ZMY_TABLE --type TABL

# View a data element definition
abapgit-agent view --objects ZMY_DTEL --type DTEL

# View a class definition
abapgit-agent view --objects ZCL_MY_CLASS

# View an interface definition
abapgit-agent view --objects ZIF_MY_INTERFACE

# View a table type definition
abapgit-agent view --objects ZMY_TTYP --type TTYP

# View multiple objects
abapgit-agent view --objects ZCL_CLASS1,ZCL_CLASS2,ZIF_INTERFACE1

# JSON output for scripting
abapgit-agent view --objects ZMY_TABLE --type TABL --json
```

### When to Use View Command

AI assistant SHOULD call `view` command when:

- User asks to "check", "look up", or "explore" an unfamiliar object
- Working with a table/structure and you don't know the field names/types
- Calling a class/interface method and you don't know the parameters
- User provides an object name that may not exist in the git repository

**Example workflow:**
```
User: "Check if SFLIGHT table has a PRICE field"

Assistant: <calls `abapgit-agent view --objects SFLIGHT --type TABL`>
→ Shows table structure with all fields including PRICE
```

## Explore Package Structure

Use the `tree` command to display package hierarchy from ABAP system:

```bash
# Display package hierarchy
abapgit-agent tree --package $MY_PACKAGE

# With object counts
abapgit-agent tree --package $MY_PACKAGE --include-objects

# Limit depth
abapgit-agent tree --package $MY_PACKAGE --depth 2

# JSON output for scripting
abapgit-agent tree --package $MY_PACKAGE --json
```

**Table Output Example:**
```
TABLE ZCAIS_D1:
|----------------+-----+----------------+----------+----------------+----------------------|
| Field          | Key | Type           |   Length | Data Elem      | Description          |
|----------------+-----+----------------+----------+----------------+----------------------|
| CLIENT         | X   | CLNT           |        3 | MANDT          | Client               |
| ID             | X   | NUMC           |        4 | NUMC4          | Count parameters     |
| NAME           |     | CHAR           |       50 | CHAR50         | Comment              |
|----------------+-----+----------------+----------+----------------+----------------------|
```

**Data Element Output Example:**
```
DATA ELEMENT ZMY_DTEL:
|----------------+----------------+----------+----------------+----------------------|
| Field          | Type           |   Length | Domain         | Description          |
|----------------+----------------+----------+----------------+----------------------|
| DOMAIN         | CHAR           |       20 | ZMY_DOMAIN     | Domain: ZMY_DOMAIN   |
| DATA_TYPE      | CHAR           |       10 |                | ABAP Type: CHAR      |
| LENGTH         | NUMC           |        5 |                | Length: 20           |
|----------------+----------------+----------+----------------+----------------------|
```

**Table Type Output Example:**
```
📖 ZMY_TTYP (Table Type)
   Table Type ZMY_TTYP in $PACKAGE

   Line Type: ZMY_STRUCTURE
   Access Mode: STANDARD
   Key Definition: WITH KEY
```

## CLI Commands Reference

This section documents the available CLI commands for ABAP development.

### Pull Command

Pull and activate ABAP objects from git repository.

```bash
# Pull all files
abapgit-agent pull

# Pull specific files (faster)
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# Pull with transport request
abapgit-agent pull --files abap/zcl_my_class.clas.abap --transport DEVK900001
```

### Inspect Command

Run syntax check on ABAP objects.

```bash
# Syntax check single file
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Syntax check multiple files
abapgit-agent inspect --files abap/zcl_class1.clas.abap,abap/zcl_class2.clas.abap

# Syntax check CDS view
abapgit-agent inspect --files abap/zc_my_view.ddls.asddls
```

### Unit Command

Run ABAP unit tests.

```bash
# Run unit tests for test class
abapgit-agent unit --files abap/zcl_my_test.clas.testclasses.abap

# Run unit tests for package
abapgit-agent unit --package $MY_PACKAGE
```

### Tree Command

Display package hierarchy.

```bash
# Display package tree
abapgit-agent tree --package $MY_PACKAGE

# With object counts
abapgit-agent tree --package $MY_PACKAGE --include-objects

# JSON output
abapgit-agent tree --package $MY_PACKAGE --json
```

### View Command

View ABAP object definitions directly from the system.

```bash
# View table structure
abapgit-agent view --objects ZMY_TABLE --type TABL

# View class definition
abapgit-agent view --objects ZCL_MY_CLASS

# View CDS view
abapgit-agent view --objects ZC_MY_CDS_VIEW --type DDLS

# JSON output
abapgit-agent view --objects ZCL_MY_CLASS --json
```

### Preview Command

Preview data from tables or CDS views.

```bash
# Preview table data
abapgit-agent preview --objects SFLIGHT

# Preview with row limit
abapgit-agent preview --objects SFLIGHT --limit 5

# Preview with WHERE filter
abapgit-agent preview --objects SFLIGHT --where "CARRID = 'AA'"

# Preview specific columns
abapgit-agent preview --objects SFLIGHT --columns CARRID,PRICE,FLDATE

# Vertical format (for wide tables)
abapgit-agent preview --objects SFLIGHT --vertical

# Compact mode (truncated values)
abapgit-agent preview --objects SFLIGHT --compact

# JSON output
abapgit-agent preview --objects SFLIGHT --json
```

### Reference Search Command

Search ABAP cheat sheets for syntax patterns and topics. Works offline without ABAP connection.

```bash
# Search for a pattern in cheat sheets
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "CX_SY_"
abapgit-agent ref "FILTER #"

# View specific topic
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql
abapgit-agent ref --topic unit-tests

# List all available topics
abapgit-agent ref --list-topics

# JSON output for scripting
abapgit-agent ref "VALUE #(" --json
```

**Available Topics:**
- `internal-tables` - Internal table operations
- `sql` - ABAP SQL syntax
- `oop` - Object-oriented ABAP
- `constructors` - Constructor expressions (VALUE, FILTER, etc.)
- `exceptions` - Exception handling (TRY-CATCH, classical)
- `unit-tests` - ABAP Unit testing
- `cds` - CDS View Entities
- `json-xml` - JSON/XML processing
- `performance` - Performance optimization
- `patterns` - Design patterns
- `dynamic` - Dynamic programming (RTTI)
- And more...

**Configuration:**
The command auto-detects cheat sheets from:
1. `referenceFolder` in `.abapGitAgent`
2. `~/abap-reference/abap-cheat-sheets`
3. `~/Documents/abap-reference/abap-cheat-sheets`

## JSON Handling - ALWAYS Use /ui2/cl_json

**CRITICAL**: Always use `/ui2/cl_json` for JSON serialization and deserialization.

**Correct:**
```abap
" Deserialize JSON to ABAP structure
DATA ls_data TYPE ty_request.
ls_data = /ui2/cl_json=>deserialize( json = lv_json ).

" Serialize ABAP structure to JSON
lv_json = /ui2/cl_json=>serialize( data = ls_response ).
```

**Never use**:
- Manual string operations (CONCATENATE, SPLIT, etc.)
- String templates for complex structures
- Direct assignment without /ui2/cl_json

This is enforced by ABAP - manual string operations for JSON parsing will cause type conflicts.

## ABAP Class Definition - Must Use PUBLIC

**CRITICAL**: Global ABAP classes MUST use `PUBLIC` in the class definition:

```abap
" Correct - global class
CLASS zcl_my_class DEFINITION PUBLIC.
  ...
ENDCLASS.

" Wrong - treated as local class, will fail activation
CLASS zcl_my_class DEFINITION.
  ...
ENDCLASS.
```

**Error symptom**: `Error updating where-used list for CLAS ZCL_MY_CLASS`

**Fix**: Add `PUBLIC` keyword:
```abap
CLASS zcl_my_class DEFINITION PUBLIC.  " <- PUBLIC required
```

## Exception Handling - Classical vs Class-Based

ABAP has two exception handling mechanisms. Using the wrong one causes silent failures.

### Quick Identification

| Method Signature | Exception Type | Handling Pattern |
|-----------------|----------------|------------------|
| `EXCEPTIONS exc = 1` | Classical | `sy-subrc` check |
| `RAISING cx_...` | Class-based | `TRY-CATCH` |

### Classical Exceptions (Old Style)

Methods declared with `EXCEPTIONS` in signature:

```abap
" Method declaration
class-methods describe_by_name
  importing p_name type any
  returning value(p_descr_ref) type ref to cl_abap_typedescr
  exceptions
    type_not_found.  " <- Classical!

" Calling with classical exception
cl_abap_structdescr=>describe_by_name(
  EXPORTING
    p_name = iv_tabname
  RECEIVING
    p_descr_ref = lo_descr
  EXCEPTIONS
    type_not_found = 1
    OTHERS = 2 ).
IF sy-subrc <> 0.
  " Handle error
ENDIF.
```

**Common classical exception methods:**
- `cl_abap_structdescr=>describe_by_name` - `type_not_found`
- `cl_abap_tabledescr=>describe_by_name` - `type_not_found`
- Many RTTI (Run Time Type Information) methods

### Class-Based Exceptions (Modern)

Methods declared with `RAISING`:

```abap
" Method declaration
methods get_ref
  importing p_name type string
  returning value(p_ref) type ref to cl_ci_checkvariant
  raising cx_ci_checkvariant.  " <- Class-based!

" Calling with TRY-CATCH
TRY.
    lo_variant = cl_ci_checkvariant=>get_ref( p_name = lv_variant ).
  CATCH cx_ci_checkvariant INTO DATA(lx_error).
    " Handle error
ENDTRY.
```

### The Mistake to Avoid

**WRONG**: Using TRY-CATCH for classical exceptions
```abap
" This does NOT catch type_not_found!
TRY.
    lo_descr = cl_abap_structdescr=>describe_by_name( iv_name ).
  CATCH cx_root.  " Never reached!
ENDTRY.
```

**CORRECT**: Use proper pattern for each type
```abap
" Classical - use sy-subrc
cl_abap_structdescr=>describe_by_name(
  EXPORTING p_name = iv_name
  RECEIVING p_descr_ref = lo_descr
  EXCEPTIONS type_not_found = 1 ).
IF sy-subrc <> 0.
  " Handle error
ENDIF.

" Class-based - use TRY-CATCH
TRY.
    lo_obj = cl_ci_checkvariant=>get_ref( lv_name ).
  CATCH cx_root INTO DATA(lx_error).
    " Handle error
ENDTRY.
```

### How to Check Which to Use

Use the `view` command to check method signature:

```bash
abapgit-agent view --objects CL_ABAP_STRUCTDESCR
```

Look for:
- `exceptions TYPE_NOT_FOUND` → Classical (use `sy-subrc`)
- `raising CX_SY_RTTI` → Class-based (use `TRY-CATCH`)

Or search the cheat sheets:

```bash
abapgit-agent ref --topic exceptions
```

## Interface Method Implementation

When implementing interface methods in ABAP classes, use the interface prefix:

```abap
" Interface definition
INTERFACE zif_my_interface PUBLIC.
  METHODS do_something IMPORTING iv_param TYPE string.
ENDINTERFACE.

" Class implementation - use interface prefix
CLASS zcl_my_class DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_my_interface.
ENDCLASS.

CLASS zcl_my_class IMPLEMENTATION.
  METHOD zif_my_interface~do_something.  " <- Use interface prefix
    " Implementation here
  ENDMETHOD.
ENDCLASS.
```

**Wrong**: `METHOD do_something.` - parameter `iv_param` will be unknown
**Correct**: `METHOD zif_my_interface~do_something.` - parameters recognized

## Creating New ABAP Objects - XML Metadata Required

**CRITICAL CHECKLIST - Never Forget!**

When creating ANY new ABAP object file, you MUST also create its XML metadata file:

| ABAP File | Required XML File |
|-----------|------------------|
| `zcl_my_class.clas.abap` | `zcl_my_class.clas.xml` |
| `zif_my_intf.intf.abap` | `zif_my_intf.intf.xml` |

**Without XML files**, abapGit will NOT recognize the objects during pull, and they won't be activated.

### XML Metadata Format for CLAS

For `zcl_abgagt_util.clas.abap`, create `zcl_abgagt_util.clas.xml`:

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_ABGAGT_UTIL</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>
```

### XML Metadata Format for INTF

For `zif_abgagt_util.intf.abap`, create `zif_abgagt_util.intf.xml`:

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_INTF" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOINTERF>
    <CLSNAME>ZIF_ABGAGT_UTIL</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOINTERF>
  </asx:values>
 </asx:abap>
</abapGit>
```

### Important Notes

1. **CRITICAL: Push to git BEFORE pulling into ABAP**
   - Always commit and push ABAP files to git first
   - Then run `abapgit-agent pull` to activate in ABAP
   - Never run `abapgit-agent pull` without pushing changes first

2. **Only pull ABAP files** - The XML metadata stays in git:
   ```bash
   abapgit-agent pull --files zcl_my_class.clas.abap
   ```
2. abapGit reads the XML from git to deserialize the ABAP code
3. XML files are NOT activated in ABAP - they are only for abapGit

```bash
# After making changes to ABAP files
git add .
git commit -m "Describe changes"
git push    # CRITICAL: Push BEFORE pulling

# Then validate in ABAP system (single file - fast)
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# Or validate all files
abapgit-agent pull
```

## Handling Persistent Syntax Errors

When fixing ABAP syntax errors using the commit-pull-commit loop:

1. **First 2-3 attempts**: Analyze the error message and try to fix based on the error details
2. **If errors persist after 2-3 iterations**:
   - Stop repeating the same fix attempts
   - Search local ABAP reference materials:
     ```bash
     # Search cheat sheets for error pattern
     abapgit-agent ref "error pattern"
     abapgit-agent ref --topic exceptions  # For exception-related errors
     ```
   - Search the web for the specific ABAP error message or syntax issue
   - Use keywords like "ABAP", the error code, and relevant context
   - Examples: "ABAP error 'XXXX' CLAS", "ABAP syntax MESSAGE is not a declaration"
3. **After finding information**:
   - Apply the correct fix based on documentation
   - Test again with `abapgit-agent pull`

**Never guess** - ABAP syntax is strict. If you're unsure, search first using the local reference materials or web search.

## Understanding abapgit-agent Output

**Critical: Never ignore these messages!**

| Message | Meaning | Action Required |
|---------|---------|-----------------|
| `Error updating where-used list` | **SYNTAX ERROR** - object has errors | Fix the syntax error |
| `Failed Objects (N) > 0` | Objects failed to activate | Fix syntax errors |
| `Success: X` with all objects checked | All good | Proceed |

**Example error flow:**
```
❌ Pull completed with errors!
⚠️  Failed Objects (1):
   ✗ CLAS ZCL_MY_CLASS: Error updating where-used list

Action: The class has syntax errors. Use inspect to get details.
```

## Check Local Implementation First

When implementing new features or fixing issues:

1. **Always check local implementations first** - This project already contains working examples of:
   - REST handlers (e.g., `zcl_abgagt_resource_pull`, `zcl_abgagt_resource_inspect`)
   - JSON serialization using `/ui2/cl_json`
   - ABAP object activation patterns
   - Error handling approaches

2. **Before inventing something new:**
   - Search the `abap/` folder for similar implementations
   - Look at existing patterns in the codebase
   - Reuse existing helper classes and methods
   - Follow the established code style

3. **Use ABAP Reference Materials (Portable):**
   ```bash
   # Use the built-in ref command (works from any directory)
   abapgit-agent ref "CORRESPONDING"
   abapgit-agent ref --topic exceptions
   abapgit-agent ref --topic sql
   abapgit-agent ref --list-topics
   ```

   The command auto-detects the reference folder from `.abapGitAgent` or common locations (`~/abap-reference`).

4. **Examples:**
   - Need to create a new REST endpoint? → Study `zcl_abgagt_resource_pull.clas.abap`
   - Need to serialize JSON? → Use `/ui2/cl_json` as shown in existing handlers
   - Need to query TADIR? → Check how other classes do it
   - Unfamiliar with exception types? → Search `27_Exceptions.md` in cheat sheets

**Don't guess patterns** - The codebase has proven implementations. Reuse them.

**Common mistakes to avoid:**
- Using wrong method parameter names (e.g., `set_string_data( iv_data = x )` should be `set_string_data( x )`)
- Forgetting to use `/ui2/cl_json` for JSON operations
- Using inline DATA declarations incorrectly

## Local Code Reference (Offline Support)

When network issues prevent accessing online resources, maintain a local folder with ABAP code examples for reference.

**Setup:**

1. Configure the reference folder path in `.abapGitAgent`:
   ```json
   {
     "referenceFolder": "<path-to-reference-folder>"
   }
   ```

2. Clone ABAP repositories for reference into this folder:
   ```bash
   mkdir -p ~/abap-reference
   cd ~/abap-reference

   # abapGit itself - best reference for ABAP patterns
   git clone https://github.com/abapGit/abapGit.git

   # ABAP coding style guides (Clean ABAP, code review)
   git clone https://github.com/SAP/styleguides.git

   # ABAP cheat sheets with code snippets for various topics
   git clone https://github.com/SAP-samples/abap-cheat-sheets.git
   ```

**Portable Reference Lookup:**

Use the built-in `ref` command (works from any directory without ABAP connection):

```bash
# Search for a pattern in cheat sheets
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "CX_SY_ZERODIVIDE"

# Search by topic
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql
abapgit-agent ref --topic unit-tests

# List all available topics
abapgit-agent ref --list-topics

# JSON output for scripting
abapgit-agent ref "CORRESPONDING" --json
```

**Configuration:** The command auto-detects the reference folder from `.abapGitAgent` or common locations (`~/abap-reference`, `~/Documents/abap-reference`).

**Legacy bash script (if needed):**
If you prefer the bash script, it's still available at `./bin/abap-ref`.

**Manual Search:**

```bash
# Detect reference folder from .abapGitAgent config
REF_FOLDER=$(cat .abapGitAgent 2>/dev/null | grep -o '"referenceFolder"[^}]*' | cut -d'"' -f4)

# Fallback to common locations
[ -z "$REF_FOLDER" ] && for path in "$HOME/abap-reference" "$HOME/Documents/abap-reference"; do
  [ -d "$path/abap-cheat-sheets" ] && REF_FOLDER="$path" && break
done

# Search cheat sheets
grep -ri "pattern" "$REF_FOLDER/abap-cheat-sheets/" --include="*.md" -l | head -10
```

**Common Topics:**

| Topic | Cheat Sheet File |
|-------|------------------|
| Internal Tables | `01_Internal_Tables.md` |
| ABAP SQL | `03_ABAP_SQL.md` |
| Constructor Expressions | `05_Constructor_Expressions.md` |
| Exceptions | `27_Exceptions.md` |
| Unit Tests | `14_ABAP_Unit_Tests.md` |
| JSON/XML | `21_XML_JSON.md` |
| Performance | `32_Performance_Notes.md` |

**Usage for AI:**

- When you encounter unfamiliar ABAP syntax, use `./bin/abap-ref` to find relevant documentation
- Read the specific cheat sheet file for the topic
- Use `abapgit-agent view` to check object definitions in the ABAP system
- Never guess ABAP syntax - always verify against references

## ABAP Object Types

Common object types in this project:
- `CLAS` - Classes
- `PROG` - Programs
- `FUGR` - Function Groups
- `INTF` - Interfaces
- `TABL` - Tables
- `DDLS` - Data Definitions

## Creating CDS Views (DDLS)

CDS views (Data Definition Language Source) require specific file naming and structure for abapGit.

### File Naming

CDS views require **two files**:

| File | Description |
|------|-------------|
| `zc_my_view.ddls.asddls` | DDL source code |
| `zc_my_view.ddls.xml` | XML metadata |

**Important:** Do NOT use `.ddls.abap` extension - use `.ddls.asddls` for the source.

### DDL Source File (`.ddls.asddls`)

```abap
@AbapCatalog.sqlViewName: 'ZCMYVIEW'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'My CDS View'
define view ZC_My_View as select from tdevc
{
  key devclass as Devclass,
      parentcl as ParentPackage,
      ctext    as Description
}
where devclass not like '$%'
```

### XML Metadata File (`.ddls.xml`)

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DDLS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DDLS>
    <DDLNAME>ZC_MY_VIEW</DDLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>My CDS View</DDTEXT>
   </DDLS>
  </asx:values>
 </asx:abap>
</abapGit>
```

### Key Points

1. **Avoid reserved words** - Field names like `PACKAGE`, `CLASS`, `INTERFACE` are reserved in CDS. Use alternatives like `PackageName`, `ClassName`.

2. **Pull all files to activate** - When activating CDS views, use `abapgit-agent pull` (not single file) to ensure proper activation:
   ```bash
   abapgit-agent pull  # Pull all files
   ```

3. **System support** - CDS views require SAP systems with CDS capability (S/4HANA, SAP BW/4HANA, or ABAP 7.51+). Older systems will show error: "Object type DDLS is not supported by this system"

### Debugging Activation Errors

When pull shows generic errors like "Activation cancelled. Check the inactive objects":

1. **Check in ADT/Eclipse** - Open the DDL source in ADT and run syntax check for detailed errors
2. **Pull all files** - Sometimes `abapgit-agent pull` (all files) works better than single file for CDS views

## Creating CDS View Entities

CDS View Entities (`define view entity`) are the modern replacement for CDS Views with additional features like **associations for OData navigation**.

### Differences from CDS Views

| Aspect | CDS View | View Entity |
|--------|----------|-------------|
| Syntax | `define view` | `define view entity` |
| Associations | No | Yes (exposed for navigation) |
| OData/Navigation | Requires separate service | Auto-exposes associations |
| ABAP Version | 7.40+ | 7.55+ / S/4HANA Cloud |

### DDL Source File with Association

```abap
@EndUserText.label: 'Package Hierarchy'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZC_Pkg_Hierarchy_VE
  as select from tdevc
  association [0..1] to tdevc as _Parent
    on _Parent.devclass = $projection.ParentPackage
{
  key devclass                           as PackageName,
      parentcl                           as ParentPackage,
      ctext                              as Description,
      dlvunit                           as SoftwareComponent,

      // Exposed associations
      _Parent
}
where devclass not like '$%'
```

### Key Points for View Entities

1. **Association syntax**: Use `$projection` to reference fields in the current entity
2. **Association cardinality**: `[0..1]`, `[1..1]`, `[0..n]`, `[1..n]`
3. **Expose associations**: Add the association name at the end of the SELECT to expose it for OData navigation
4. **Activation warnings**: Search help warnings are informational and don't block activation

## Naming Conventions

- Use `Z_` or `Y_` prefix for custom objects
- Class names: `ZCL_<NAME>`
- Interface names: `ZIF_<NAME>`
- Programs: `Z<NAME>`
- Package: `$<PROJECT_NAME>`

## Command Factory Pattern

### Command Class Naming
Command implementations should follow the naming convention:
- `ZCL_ABGAGT_COMMAND_<COMMAND>` where `<COMMAND>` is PULL, INSPECT, UNIT, etc.

Example:
```abap
CLASS zcl_abgagt_command_pull DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_command.
ENDCLASS.
```

### Command Interface
Commands implement `ZIF_ABGAGT_COMMAND` with constants:

```abap
INTERFACE zif_abgagt_command PUBLIC.
  CONSTANTS:
    gc_pull TYPE string VALUE 'PULL',
    gc_inspect TYPE string VALUE 'INSPECT',
    gc_unit TYPE string VALUE 'UNIT'.

  METHODS get_name RETURNING VALUE(rv_name) TYPE string.
  METHODS execute IMPORTING it_files TYPE string_table
                 RETURNING VALUE(rv_result) TYPE string.
ENDINTERFACE.
```

### Factory with Dynamic Object Creation
Use dynamic object creation to avoid syntax errors in one command affecting others:

```abap
CLASS zcl_abgagt_cmd_factory DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_cmd_factory.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_factory) TYPE REF TO zif_abgagt_cmd_factory.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_command_map,
             command TYPE string,
             class_name TYPE string,
           END OF ty_command_map.
    DATA mt_command_map TYPE TABLE OF ty_command_map.
ENDCLASS.

CLASS zcl_abgagt_cmd_factory IMPLEMENTATION.
  METHOD constructor.
    mt_command_map = VALUE #(
      ( command = zif_abgagt_command=>gc_pull     class_name = 'ZCL_ABGAGT_COMMAND_PULL' )
      ( command = zif_abgagt_command=>gc_inspect class_name = 'ZCL_ABGAGT_COMMAND_INSPECT' )
      ( command = zif_abgagt_command=>gc_unit   class_name = 'ZCL_ABGAGT_COMMAND_UNIT' )
    ).
  ENDMETHOD.

  METHOD zif_abgagt_cmd_factory~get_command.
    READ TABLE mt_command_map WITH KEY command = iv_command
      ASSIGNING FIELD-SYMBOL(<ls_map>).
    IF sy-subrc = 0.
      CREATE OBJECT ro_command TYPE (<ls_map>-class_name).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

**Benefits:**
- Factory class activates independently of command classes
- Syntax errors in one command don't affect others
- Add new commands by updating the mapping table

## Exception Handling in ABAP

ABAP has two different ways to handle exceptions. Understanding the difference is critical.

### Classical ABAP Exceptions (EXCEPTIONS)

Used in older function modules and some OO classes. Defined in method signature using `EXCEPTIONS` keyword.

**Method Definition:**
```abap
METHODS method_name
  IMPORTING iv_param TYPE string
  EXCEPTIONS
    exc1 = 1
    exc2 = 2
    OTHERS = 3.
```

**Method Call:**
```abap
method_name(
  EXPORTING iv_param = lv_value
  EXCEPTIONS
    exc1 = 1
    exc2 = 2
    OTHERS = 3 ).

IF sy-subrc <> 0.
  " Handle error - check sy-subrc for exception number
ENDIF.
```

**Characteristics:**
- Return code in `sy-subrc`
- No exception objects
- Legacy approach

### Modern ABAP Exceptions (TRY-CATCH)

Used in modern OO ABAP (7.40+). Uses exception classes and RAISING clause.

**Exception Class:**
```abap
CLASS cx_my_exception DEFINITION INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    METHODS constructor IMPORTING textid LIKE textid OPTIONAL.
ENDCLASS.
```

**Method Definition:**
```abap
METHODS method_name
  IMPORTING iv_param TYPE string
  RAISING cx_my_exception.
```

**Method Call:**
```abap
TRY.
    method_name( iv_param = lv_value ).
  CATCH cx_my_exception INTO lx_error.
    lv_message = lx_error->get_text( ).
ENDTRY.
```

### How to Identify Which to Use

Look at the method signature:

| Syntax | Type |
|--------|------|
| `EXCEPTIONS exc1 = 1` | Classical |
| `RAISING cx_exception` | Modern (TRY-CATCH) |

### Real Example: cl_ci_checkvariant=>get_ref

This method uses **classical exceptions**:

```abap
" WRONG - using TRY-CATCH
TRY.
    lo_variant = cl_ci_checkvariant=>get_ref(
      p_user = ''
      p_name = lv_variant ).
  CATCH cx_root.
    " This won't work!
ENDTRY.

" CORRECT - using EXCEPTIONS with EXPORTING and RECEIVING
cl_ci_checkvariant=>get_ref(
  EXPORTING
    p_user = ''
    p_name = lv_variant
  RECEIVING
    p_ref = lo_variant
  EXCEPTIONS
    chkv_not_exists = 1
    missing_parameter = 2
    broken_variant = 3
    OTHERS = 4 ).

IF sy-subrc <> 0.
  " Handle error
ENDIF.
```

**Key Points:**
1. Use `EXPORTING` before importing parameters when using `EXCEPTIONS`
2. Use `RECEIVING` for RETURNING parameters (not direct assignment)
3. Check `sy-subrc` for exception codes
4. Not all OO methods use TRY-CATCH - some still use classical exceptions

## Best Practices

### Always Return Interface Types, Not Class Types

When defining factory methods or creating objects, return the interface type, not the class type:

```abap
" WRONG - returns concrete class
CLASS-METHODS get_instance
  RETURNING VALUE(ro_factory) TYPE REF TO zcl_abgagt_cmd_factory.

" CORRECT - returns interface type
CLASS-METHODS get_instance
  RETURNING VALUE(ro_factory) TYPE REF TO zif_abgagt_cmd_factory.
```

**Why?** Interface types provide better abstraction, easier testing, and follow the dependency inversion principle. Callers depend on the interface (abstraction), not the concrete class.

## Unit Testing with Local Test Classes

### File Structure

For ABAP local unit tests, use a **separate file** with `.testclasses.abap` extension:

```
abap/
  zcl_my_class.clas.abap          <- Main class (no test code)
  zcl_my_class.clas.testclasses.abap  <- Local test class
  zcl_my_class.clas.xml           <- XML with WITH_UNIT_TESTS = X
```

### Required Elements

1. **Test class file** (`zcl_my_class.clas.testclasses.abap`):
   ```abap
   *"* use this source file for your test class implementation
   *"* local test class
   CLASS ltcl_zcl_my_class DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
     PRIVATE SECTION.
       DATA mo_cut TYPE REF TO zcl_my_class.
       METHODS setup.
       METHODS test_method1 FOR TESTING.
       METHODS test_method2 FOR TESTING.
   ENDCLASS.

   CLASS ltcl_zcl_my_class IMPLEMENTATION.
     METHOD setup.
       CREATE OBJECT mo_cut.
     ENDMETHOD.
     METHOD test_method1.
       " Test code using cl_abap_unit_assert
     ENDMETHOD.
   ENDCLASS.
   ```

2. **XML metadata** (`zcl_my_class.clas.xml`):
   ```xml
   <VSEOCLASS>
     ...
     <WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>
   </VSEOCLASS>
   ```

### Naming Conventions

- Test class name: `LTCL_ZCL_<CLASSNAME>` (e.g., `LTCL_ZCL_COMMAND_PULL`)
- Test methods: `TEST_<methodname> FOR TESTING` or simply `test_method FOR TESTING`
- Test file: `<classname>.clas.testclasses.abap`

### CRITICAL: Method Name Length Limit

**Test method names MUST NOT exceed 30 characters!**

```abap
" WRONG - 34 characters (syntax error)
METHODS test_execute_with_minimal_params FOR TESTING.

" CORRECT - 18 characters
METHODS test_exec_minimal FOR TESTING.
```

Examples of compliant names:
- `test_get_name` (13 chars)
- `test_exec_minimal` (18 chars)
- `test_exec_files` (16 chars)
- `test_interface` (15 chars)

### Common Assertions

```abap
cl_abap_unit_assert=>assert_equals( act = lv_actual exp = lv_expected msg = 'Error message' ).
cl_abap_unit_assert=>assert_not_initial( act = lv_data msg = 'Should not be initial' ).
cl_abap_unit_assert=>assert_bound( act = lo_ref msg = 'Should be bound' ).
cl_abap_unit_assert=>assert_true( act = lv_bool msg = 'Should be true' ).
```

### What NOT To Do

- ❌ Don't add test methods directly in the main `.clas.abap` file
- ❌ Don't use `CLASS ... DEFINITION ...` without the special comment header
- ❌ Don't reference `<TESTCLASS>` in XML - abapGit auto-detects `.testclasses.abap`
- ❌ Don't use nested local classes inside the main class definition

### Running Tests

In ABAP: SE24 → Test → Execute Unit Tests

Or via abapGit: Pull the files and run tests in the ABAP system.
