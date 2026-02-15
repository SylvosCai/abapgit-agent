# ABAP Project Guidelines - Template

This file provides guidelines for **generating ABAP code** in abapGit repositories.

**Use this file as a template**: Copy it to your ABAP repository root when setting up new projects with Claude Code.

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
   - Search the web for the specific ABAP error message or syntax issue
   - Use keywords like "ABAP", the error code, and relevant context
   - Examples: "ABAP error 'XXXX' CLAS", "ABAP syntax MESSAGE is not a declaration"
3. **After finding information**:
   - Apply the correct fix based on documentation
   - Test again with `abapgit-agent pull`

**Never guess** - ABAP syntax is strict. If you're unsure, search first.

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

3. **Examples:**
   - Need to create a new REST endpoint? → Study `zcl_abgagt_resource_pull.clas.abap`
   - Need to serialize JSON? → Use `/ui2/cl_json` as shown in existing handlers
   - Need to query TADIR? → Check how other classes do it

**Don't guess patterns** - The codebase has proven implementations. Reuse them.

**Common mistakes to avoid:**
- Using wrong method parameter names (e.g., `set_string_data( iv_data = x )` should be `set_string_data( x )`)
- Forgetting to use `/ui2/cl_json` for JSON operations
- Using inline DATA declarations incorrectly

## Local Code Reference (Offline Support)

When network issues prevent accessing online resources, you can maintain a local folder with ABAP code examples for reference.

**Setup:**

1. Configure the reference folder path in `.abapGitAgent`:
   ```json
   {
     "referenceFolder": "<path-to-reference-folder>"
   }
   ```

2. Clone ABAP repositories for reference into this folder:
   ```bash
   # abapGit itself - best reference for ABAP patterns
   git clone https://github.com/abapGit/abapGit.git

   # ABAP coding style guides (Clean ABAP, code review)
   git clone https://github.com/SAP/styleguides.git

   # ABAP cheat sheets with code snippets for various topics
   git clone https://github.com/SAP/abap-cheat-sheets.git
   ```

**Usage:**

- When Claude needs to reference ABAP patterns, read files from the folder configured in `.abapGitAgent` (`referenceFolder`)
- Useful for offline development or when network is unreliable
- Keep commonly used ABAP utilities, class patterns, and examples

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
